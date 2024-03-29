module Language.PureScript.Scheme.CodeGen.Transpiler where

import Data.Text (Text)

import Language.PureScript.Names
       (Ident(..), Qualified(..),
        disqualify, runModuleName, runProperName, showQualified)
import Language.PureScript.PSString (PSString)
import Language.PureScript.CoreFn.Module (Module(..))
import Language.PureScript.CoreFn.Ann (Ann)
import Language.PureScript.CoreFn.Meta (Meta(..))
import Language.PureScript.CoreFn.Expr
       (Expr(..), Bind(..), CaseAlternative(..))
import Language.PureScript.CoreFn.Binders (Binder(..))
import Language.PureScript.AST.Literals (Literal(..))

import Language.PureScript.Scheme.Util (concatMapWithIndex)
import Language.PureScript.Scheme.CodeGen.SExpr (SExpr(..), everywhere)
import Language.PureScript.Scheme.CodeGen.Scheme
       (t, stringEqQ', stringHash',
        define, quote, lambda1, letRecS, letRecS1, cond, begin,
        eq2, eqQ, charEqQ2, stringEqQ2, and_, cons, cons, car, cdr,
        vector, vector, vectorLength, vectorRef,
        makeHashtable, hashtableSetB, hashtableRef, hashtableCopy,
        error_)
import Language.PureScript.Scheme.CodeGen.Case
       (Alternative(..), GuardedExpr, toAlternatives)

import Language.PureScript.Scheme.CodeGen.Library (Library(..))

import qualified Data.Text as Text


moduleToLibrary :: Module Ann -> Library
moduleToLibrary (Module _sourceSpan _comments moduleName _path
                        imports exports _reExports foreigns
                        declarations)
  = Library
    { libraryName = runModuleName moduleName
    , libraryExports = map identToScheme exports
    , libraryImports = map (runModuleName . snd) imports
    , libraryForeigns = map identToScheme foreigns
    , libraryBody = libraryBody
    }
  where
    libraryBody = concatMap topBindToScheme declarations

    ----------------------------------------------------------------------------

    topBindToScheme :: Bind Ann -> [SExpr]

    topBindToScheme bind
      = case bind of
          NonRec _ann ident expr
            -> [ def ident expr ]
          Rec xs
            -> map (\((_ann, ident), expr) -> def ident expr)
                   xs
      where
        def :: Ident -> Expr Ann -> SExpr
        def ident expr = define (identToScheme ident) (exprToScheme expr)

    ----------------------------------------------------------------------------

    exprToScheme :: Expr Ann -> SExpr

    exprToScheme (Literal _ann literal)
      = literalToScheme literal
    exprToScheme (Constructor _ann _typeName constructorName fields)
      = constructorToScheme constructorName fields
    exprToScheme (Accessor _ann property object)
      = accessorToScheme object property
    exprToScheme (ObjectUpdate _ann object keysAndValues)
      = objectUpdateToScheme object keysAndValues
    exprToScheme (Abs _ann arg expr)
      = lambda1 (identToScheme arg) (exprToScheme expr)
    exprToScheme (App _ann function arg)
      = appToScheme function arg
    exprToScheme (Var _ann qualifiedIdent)
      = varToScheme qualifiedIdent
    exprToScheme (Case _ann values caseAlternatives)
      = caseToScheme values caseAlternatives
    exprToScheme (Let _ann binds expr)
      = letToScheme binds expr

    ----------------------------------------------------------------------------

    literalToScheme :: Literal (Expr Ann) -> SExpr

    literalToScheme (NumericLiteral (Left  i)) = Integer i
    literalToScheme (NumericLiteral (Right f)) = Float f
    literalToScheme (StringLiteral  x)         = String x
    literalToScheme (CharLiteral    x)         = Character x
    literalToScheme (BooleanLiteral x)         = Boolean x
    literalToScheme (ArrayLiteral   xs)        = vector $ map exprToScheme xs
    literalToScheme (ObjectLiteral  xs)        = objectToScheme xs

    ----------------------------------------------------------------------------

    objectToScheme :: [(PSString, Expr Ann)] -> SExpr
    objectToScheme xs =
      letRecS1 ("$ht", makeStringHashtable (toInteger $ length xs))
               (begin ((fmap go xs) ++ [Symbol "$ht"]))
      where
        go :: (PSString, Expr Ann) -> SExpr
        go (key, value) = hashtableSetB (Symbol "$ht")
                                        (psStringToSExpr key)
                                        (exprToScheme value)

    ----------------------------------------------------------------------------

    -- An algebraic data type can have one or more constructors.
    --
    -- E.g.:
    --   data Foo1 = Foo1
    --   data Foo2 = Foo2 | Bar2
    --   data Foo3 = Foo3 | Bar3 | Baz3
    --
    -- A constructor for an algebraic data type can take zero or more values
    -- whose type is provided to the constructor.
    --
    -- E.g.:
    --   data Foo1 = Foo1 Int String
    --   data Foo2 = Foo2 Int | Bar2 String
    --
    -- The constructed data structure is translated into a pair.
    --
    --   - The car of the pair is the tag. It is used to determine which
    --     constructor produced a given data structure. The tag is a quoted
    --     symbol of the Constructor name.
    --     TODO: maybe don't use a pair if the ADT has a single constructor?
    --   - The cdr of the pair is a vector, holding the values of the
    --     constructed data structure. If the constructor doesn't take any value
    --     the vector will be empty.
    --     TODO: maybe use something else instead of an empty vector?
    --
    -- If the constructor takes one or more values it is translated into a
    -- function taking such values and returning the constructed data structure.
    --
    -- E.g.:
    --   The following code
    --     data Foo = Bar Int
    --   Is translated into
    --     (define Foo (lambda (v0) (cons 'Bar v0)))
    --
    -- If the constructor takes no values it is translated into a constructed
    -- data structure.
    --
    -- E.g.:
    --   The following code
    --     data Foo = Bar
    --   Is translated into
    --     (define Foo (cons 'Bar (vector)))
    --
    -- Keep in mind that data types usually have multiple constructors.
    --
    -- E.g.:
    --   The following code
    --     data Foo = Bar Int | Baz String | Qux
    --   Is translated into
    --     (define Bar (lambda (v0) (cons 'Bar (vector v0))))
    --     (define Baz (lambda (v0) (cons 'Baz (vector v0))))
    --     (define Qux (cons 'Qux (vector)))
    --
    -- Keep in mind that in PureScript functions with more than one arguments
    -- actually are multiple nested curried single argument functions. The same
    -- holds for data constructors: a product type with N fields results into a
    -- curried function of N arguments.
    --
    -- E.g.:
    --   The following code
    --     data Foo = Bar Int Int
    --   Is translated into
    --     (define Bar (lambda (v0) (lambda (v1) (cons 'Bar (vector v0 v1)))))
    constructorToScheme constructorName fields =
      go fields
      where
        -- Here we build the nested curried lambda expressions.
        go (x:xs) = lambda1 (identToScheme x) (go xs)
        -- Here we build the body if the most inner lambda expresison that
        -- returns the constructed data structure.
        -- If the data type doesn't take values then fields is an empty
        -- and only this branch is executed.
        go [] = cons (quote (Symbol (runProperName constructorName)))
                     (vector (map (\field -> Symbol (identToScheme field))
                                  fields))

    ----------------------------------------------------------------------------

    accessorToScheme :: (Expr Ann) -> PSString -> SExpr
    accessorToScheme object property
      = accessorToScheme' (exprToScheme object) property

    ----------------------------------------------------------------------------

    objectUpdateToScheme :: Expr Ann -> [(PSString, Expr Ann)] -> SExpr
    objectUpdateToScheme object keysAndValues
      = letRecS1 ("$ht", hashtableCopy (exprToScheme object))
                 (begin ((fmap go keysAndValues) ++ [Symbol "$ht"]))
      where
        go :: (PSString, Expr Ann) -> SExpr
        go (key, value) = hashtableSetB (Symbol "$ht")
                                        (psStringToSExpr key)
                                        (exprToScheme value)

    ----------------------------------------------------------------------------

    appToScheme :: Expr Ann -> Expr Ann -> SExpr
    appToScheme function arg
      = case function of
          -- When Var is newtype we have to drop the application and
          -- just return the applied value.
          -- The newtype definition is alway (lambda (x) x).
          Var (_sourceSpan, _comments, _maybeSourceType, Just IsNewtype)
              _qualifiedIdent
            -> exprToScheme arg
          _ -> List [exprToScheme function, exprToScheme arg]

    ----------------------------------------------------------------------------

    varToScheme :: Qualified Ident -> SExpr
    varToScheme qualifiedIdent@(Qualified maybeModuleName ident) =
      case maybeModuleName of
        Nothing -> Symbol (identToScheme ident)
        Just moduleName'
          | moduleName == moduleName' -> Symbol (identToScheme ident)
          | (runModuleName moduleName') == "Prim"
            && (identToScheme ident) == "undefined"
            -> (error_ $ String "undefined")
          | otherwise -> Symbol (showQualified identToScheme qualifiedIdent)

    ----------------------------------------------------------------------------

    caseToScheme :: [Expr Ann] -> [CaseAlternative Ann] -> SExpr
    caseToScheme values caseAlternatives =
      cond clauses
      where
        clauses = concatMap condClause alternatives
        alternatives = toAlternatives values caseAlternatives

        -- Emit a list of pairs (condition, result) for each case alternative to be used
        -- by cond. Every case alternative can produce multiple of them because there
        -- might be multiple guards for each of them.
        condClause :: Alternative -> [(SExpr, SExpr)]

        -- If the result is a guarded expression then we concatenate the guard
        -- conditions to the conditions for the alternative.
        condClause (Alternative boundValues (Left guardedExprs))
          = map guardedExprToClause guardedExprs
          where
            guardedExprToClause :: GuardedExpr -> (SExpr, SExpr)
            guardedExprToClause (guard, result)
              = ( test boundValues $ Just $
                  replaceVariables
                    (exprToScheme guard)
                    (variablesToReplace boundValues)
                , replaceVariables
                    (exprToScheme result)
                    (variablesToReplace boundValues)
                )

        -- If the result is a simple expression then the result is just the
        -- expression.
        condClause (Alternative boundValues (Right result))
          = [(test boundValues Nothing, bindersToResult boundValues result)]

        -- Emit the test for a cond clause.
        -- In scheme: (cond ((and test1a test1b) result1) ...)
        -- 
        -- Always wrap the test in an `and' form since binderToTest has to return
        -- a list because when applied to a ConstructorBinder it returns a list
        -- of SExpr (while the other cases return a list of SExpr with a single
        -- element only).
        --
        -- We could have produced a single SExpr node instead of a list of SExpr
        -- in binderToTest (so that for a ConstructorBinder it would have emitted
        -- an `and' form and for all other cases a simple SExpr node) but we
        -- would have ended up with nested `and's: one coming from `test' and
        -- one coming from `binderToTest' when it handles a ConstructorBinder.
        --
        -- This means that we could end up with and `and' form with a single node
        -- (e.g. (and foo)) but this is very simple to optimize compared to
        -- nested `and' forms that the alternative would have caused.
        --
        -- Keep in mind that this means that we could end up in the case of a
        -- ConstructorBinder with something like:
        --     (cond ((and (eq? (car v) 'Foo) cond1)  result1)
        --           ((and (eq? (car v) 'Foo) cond2)) result2)
        -- TODO: Maybe we should avoid the repeating of the first check.
        --
        -- The additional guard parameter is so that we can include the guard
        -- conditions when handling a guarded expression.
        test :: [(Expr Ann, Binder Ann)] -> Maybe SExpr -> SExpr
        test valueAndBinders maybeGuard = and_ $
          concatMap (\(value, binder) -> binderToTest (exprToScheme value) binder) valueAndBinders
          <> case maybeGuard of
              Nothing -> []
              Just g -> [g]

        -- Emit a cond clause test for each value and binder.
        -- E.g.: (value: 23, binder: 42) -> (= 23 42)
        binderToTest :: SExpr -> Binder Ann -> [SExpr]
        binderToTest _value (NullBinder _ann) = [t]
        binderToTest value (LiteralBinder _ann (NumericLiteral (Left i)))
          = [eq2 value (Integer i)]
        binderToTest value (LiteralBinder _ann (NumericLiteral (Right f)))
          = [eq2 value (Float f)]
        binderToTest value (LiteralBinder _ann (StringLiteral x))
          = [stringEqQ2 value (String x)]
        binderToTest value (LiteralBinder _ann (CharLiteral x))
          = [charEqQ2 value (Character x)]
        binderToTest value (LiteralBinder _ann (BooleanLiteral x))
          = [eqQ value (Boolean x)]

        binderToTest value (LiteralBinder _ann (ArrayLiteral binders))
          = (:) (eq2 (vectorLength value)
                     (Integer (toInteger $ length binders)))
                (concatMapWithIndex
                  (\i b -> (binderToTest (vectorRef value (Integer i))
                                         b))
                  binders)

        binderToTest value (LiteralBinder _ann (ObjectLiteral binders))
          = (concatMap
              (\(key, binder) -> (binderToTest (accessorToScheme' value key)
                                               binder))
              binders)

        binderToTest _value (VarBinder _ann _ident) = [t]

        -- When the constructor is a newtype we're not provided with the usual
        -- structure we're using for constructors (pair . [vals]). Instead we
        -- are handed with just the inner type.
        -- There are guarantees that there will be just a single binder but this
        -- is unfortunately not encoded in the CoreFn data type. The official
        -- PureScript backend is doing exactly the same and who should we trust
        -- otherwise?
        binderToTest value (ConstructorBinder (_, _, _, Just IsNewtype)
                            _typeName _constructorName [binder])
          = binderToTest value binder

        -- For a ConstructorBinder we have to emit at least one test, but more likely
        -- multiple tests.
        --
        -- The first test has to check if, given the tagged pair
        -- ('ConstructorName . fields), its head containing the Constructor name
        -- matches the `value' we're currently analyzing in the code.
        -- For instance, in:
        --     match (Foo x) = x
        -- The first test translates into
        --     (eq? (car value) (quote Foo))
        -- Where value is the expression holding (Foo x)
        -- 
        -- Then we have to check the constructor fields, if any. The
        -- ConstructorBinder could contain multiple binders, each one binding an
        -- identifier to an element of the Constructor. We have to compare and
        -- access those through vector-ref.
        -- For instance, in:
        --     match (Foo 1) = 1
        -- The remaining tests translate into:
        --     (= (vector-ref (cdr v) 0) 1)
        binderToTest value (ConstructorBinder _ann _typeName
                                              constructorName binders) =
          (:) (eqQ (car value)
                   (quote (Symbol (runProperName (disqualify constructorName)))))
              (concatMapWithIndex
                (\i b -> (binderToTest (vectorRef (cdr value) (Integer i))
                                       b))
                binders)

        -- Consider the case:
        --     data Foo = Bar Int
        --     match bar@(Bar i) = bar
        -- It get translated to:
        --    (define Bar (lambda (x) ('Bar . x)))
        --    (define match (lambda (v) (cond (eq? (car v) 'Bar) v)))
        -- `bar' (in PureScript) is an alias to `v' (in Scheme). `value' is an
        -- `Expr.Var "v"', and `ident' is an `Ident.Ident "bar"'.
        -- So in order to generate the test for the cond in the case of a
        -- NamedBinder we have to process its binder (with binderToTest) using
        -- the actual variable name `value' and not with the alias `ìdent'.
        -- We will have to use ident when we have to deal with the result associated
        -- to this case though.
        binderToTest value (NamedBinder _ann _ident binder)
          = binderToTest value binder

        -- Emit a cond clause result for multiple values and binders associated to
        -- a single result. This is done by replacing the identifiers in the result
        -- expression according to its VarBinders.
        bindersToResult :: [(Expr Ann, Binder Ann)] -> Expr Ann -> SExpr
        bindersToResult valuesAndBinders result =
          replaceVariables (exprToScheme result)
                           (variablesToReplace valuesAndBinders)


        variablesToReplace :: [(Expr Ann, Binder Ann)] -> [(Text, SExpr)]
        variablesToReplace valuesAndBinders =
          concatMap (\(value, binder) -> go (exprToScheme value) binder)
                    valuesAndBinders
          where
            go _value (NullBinder _ann) = []

            go value (LiteralBinder _ann (ArrayLiteral binders))
              = concatMapWithIndex
                  (\i b -> (go (vectorRef value (Integer i)) b))
                  binders

            go value (LiteralBinder _ann (ObjectLiteral binders))
              = concatMap
                  (\(k, b) -> (go (accessorToScheme' value k) b))
                  binders
  
            go _value (LiteralBinder _ann _literal) = []

            -- Each identifier in a VarBinder will be replaced by its
            -- corresponding value.
            -- E.g. `foo m n = m + n' will emit a function whose parameters are
            -- `v' and `v0', but the `m + n' expression will use `m' and `n' as
            -- variable.
            -- We have to replace each occurrence of `m' with `v' and each
            -- occurrence of `n' with `v0'.
            go value (VarBinder _ann ident) = [(identToScheme ident, value)]

            -- When the constructor is a newtype we have to analyze just the
            -- binder. Check the comment for binderToTest about the same case
            -- for more information.
            go value (ConstructorBinder (_, _, _, Just IsNewtype)
                      _typeName _constructorName [binder])
              = go value binder

            -- For a ConstructorBinder we have to replace each identifier in its
            -- binders with an access to the right index of the vector in the cdr
            -- of the tagged pair holding the Constructor values.
            go value (ConstructorBinder _ann _typeName _constructorName binders)
              = concatMapWithIndex
                  (\i b -> (go (vectorRef (cdr value) (Integer i)) b))
                  binders

            -- Consider the case:
            --     data Foo = Bar Int
            --     match bar@(Bar i) = bar
            -- It get translated to:
            --    (define Bar (lambda (x) ('Bar . x)))
            --    (define match (lambda (v) (cond (eq? (car v) 'Bar) v)))
            -- `value' is an `Expr.Var "v"', and `ident' is an
            -- `Ident.Ident "bar"'.
            -- We have to do multiple replacements here. First, we have to
            -- replace each each `ident' with `value'. Then, we have to apply
            -- other possible replacements for the binder associated to the
            -- NamedBinder.
            go value (NamedBinder _ann ident binder) =
              (:) (identToScheme ident, value)
                  (go value binder)

    ----------------------------------------------------------------------------

    letToScheme :: [Bind Ann] -> Expr Ann -> SExpr
    letToScheme binds body
      = letRecS (concatMap bindToScheme binds) (exprToScheme body)
      where
        bindToScheme :: Bind Ann -> [(Text, SExpr)]
        bindToScheme bind
          = case bind of
              NonRec _ann ident expr -> [ binding ident expr ]
              Rec xs -> map (\((_ann, ident), expr) -> binding ident expr) xs

        binding :: Ident -> Expr Ann -> (Text, SExpr)
        binding ident expr = (identToScheme ident, exprToScheme expr)

    ----------------------------------------------------------------------------

-- Replace each (Symbol from) into a `to' SExpr everywhere in ast
replaceVariables :: SExpr -> [(Text, SExpr)] -> SExpr
replaceVariables ast mapping = everywhere (go mapping) ast
  where
    go ((from, to):xs) (Symbol i) =
      if i == from then to else go xs (Symbol i)
    go [] ast' = ast'
    go _xs ast' = ast'

psStringToSExpr :: PSString -> SExpr
psStringToSExpr x = String x

makeStringHashtable :: Integer -> SExpr
makeStringHashtable size = makeHashtable stringHash' stringEqQ' (Integer size)

accessorToScheme' :: SExpr -> PSString -> SExpr
accessorToScheme' object property
  = hashtableRef object
                 (psStringToSExpr property)
                 (String $ "Key not found: " <> property)

identToScheme :: Ident -> Text
identToScheme (Ident name) = Text.concatMap identCharToText name
identToScheme (GenIdent _ _) = error "GenIdent in identToScheme"
identToScheme UnusedIdent = "$__unused"

identCharToText :: Char -> Text
identCharToText '\'' = "$prime"
identCharToText c = Text.singleton c
