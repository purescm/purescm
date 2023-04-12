module PureScript.Backend.Chez.Syntax.Common where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Newtype (wrap)
import Data.Tuple (Tuple(..))
import PureScript.Backend.Chez.Constants (scmPrefixed)
import PureScript.Backend.Chez.Syntax (ChezExpr(..))
import PureScript.Backend.Optimizer.CoreFn (Prop(..))

app :: ChezExpr -> ChezExpr -> ChezExpr
app f x = List [ f, x ]

runUncurriedFn :: ChezExpr -> Array ChezExpr -> ChezExpr
runUncurriedFn f s = List $ Array.cons f s

mkUncurriedFn :: Array String -> ChezExpr -> ChezExpr
mkUncurriedFn a e = Lambda a e

runCurriedFn :: ChezExpr -> NonEmptyArray ChezExpr -> ChezExpr
runCurriedFn f s = NonEmptyArray.foldl1 app $ NonEmptyArray.cons f s

mkCurriedFn :: NonEmptyArray String -> ChezExpr -> ChezExpr
mkCurriedFn a e = Array.foldr (Lambda <<< Array.singleton) e $ NonEmptyArray.toArray a

thunk :: ChezExpr -> ChezExpr
thunk e = Lambda [] e

unthunk :: ChezExpr -> ChezExpr
unthunk e = List [ e ]

makeStringHashtable :: ChezExpr
makeStringHashtable = runUncurriedFn (Identifier $ scmPrefixed "make-hashtable")
  [ Identifier $ scmPrefixed "string-hash"
  , Identifier $ scmPrefixed "string=?"
  ]

hashtableSet :: ChezExpr -> Prop ChezExpr -> ChezExpr
hashtableSet r (Prop k v) = runUncurriedFn (Identifier $ scmPrefixed "hashtable-set!")
  [ r, StringExpr $ wrap k, v ]

record :: Array (Prop ChezExpr) -> ChezExpr
record p = do
  let
    recordName :: String
    recordName = "$record"

    recordIdent :: ChezExpr
    recordIdent = Identifier recordName
  Let false (NonEmptyArray.singleton (Tuple recordName makeStringHashtable)) $
    if Array.null p then
      recordIdent
    else
      List $ Array.snoc (hashtableSet recordIdent <$> p) recordIdent

quote :: ChezExpr -> ChezExpr
quote e = app (Identifier $ scmPrefixed "quote") e

eqQ :: ChezExpr -> ChezExpr -> ChezExpr
eqQ x y = runUncurriedFn (Identifier $ scmPrefixed "eq?") [ x, y ]

vector :: Array ChezExpr -> ChezExpr
vector = List <<< Array.cons (Identifier $ scmPrefixed "vector")

recordTypeName :: String -> String
recordTypeName i = i <> "$"

recordTypeCurriedConstructor :: String -> String
recordTypeCurriedConstructor i = i

recordTypeUncurriedConstructor :: String -> String
recordTypeUncurriedConstructor i = i <> "*"

recordTypePredicate :: String -> String
recordTypePredicate i = i <> "?"

recordTypeAccessor :: String -> String -> String
recordTypeAccessor i field = i <> "-" <> field

recordAccessor :: ChezExpr -> String -> String -> ChezExpr
recordAccessor expr name field =
  runUncurriedFn (Identifier $ recordTypeAccessor name field) [ expr ]
