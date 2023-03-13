module PureScript.Backend.Chez.Syntax where

import Prelude

import Data.Argonaut as Json
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Dodo as Dodo
import Prim as Prim
import PureScript.Backend.Optimizer.CoreFn (Ident(..), ModuleName(..), Prop(..))
import PureScript.Backend.Optimizer.Syntax (Level(..))
import Safe.Coerce (coerce)

newtype LiteralDigit = LiteralDigit Prim.String

derive instance Newtype LiteralDigit _
derive newtype instance Eq LiteralDigit
derive newtype instance Ord LiteralDigit

data ChezExpr
  = Integer LiteralDigit
  | Float LiteralDigit
  | String Prim.String
  | Boolean Prim.Boolean
  | Identifier Prim.String
  | List (Prim.Array ChezExpr)

printChezExpr :: forall a. ChezExpr -> Dodo.Doc a
printChezExpr e = case e of
  Integer (LiteralDigit x) -> Dodo.text x
  Float (LiteralDigit x) -> Dodo.text x
  String x -> Dodo.text x
  Boolean x -> Dodo.text $ if x then "#t" else "#f"
  Identifier x -> Dodo.text x
  List xs -> Dodo.text "(" <> Dodo.words (printChezExpr <$> xs) <> Dodo.text ")"

toChezIdent :: Maybe Ident -> Level -> Ident
toChezIdent i (Level l) = Ident $ case i of
  Just (Ident i') -> i' <> show l
  Nothing -> "_" <> show l

--

chezCurriedApplication :: ChezExpr -> NonEmptyArray ChezExpr -> ChezExpr
chezCurriedApplication f s = NonEmptyArray.foldl1 app $ NonEmptyArray.cons f s

chezCurriedFunction :: NonEmptyArray Ident -> ChezExpr -> ChezExpr
chezCurriedFunction a e = lambda (coerce $ NonEmptyArray.toArray a) e

chezLet :: Ident -> ChezExpr -> ChezExpr -> ChezExpr
chezLet (Ident i) v e = List [ Identifier "scm:letrec*", List [ List [ Identifier i, v ] ], e ]

--

app :: ChezExpr -> ChezExpr -> ChezExpr
app f x = List [ f, x ]

define :: Ident -> ChezExpr -> ChezExpr
define i e = List [ Identifier "scm:define", Identifier $ coerce i, e ]

library :: ModuleName -> Prim.Array Ident -> Prim.Array ChezExpr -> ChezExpr
library moduleName exports bindings =
  List $
    [ Identifier "library"
    , Identifier $ "(" <> coerce moduleName <> " lib" <> ")"
    , List $
        [ Identifier "export"
        ] <> (Identifier <$> coerce exports)
    , List
        [ Identifier "import"
        , List
            [ Identifier "prefix"
            , List
                [ Identifier "chezscheme"
                ]
            , Identifier "scm:"
            ]
        ]
    ] <> bindings

record :: Prim.Array (Prop ChezExpr) -> ChezExpr
record r =
  let
    field :: Prop ChezExpr -> ChezExpr
    field (Prop k v) =
      List
        [ Identifier "scm:hashtable-set!"
        , Identifier "$record"
        , String $ Json.stringify $ Json.fromString k
        , v
        ]
  in
    List $
      [ Identifier "scm:letrec*"
      , List
          [ List
              [ Identifier "$record"
              , List
                  [ Identifier "scm:make-hashtable"
                  , Identifier "scm:string-hash"
                  , Identifier "scm:string=?"
                  ]
              ]
          ]
      ] <> (field <$> r) <> [ Identifier "$record" ]

lambda :: Prim.Array Prim.String -> ChezExpr -> ChezExpr
lambda a e = List [ Identifier "scm:lambda", List $ Identifier <$> a, e ]

vector :: Prim.Array ChezExpr -> ChezExpr
vector = List <<< Array.cons (Identifier "scm:vector")
