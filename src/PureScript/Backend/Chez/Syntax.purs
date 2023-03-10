module PureScript.Backend.Chez.Syntax where

import Prelude

import Data.Array as Array
import Data.Newtype (class Newtype)
import Dodo as Dodo
import Prim as Prim

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

app :: ChezExpr -> ChezExpr -> ChezExpr
app f x = List [ f, x ]

define :: Prim.String -> ChezExpr -> ChezExpr
define i e = List [ Identifier "scm:define", Identifier i, e ]

library :: Prim.String -> Prim.Array Prim.String -> Prim.Array ChezExpr -> ChezExpr
library moduleName exports bindings =
  List $
    [ Identifier "library"
    , Identifier $ "(" <> moduleName <> " lib" <> ")"
    , List $
        [ Identifier "export"
        ] <> (Identifier <$> exports)
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

record :: Prim.Array { k :: Prim.String, v :: ChezExpr } -> ChezExpr
record r =
  let
    field :: { k :: Prim.String, v :: ChezExpr } -> ChezExpr
    field { k, v } =
      List
        [ Identifier "scm:hashtable-set!"
        , Identifier "$record"
        , String k
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

vector :: Prim.Array ChezExpr -> ChezExpr
vector = List <<< Array.cons (Identifier "scm:vector")
