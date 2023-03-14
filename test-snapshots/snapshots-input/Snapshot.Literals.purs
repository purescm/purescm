module Snapshot.Literals where

boolean1 :: Boolean
boolean1 = true

boolean2 :: Boolean
boolean2 = false

int :: Int
int = 1

number :: Number
number = 2.0

char :: Char
char = 'a'

string :: String
string = "string"

array :: Array Int
array = []

array2 :: Array Int
array2 = [ 1, 2, 3 ]

record :: {}
record = {}

record2 :: { foo :: String }
record2 = { foo: "bar" }
