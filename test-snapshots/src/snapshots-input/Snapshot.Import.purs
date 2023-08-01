module Snapshot.Import where

import Snapshot.Import.Impl as I
import Snapshot.Import.Product
import Snapshot.Import.Constructor

fortyThree :: Int
fortyThree = I.addImpl 21 22

fst :: Product -> Int
fst (Product x _) = x

foo :: Foo
foo = Foo
