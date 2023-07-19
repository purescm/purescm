module Snapshot.Literals.Number where

import Prelude

nan :: Number
nan = 0.0 / 0.0

plusInfinity :: Number
plusInfinity = 1.0 / 0.0

minusInfinity :: Number
minusInfinity = -1.0 / 0.0
