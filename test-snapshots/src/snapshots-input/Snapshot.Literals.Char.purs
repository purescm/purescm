module Snapshot.Literals.Char where

null = '\x0000' :: Char
alarm = '\x0007' :: Char
backspace = '\x0008' :: Char
tab = '\x0009' :: Char
newline = '\x000A' :: Char
vtab = '\x000B' :: Char
page = '\x000C' :: Char
return = '\x00D' :: Char
escape = '\x001B' :: Char
space = ' ' :: Char
delete = '\x007F' :: Char
ls = '\x2028' :: Char

firstPrintableChar = '!' :: Char

singleQuote = '\'' :: Char
doubleQuote = '\"' :: Char

lastPrintableChar = '~' :: Char -- \x007E

latin1Supplement = '±' :: Char -- \x00B1

latinExtendedA = 'ā' :: Char -- \x0101

latinExtendedB = 'ƀ' :: Char -- \x0180

forallChar = '∀' :: Char -- \x2200

closeToTop = '︙' :: Char -- \xFE19'

biggestChar = '\xFFFF' :: Char
