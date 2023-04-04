module Snapshot.Literals.String where

block1 :: String
block1 = """block"""

block2 :: String
block2 = """
foo
bar
"""

block3 :: String
block3 = """foo
bar"""

block4 :: String
block4 = """foo
bar
baz"""

block5 :: String
block5 = """foo
bar
baz
"""

---

escape1 :: String
escape1 = "\t"

escape2 :: String
escape2 = "\n"

escape3 :: String
escape3 = "\r"

escape4 :: String
escape4 = "\r"

escape5 :: String
escape5 = "\r\n"

escape6 :: String
escape6 = "\\"

escape7 :: String
escape7 = "\""

escape8 :: String
escape8 = "\'"

---

-- Lowest valid code point
unicode1 :: String
unicode1 = "\x0"

unicode2 :: String
unicode2 = "\x10ffff"

-- Emoji
unicode3 :: String
unicode3 = "\x2713"

-- Unicode literal with leading zero. Should be equal to unicode3.
unicode4 :: String
unicode4 = "\x02713"

-- Multiple unicode literals in a single string
unicode5 :: String
unicode5 = "Foo \x2713 Bar \x2713 \n Baz \x2713"

unicode6 :: String
unicode6 = "2342\x2713"

-- String with ^H
unicode7 :: String
unicode7 = "Foo \x8\x8\x8\x8 Bar"
