#!r6rs
#!chezscheme
(library
  (Snapshot.Literals.String lib)
  (export
    block1
    block2
    block3
    block4
    block5
    escape1
    escape2
    escape3
    escape4
    escape5
    escape6
    escape7
    escape8
    unicode1
    unicode2
    unicode3
    unicode4
    unicode5
    unicode6
    unicode7)
  (import
    (prefix (chezscheme) scm:)
    (prefix (purs runtime lib) rt:))

  (scm:define unicode7
    "Foo \b\b\b\b Bar")

  (scm:define unicode6
    "2342✓")

  (scm:define unicode5
    "Foo ✓ Bar ✓ \n Baz ✓")

  (scm:define unicode4
    "✓")

  (scm:define unicode3
    "✓")

  (scm:define unicode2
    "􏿿")

  (scm:define unicode1
    "\x0000;")

  (scm:define escape8
    "'")

  (scm:define escape7
    "\"")

  (scm:define escape6
    "\\")

  (scm:define escape5
    "\r\n")

  (scm:define escape4
    "\r")

  (scm:define escape3
    "\r")

  (scm:define escape2
    "\n")

  (scm:define escape1
    "\t")

  (scm:define block5
    "foo\nbar\nbaz\n")

  (scm:define block4
    "foo\nbar\nbaz")

  (scm:define block3
    "foo\nbar")

  (scm:define block2
    "\nfoo\nbar\n")

  (scm:define block1
    "block"))
