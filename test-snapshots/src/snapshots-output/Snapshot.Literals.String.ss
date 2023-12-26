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
    (prefix (purs runtime) rt:))

  (scm:define unicode7
    (rt:string->bytestring "Foo \b\b\b\b Bar"))

  (scm:define unicode6
    (rt:string->bytestring "2342✓"))

  (scm:define unicode5
    (rt:string->bytestring "Foo ✓ Bar ✓ \n Baz ✓"))

  (scm:define unicode4
    (rt:string->bytestring "✓"))

  (scm:define unicode3
    (rt:string->bytestring "✓"))

  (scm:define unicode2
    (rt:string->bytestring "􏿿"))

  (scm:define unicode1
    (rt:string->bytestring "\x0000;"))

  (scm:define escape8
    (rt:string->bytestring "'"))

  (scm:define escape7
    (rt:string->bytestring "\""))

  (scm:define escape6
    (rt:string->bytestring "\\"))

  (scm:define escape5
    (rt:string->bytestring "\r\n"))

  (scm:define escape4
    (rt:string->bytestring "\r"))

  (scm:define escape3
    (rt:string->bytestring "\r"))

  (scm:define escape2
    (rt:string->bytestring "\n"))

  (scm:define escape1
    (rt:string->bytestring "\t"))

  (scm:define block5
    (rt:string->bytestring "foo\nbar\nbaz\n"))

  (scm:define block4
    (rt:string->bytestring "foo\nbar\nbaz"))

  (scm:define block3
    (rt:string->bytestring "foo\nbar"))

  (scm:define block2
    (rt:string->bytestring "\nfoo\nbar\n"))

  (scm:define block1
    (rt:string->bytestring "block")))
