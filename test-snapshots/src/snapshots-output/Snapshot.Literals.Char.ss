#!r6rs
#!chezscheme
(library
  (Snapshot.Literals.Char lib)
  (export
    alarm
    backspace
    biggestChar
    closeToTop
    delete
    doubleQuote
    escape
    firstPrintableChar
    forallChar
    lastPrintableChar
    latin1Supplement
    latinExtendedA
    latinExtendedB
    ls
    newline
    null
    page
    return
    singleQuote
    space
    tab
    vtab)
  (import
    (prefix (chezscheme) scm:)
    (prefix (_Chez_Runtime lib) rt:))

  (scm:define vtab
    #\vtab)

  (scm:define tab
    #\tab)

  (scm:define space
    #\space)

  (scm:define singleQuote
    #\')

  (scm:define return
    #\return)

  (scm:define page
    #\page)

  (scm:define null
    #\nul)

  (scm:define newline
    #\linefeed)

  (scm:define ls
    #\ls)

  (scm:define latinExtendedB
    #\x0180)

  (scm:define latinExtendedA
    #\x0101)

  (scm:define latin1Supplement
    #\x00b1)

  (scm:define lastPrintableChar
    #\~)

  (scm:define forallChar
    #\x2200)

  (scm:define firstPrintableChar
    #\!)

  (scm:define escape
    #\esc)

  (scm:define doubleQuote
    #\")

  (scm:define delete
    #\delete)

  (scm:define closeToTop
    #\xfe19)

  (scm:define biggestChar
    #\xffff)

  (scm:define backspace
    #\backspace)

  (scm:define alarm
    #\alarm))
