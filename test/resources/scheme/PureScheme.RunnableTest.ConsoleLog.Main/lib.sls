(library
  (PureScheme.RunnableTest.ConsoleLog.Main lib)
  (export main)
  (import (prefix (rnrs) scm:) (prefix (Effect.Console lib) Effect.Console.))


  (scm:define main (Effect.Console.log "foo"))
  )