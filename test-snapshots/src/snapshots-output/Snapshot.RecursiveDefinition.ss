#!r6rs
#!chezscheme
(library
  (Snapshot.RecursiveDefinition lib)
  (export
    alpha
    bravo
    charlie
    delta
    main)
  (import
    (prefix (chezscheme) scm:)
    (prefix (purs runtime) rt:)
    (prefix (Data.Unit lib) Data.Unit.))

  (scm:define main
    (scm:lambda ()
      Data.Unit.unit))

  (scm:define delta
    (scm:lambda (x0)
      (scm:lambda (y1)
        (scm:cond
          [(scm:fx=? x0 y1) (($lazy-bravo) 0)]
          [scm:else 1.0]))))

  (scm:define alpha
    (scm:lambda (v0)
      (scm:cond
        [(scm:fx=? v0 0) ($lazy-bravo)]
        [(scm:fx=? v0 1) ($lazy-charlie)]
        [(scm:fx=? v0 2) (scm:lambda (y1)
          (scm:cond
            [(scm:fx>? y1 0) (($lazy-bravo) y1)]
            [scm:else (($lazy-charlie) y1)]))]
        [scm:else (scm:lambda (y1)
          (scm:cond
            [(scm:fx=? y1 v0) (($lazy-bravo) 0)]
            [scm:else 1.0]))])))

  (rt:define-lazy $lazy-charlie "charlie" "Snapshot.RecursiveDefinition"
    (alpha 4))

  (rt:define-lazy $lazy-bravo "bravo" "Snapshot.RecursiveDefinition"
    (alpha 3))

  (scm:define charlie
    ($lazy-charlie))

  (scm:define bravo
    ($lazy-bravo)))
