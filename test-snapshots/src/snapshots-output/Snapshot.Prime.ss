#!r6rs
#!chezscheme
(library
  (Snapshot.Prime lib)
  (export
    Ctor$p
    Ctor$p*
    Ctor$p-value0
    Ctor$p-value1
    Ctor$p?
    DCtor
    DCtor?
    F1
    F1?
    F2
    F2?
    NCtor
    NewtypeCtor$p
    classMember$p
    foo
    foo$p
    foo$p$p
    foo$poo
    ignore
    instanceName$p
    normal
    useClass
    useDataCtor
    useDataType
    useFooPrime1
    useFooPrime2
    useFooPrime3
    useInstance
    useMember
    useNewtypeCtor
    useNewtypeType
    useNormal)
  (import
    (prefix (chezscheme) scm:)
    (prefix (_Chez_Runtime lib) rt:))

  (scm:define NCtor
    (scm:lambda (x0)
      x0))

  (scm:define NewtypeCtor$p
    (scm:lambda (x0)
      x0))

  (scm:define F1
    (scm:quote F1))

  (scm:define F1?
    (scm:lambda (v)
      (scm:eq? (scm:quote F1) v)))

  (scm:define F2
    (scm:quote F2))

  (scm:define F2?
    (scm:lambda (v)
      (scm:eq? (scm:quote F2) v)))

  (scm:define DCtor
    (scm:quote DCtor))

  (scm:define DCtor?
    (scm:lambda (v)
      (scm:eq? (scm:quote DCtor) v)))

  (scm:define-record-type (Ctor$p$ Ctor$p* Ctor$p?)
    (scm:fields (scm:immutable value0 Ctor$p-value0) (scm:immutable value1 Ctor$p-value1)))

  (scm:define Ctor$p
    (scm:lambda (value0)
      (scm:lambda (value1)
        (Ctor$p* value0 value1))))

  (scm:define useNewtypeType
    (scm:lambda (i0)
      i0))

  (scm:define useNewtypeCtor
    (scm:lambda (i0)
      i0))

  (scm:define useDataType
    (scm:lambda (v0)
      DCtor))

  (scm:define useDataCtor
    (scm:lambda (s0)
      (Ctor$p* s0 4)))

  (scm:define normal
    (scm:lambda (dict0)
      accessor-get-prop))

  (scm:define useNormal
    (scm:lambda (dictNormal0)
      (scm:lambda (dictNormal11)
        (scm:lambda (a2)
          (scm:lambda (b3)
            (scm:string-append (accessor-get-prop a2) (accessor-get-prop b3)))))))

  (scm:define instanceName$p
    (scm:letrec* (($record (scm:make-hashtable scm:string-hash scm:string=?))) (scm:hashtable-set! $record "normal" (scm:lambda (v0) (scm:cond ((F1? v0) "F1") ((F2? v0) "F2") (scm:else (scm:raise (scm:condition (scm:make-error) (scm:make-message-condition "Failed pattern match"))))))) $record))

  (scm:define useInstance
    "F1F2")

  (scm:define ignore
    (scm:lambda (dict0)
      accessor-get-prop))

  (scm:define useClass
    (scm:lambda (dictClassName$p0)
      accessor-get-prop))

  (scm:define foo$poo
    "foo'oo")

  (scm:define useFooPrime3
    "foo'oo")

  (scm:define foo$p$p
    "foo'")

  (scm:define useFooPrime2
    "foo'")

  (scm:define foo$p
    "foo'")

  (scm:define useFooPrime1
    "foo'")

  (scm:define foo
    "foo")

  (scm:define classMember$p
    (scm:lambda (dict0)
      accessor-get-prop))

  (scm:define useMember
    (scm:lambda (dictClassMember0)
      accessor-get-prop)))
