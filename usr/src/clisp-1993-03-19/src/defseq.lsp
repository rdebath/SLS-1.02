;; Definitionen der Standard-Sequence-Typen
;; (in Verbindung mit SEQUENCE.Q)
;; Bruno Haible 9.7.1989, 1.8.1989, 2.8.1989

(in-package "SYSTEM")

(%defseq
  (vector
    'LIST
    #'identity
    #'list-upd
    #'list-endtest
    #'list-fe-init
    #'list-upd
    #'list-endtest
    #'list-access
    #'list-access-set
    #'identity
    #'list-llength
    #'make-list
    #'list-elt
    #'list-set-elt
    #'list-init-start
    #'list-fe-init-end
) )

(%defseq ; VECTOR steht für GENERAL-VECTOR
  (vector
    'VECTOR
    #'vector-init
    #'vector-upd
    #'vector-endtest
    #'vector-fe-init
    #'vector-fe-upd
    #'vector-fe-endtest
    #'aref
    #'sys::store
    #'identity
    #'vector-length
    #'make-array
    #'aref
    #'sys::store
    #'vector-init-start
    #'vector-fe-init-end
) )

(%defseq
  (vector
    'STRING
    #'vector-init
    #'vector-upd
    #'vector-endtest
    #'vector-fe-init
    #'vector-fe-upd
    #'vector-fe-endtest
    #'char
    #'sys::store
    #'identity
    #'vector-length
    #'make-string
    #'char
    #'sys::store
    #'vector-init-start
    #'vector-fe-init-end
) )

(%defseq
  (vector
    'BIT-VECTOR
    #'vector-init
    #'vector-upd
    #'vector-endtest
    #'vector-fe-init
    #'vector-fe-upd
    #'vector-fe-endtest
    #'bit
    #'sys::store
    #'identity
    #'vector-length
    #'make-bit-vector
    #'bit
    #'sys::store
    #'vector-init-start
    #'vector-fe-init-end
) )

#-CLISP1
(mapc
  #'(lambda (n &aux (eltype (list 'UNSIGNED-BYTE n)))
      (%defseq
        (vector
          n ; n steht für `(VECTOR (UNSIGNED-BYTE ,n))
          #'vector-init
          #'vector-upd
          #'vector-endtest
          #'vector-fe-init
          #'vector-fe-upd
          #'vector-fe-endtest
          #'aref
          #'sys::store
          #'identity
          #'vector-length
          #'(lambda (length) (make-array length :element-type eltype))
          #'aref
          #'sys::store
          #'vector-init-start
          #'vector-fe-init-end
    ) ) )
  '(1 2 4 8 16 32)
)

