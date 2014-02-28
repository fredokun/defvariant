defvariant
==========

Variants for Common Lisp (for ML nostalgics ?)

```lisp
(defpackage :try-defvariant
  (:use :cl :defvariant))

(in-package :try-defvariant)

(defvariant btree
   (leaf)
   (node val left right))
```

`=> MATCH-BTREE`

```lisp
(let ((my-tree
        (make-btree-node :val 42 
                         :left (make-btree-leaf) 
                         :right (make-btree-leaf))))
  (match-btree my-tree
        (leaf _ "leaf !")
        (node (v l r) v)))
```

`=> 42`

----
See. `defvariant.md` for the whole story.
