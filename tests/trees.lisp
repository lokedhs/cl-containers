(in-package #:cl-containers-test)

(deftestsuite test-trees (cl-containers-test)
  ())

(addtest (test-trees)
  insert-delete-consistency
  (ensure-cases (class)
      '(binary-search-tree
	red-black-tree)
    (let ((c (make-instance class)))
      (insert-item c 31)
      (ensure-same (size c) 1 :test '=)
      (delete-item c 31)
      (ensure-same (size c) 0 :test '=)
      )))

(addtest (test-trees)
  searching-and-deleting-keyed
  (ensure-cases (class)
      '(binary-search-tree
	red-black-tree)
    (let ((b (make-container class
			     :key #'first
			     :test #'equal)))
      (insert-list b '((2) (3) (10) (1) (4)))
      (ensure-same (size b) 5)
      (ensure-same (first (first-item b)) 1)
      (ensure-same (search-for-node b 1 :key #'first) (containers::first-node b))
      (delete-item b '(2))
      (ensure-same (size b) 4)
      (insert-item b '(7))
      (insert-item b '(-2))
      (insert-item b '(12))
      (ensure-same (size b) 7)
      (empty! b)
      (ensure-same (size b) 0)
      )))

(addtest (test-trees)
  find-on-nonexistant-item
  (ensure-cases (class)
      '(binary-search-tree
	red-black-tree)
    (let ((c (make-instance class)))
      (ensure-null (item-at c 1))
      (ensure-null (find-element c 1))
      (ensure-null (find-item c 1))
      (ensure-null (find-node c 1))
      )))

(addtest (test-trees)
  find-on-nonexistant-item-nonempty

  (ensure-cases (class)
      '(binary-search-tree
	red-black-tree)
    (let ((c (make-instance class)))
      (insert-list c '(64 83 68 84 97))
      (ensure-null (item-at c 1))
      (ensure-null (find-element c 1))
      (ensure-null (find-item c 1))
      (ensure-null (find-node c 1))
      )))

(addtest (test-trees)
  randomized-testing
  (ensure-cases (class)
      '(binary-search-tree
	red-black-tree)
    (let* ((count 20)
	   (randlist (loop repeat count
			   collect (random 100)))
	   (c (make-container class )))
      (loop for n in randlist
	    do (insert-item c n))
      (ensure-same (size c) count)
      (ensure-same (first-element c) (apply #'min randlist))
      (ensure-same (last-element c) (apply #'max randlist))
      (loop for n in randlist
	    do (ensure (item-at c n))
	    do (ensure (find-item c n)))
      (ensure-same (collect-elements c)
		   (sort (copy-list randlist) #'<)
		   :test #'equal)


      ;;now remove half the elements and make sure it still makes sense.
      (loop repeat (/ count 2)
	    for n = (pop randlist)
	    do (ensure (typep (delete-element c n)
			      'containers::bst-node)))

      (ensure-same (size c) (length randlist))
      (ensure-same (first-element c) (apply #'min randlist))
      (ensure-same (last-element c) (apply #'max randlist))
      (loop for n in randlist
	    do (ensure (item-at c n))
	    do (ensure (find-item c n)))
      (ensure-same (collect-elements c)
		   (sort (copy-list randlist) #'<)
		   :test #'equal)

      ;;remove the rest
      (ensure-null (empty! c))
      (ensure (empty-p c))
      (ensure-same (size c) 0)
      (ensure-null (collect-elements c)))))

(defparameter *rbtree-special-test-data*
  '((:INSERT (720255619831889/500000 . 1))
    (:INSERT (180063904958453/125000 . 101))
    (:INSERT (1440511239667639/1000000 . 102))
    (:INSERT (720255619833821/500000 . 103))
    (:INSERT (1440511239667643/1000000 . 104))
    (:INSERT (360127809916911/250000 . 105))
    (:INSERT (720255619833823/500000 . 106))
    (:INSERT (1440511239667647/1000000 . 107))
    (:INSERT (22507988119807/15625 . 108))
    (:INSERT (1440511239667649/1000000 . 109))
    (:REMOVE (720255619831889/500000 . 1))
    (:INSERT (720255619833833/500000 . 110))
    (:REMOVE (180063904958453/125000 . 101))
    (:INSERT (180063904958459/125000 . 111))
    (:REMOVE (1440511239667639/1000000 . 102))
    (:INSERT (720255619833837/500000 . 112))
    (:REMOVE (720255619833821/500000 . 103))
    (:INSERT (360127809916919/250000 . 113))
    (:REMOVE (1440511239667643/1000000 . 104))
    (:INSERT (1440511239667677/1000000 . 114))
    (:REMOVE (360127809916911/250000 . 105))
    (:INSERT (1440511239667679/1000000 . 115))
    (:REMOVE (720255619833823/500000 . 106))
    (:INSERT (9003195247923/6250 . 116))
    (:REMOVE (1440511239667647/1000000 . 107))
    (:INSERT (720255619833841/500000 . 117))
    (:REMOVE (22507988119807/15625 . 108))
    (:INSERT (1440511239667683/1000000 . 118))
    (:REMOVE (1440511239667649/1000000 . 109))
    (:INSERT (288102247933537/200000 . 119))
    (:INSERT (90031952478987/62500 . 2))
    (:INSERT (720255619835067/500000 . 120))
    (:INSERT (720255619835069/500000 . 121))
    (:INSERT (720255619835071/500000 . 122))
    (:INSERT (22507988119846/15625 . 123))
    (:INSERT (288102247934029/200000 . 124))
    (:INSERT (720255619835073/500000 . 125))
    (:INSERT (1440511239670147/1000000 . 126))
    (:INSERT (360127809917537/250000 . 127))
    (:INSERT (28810224793403/20000 . 128))
    (:REMOVE (90031952478987/62500 . 2))
    (:INSERT (180063904958769/125000 . 129))
    (:REMOVE (720255619835067/500000 . 120))
    (:INSERT (1440511239670153/1000000 . 130))
    (:REMOVE (720255619835069/500000 . 121))
    (:INSERT (288102247934031/200000 . 131))
    (:REMOVE (720255619835071/500000 . 122))))

(defun value< (v1 v2)
  (cond ((= (car v1) (car v2))
         (< (cdr v1) (cdr v2)))
        (t
         (< (car v1) (car v2)))))

(defun value= (v1 v2)
  (and (= (car v1) (car v2))
       (= (cdr v1) (cdr v2))))

(addtest (test-trees)
  test-rbtrees
  (let ((q (make-instance 'cl-containers:red-black-tree
                          :key #'identity
                          :sorter #'value<
                          :test #'value=)))
    (loop
       with expected-size = 0
       for i from 1
       for (cmd value) in *rbtree-special-test-data*
       do (ensure (= expected-size (cl-containers:size q)))
       do (ecase cmd
            (:insert (progn
                       (cl-containers:insert-item q value)
                       (incf expected-size)))
            (:remove (let ((result (cl-containers:delete-item q value)))
                       (ensure (value= value (cl-containers:element result)))
                       (decf expected-size))))
       finally (ensure (= expected-size (cl-containers:size q))))))

#|
(setf c (make-instance 'red-black-tree))
(insert-item c 1)
(delete-item c 2)
(size c)
c
(collect-nodes c)
(collect-elements c)
(delete-item c 1)
(trace delete-item)
(trace delete-node)
(trace)

(delete-element c 1)
(find-element c 1)

(compute-applicable-methods #'size (list c))
(compute-applicable-methods #'collect-nodes (list c))
(compute-applicable-methods #'iterate-nodes (list c #'identity))
(compute-applicable-methods #'delete-item (list c 1))
|#
