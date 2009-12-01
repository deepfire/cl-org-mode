(in-package :cl-org-mode)

(defclass node ()
  ((next-node :initarg :next-node 
	      :accessor node.next-node 
	      :initform nil))
  (:documentation "Base class for all nodes"))

(defvar *dispatchers* nil
  "A dynamic variable to hold a list of nodes to use as  dispatchers.
The default method for FIND-NEXT-NODE will call NODE-START on these nodes")

(defgeneric node-dispatchers (node)
  (:documentation "Called by the reader in order to set the dynamic dispatch environment before reading the next node"))

(defgeneric find-next-node (node next-node stack)
  (:documentation "Find the next node that starts in STREAM, implicitly ending NODE.

All methods must return the multiple values (VALUES NEW-NODE OLD-STACK) as if from NODE-START. "))

(defgeneric read-next-node (node next-node stream)
  (:documentation "read the next node in stream and return it.

This is the main entry point for specializing node types."))

(defgeneric read-node (starting-node stream)
  (:documentation "return the next node after reading it from the stream 

The default method simply calls READ-NEXT-NODE.")
  (:method (starting-node stream)
    (read-next-node starting-node (node.next-node starting-node) stream)))

(defgeneric node-start (node stack)
  (:documentation 
  "Indicate a new node should begin at this point in the stack. 

The parser will pass a class prototype instance via NODE, so it
shouldn't be mutated.

All methods _must_ return (VALUES NEW-NODE OLD-STACK) where NEW-NODE
created NODE object and any remaining stack which likely belongs to
the previous node."))

(defgeneric node-end (node next-node stack)
  (:documentation 
  "return true if stack of characters indicate this node has finished reading"))


(defgeneric finalize-node (node next-node stack)
  (:documentation "Called when the node has finished reading.
 
This is usually either because node-end returned true or implicitly
because another node has started"))