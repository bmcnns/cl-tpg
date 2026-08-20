(defpackage :bes
	    (:use :cl)
	    (:import-from :lparallel
			  #:*kernel*
			  #:make-kernel
			  #:pmap
			  #:end-kernel)
	    (:export :start-server :stop-server :execute-team :run-search
		     #:*num-threads*
		     #:*population-size*
		     #:*num-observations*
		     #:*num-actions*
		     #:*init-num-learners*
		     #:*max-num-learners*
		     #:*p-add*
		     #:*p-del*
		     #:*p-mut*
		     #:*p-act*
		     #:*p-swap*
		     #:*gap*
		     #:*init-program-size*
		     #:*max-program-size*
		     #:*p-add-instr*
		     #:*p-del-instr*
		     #:*p-swap-instrs*
		     #:*p-mut-constant*
		     #:*p-mut-constant-sign*
		     #:*output-file*
		     #:*trial*
		     #:best-policy
		     #:across-many-trials))

(defpackage :bes-gym
  (:use :cl :bes)
  (:shadow #:step)
  (:export #:rollout)
  (:documentation "A Gymnasium wrapper for BES"))

(in-package :bes)
       
