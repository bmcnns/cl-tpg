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

		     #:*fitness-file*
		     #:*cdf-file*
		     #:*state-visitation-file*
		     #:*video-file*
		     
		     #:*trial*
		     #:best-policy
		     #:across-many-trials
		     #:*stochastic*
		     #:entropy
		     #:*entropy-regularized*
		     #:*beta*
		     #:*monte-carlo-rollouts*

		     #:init-success-accumulator
		     #:write-success-curve
		     #:*max-depth*))

(defpackage :bes-gym
  (:use :cl :bes)
  (:shadow #:step)
  (:export #:rollout #:entropy-regularized-rollout)
  (:documentation "A Gymnasium wrapper for BES"))

(in-package :bes)
       
