(in-package :bes)

(defvar *trial* 1)
(defvar *fitness-file*)
(defvar *cdf-file*)
(defvar *state-visitation-file*)
(defvar *video-file*)
(defvar *exploitability-file*)
(defvar *champion-file*)

(defvar *stochastic*)
(defvar *entropy-regularized*)
(defvar *monte-carlo-rollouts*)

(defun seed-or-random-seed (seed)
  "The start-search TCP packet will either contain :random or an integer seed.
   If an integer is provided, return it as is. If it is :random, return a random integer."
  (if (eq seed :random)
      (random 9999999999)
      seed))

(defun make-initial-population ()
  "Set the population to an initial set of random candidate solutions."
  (setf *teams* (loop repeat *population-size*
			   collect (make-team))))

(defun accuracy (team dataset)
  (let ((predictions (execute-team-on-dataset team dataset))
	(actuals (actions dataset)))
    (/ (loop for actual across actuals
	  for predicted in predictions
	     count (= actual predicted))
       (length actuals))))

(defun log-likelihood (team dataset)
  (let ((predictions (execute-team-on-dataset team dataset))
        (actuals (actions dataset)))
    (/ (loop for probabilities in predictions
             for action across actuals
	     sum (log (max 1d-12 (nth action probabilities))))
       (length actuals))))

(defun make-fitness-function (&rest kwargs &key gym-environment-name dataset-name &allow-other-keys)
  (let ((filtered-kwargs (alexandria:remove-from-plist kwargs :gym-environment-name :dataset-name)))
    (cond
      (gym-environment-name
       (setf *fitness-fn*
             (let (current-gen-seeds current-gen)
               (lambda (team)
                 ;; Generate a new set of seeds only when *generation* advances
                 (unless (eql current-gen *generation*)
                   (setf current-gen *generation*
                         current-gen-seeds (loop repeat *monte-carlo-rollouts* collect (random 9999999))))
                 (/ (reduce #'+
                            (loop for seed in current-gen-seeds
				  if (and *stochastic* *entropy-regularized*)
				    collect (apply #'bes-gym:entropy-regularized-rollout
						   team
						   gym-environment-name
						   seed
						   filtered-kwargs)
				  else
				    collect (apply #'bes-gym:rollout 
						   team 
						   gym-environment-name 
						   seed 
						   filtered-kwargs)))
                    (length current-gen-seeds))))))
      (dataset-name
       (let ((dataset (load-dataset dataset-name)))
         (setf *fitness-fn*
	       (if *stochastic*
		   (lambda (team)
		     (log-likelihood team dataset))
		   (lambda (team)
		     (accuracy team dataset)))))))))

(defun safe-evaluate-team (team)
  (cons team
        (handler-case
            (funcall *fitness-fn* team)
          (floating-point-overflow () :bad)
          (floating-point-invalid-operation () :bad)
          (division-by-zero () :bad)
          (error () :bad))))

(defun evaluate ()
  "Returns a list of (team . fitness), skipping and deleting bad teams."
  (let* ((results (mapcar #'safe-evaluate-team
                                     (root-teams)))
         (bad-teams (loop for (team . fitness) in results
                          when (eq fitness :bad)
                            collect team))
         (good-results (remove :bad results :key #'cdr)))
    ;; Do mutation/deletion serially.
    (dolist (team bad-teams)
      (delete-team team))
    good-results))

(defun select (scores)
  "Remove GAP percent of the population by removing the worst teams."
  (let* ((sorted (sort (copy-list scores) #'> :key #'cdr))
	 (n-remove (floor (* *gap* (length scores))))
	 (worst (last sorted n-remove))
	 (best-fitness (cdr (first sorted))))
    (emit-fitness-scores (who-am-i) best-fitness *generation*)
    (dolist (entry worst)
      (delete-team (car entry)))))

(defun should-send-migrants-p ()
  "Returns T periodically when the generation matches the migration interval."
  (= (mod *generation* *migration-interval*) 0))

(defun send-migrants (evaluation-scores)
  "Periodically send the best individual from this island to another island."
  (let* ((island-id (who-am-i))
	 (neighbours (get-neighbour-ids island-id)))
    (when neighbours
      (let ((random-neighbour (random-choice neighbours))
	    (best-individual (car (alexandria:extremum evaluation-scores #'> :key #'cdr))))
	(send-migrant-over-socket random-neighbour best-individual)))))

(defun receive-migrants ()
  "Replaces the worst individuals unless the migration buffer
   exceeds the population size (albeit unlikely) in which case
   it simply adds them all to the population."

  ;; Internal teams are added unconditionally
  (loop for internal-team = (pop-internal-team)
	while internal-team
	do (push internal-team *teams*))

  ;; Root teams compete for the 'worst' slots.
  (loop for root-team = (pop-root-team)
	while root-team
	do (push root-team *teams*)))

(defun reproduce ()
  (loop while (< (length (root-teams)) *population-size*)
	do (mutate-team (clone-team (random-choice (root-teams))))))

(defun evolve ()
  "Evolve the population for a single generation."
 ; (receive-migrants)

  (let* ((evaluation-scores (evaluate))
	 (best-score (alexandria:extremum evaluation-scores #'> :key #'cdr)))
    (with-open-file (str *fitness-file*
			 :direction :output
			 :if-does-not-exist :create
			 :if-exists :append)
      (when (zerop (file-length str))
	(format str "TRIAL GENERATION FITNESS~%"))
      (format str "~A ~A ~,8F~%" *trial* *generation* (cdr best-score)))

    (format t "Trial ~5A Generation ~5A Fitness ~,8F~%" *trial* *generation* (cdr best-score))

    (setf *champion* (car best-score))

    (select evaluation-scores)
    
    (reproduce)))

(defun run-search (mode gym-environment-name dataset-name seed generations &rest kwargs)
  "Search the solution space with a tangled program graph."
  (let* ((seed (seed-or-random-seed seed))
	 (captured-state (sb-ext:seed-random-state seed)))
    (setf *random-state* captured-state)

    (setf *teams* nil)
    (setf *champion* nil)
    (setf *generation* 1)
    
    ;; make the initial population
    (make-initial-population)

    (ecase mode
      (:online (apply #'make-fitness-function :gym-environment-name gym-environment-name kwargs))
      (:offline (make-fitness-function :dataset-name dataset-name)))

    (loop while (<= *generation* generations)
	  do (evolve)
	  do (incf *generation*))))
		   
(defun best-policy ()
  "Returns the individual in *teams* with the highest fitness."
  (alexandria:extremum (evaluate) #'> :key #'cdr))
