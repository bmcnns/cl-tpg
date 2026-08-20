(in-package :bes-gym)

(defun obs->array (obs)
  "Coerces an observation OBS from list to simple double-float array.
   Our TPG implementation assumes double-float arrays for maximum performance.
   Py4CL sends the observations as lists."
  (if (listp obs)
      (map '(simple-array double-float (*))
	   (lambda (x) (coerce x 'double-float))
	   obs)
      (map '(simple-array double-float (*))
	   (lambda (x) (coerce x 'double-float))
	   (list obs))))

(defun make (environment-name &rest make-kwargs &key (video-path nil) &allow-other-keys)
  "Makes a new Gymnasium environment. If you are using a custom Gymnasium environment,
   make sure to register it first."
  (let* ((filtered-kwargs (alexandria:remove-from-plist make-kwargs :video-path))
	 (env (if video-path
		  (apply #'py4cl2:pycall "gym.make" environment-name
			 :render_mode "rgb_array"
			 filtered-kwargs)
		  (apply #'py4cl2:pycall "gym.make" environment-name
			 filtered-kwargs))))
    (if video-path
	(py4cl2:pycall "gym.wrappers.RecordVideo"
		       env
		       "./"
		       :episode_trigger (py4cl2:pyeval "lambda x: True"))
	env)))
  
(defun reset (env seed)
  "Reset the environment to a fresh start (determined by SEED).
   Calls env.reset(seed=SEED)."
  (obs->array (car (py4cl2:pymethod env "reset" :seed seed))))

(defun step (env action)
  "Interact with the environment by taking action ACTION.
   Calls env.step(action) in Python."
  (destructuring-bind (obs rew term trunc info)
      (py4cl2:pymethod env "step" action)
    (values (obs->array obs) rew term trunc info)))

(defun rollout (root-team environment-name seed &rest rollout-kwargs
		&key (video-path nil) &allow-other-keys)
  "Runs a complete episode against a Gymnasium environment with name ENVIRONMENT-NAME
   using ROOT-TEAM as the policy. Specify a SEED to control for the starting state.
   Specify a filename for video-path if you want to record a video of the agent."
  (py4cl2:pyexec "import gymnasium as gym")
  (let* ((filtered-kwargs (alexandria:remove-from-plist rollout-kwargs :video-path))
	 (env (apply #'make environment-name :video-path video-path filtered-kwargs))
	 (episode-reward 0.0)
	 (observation (reset env seed))
	 (states '()))
    (unwind-protect
	 (loop for timestep from 0
	       do (let ((action (bes:execute-team root-team observation)))
		    (multiple-value-bind (obs rew term trunc info)
			(step env action)
		      (declare (ignore info))
		      (incf episode-reward rew)
		      (setf observation obs)
		      (push obs states)
		      (when (or term trunc)
			(return)))))
      (ignore-errors
       (py4cl2:pymethod env "close")
       (when (and video-path
		  (probe-file "rl-video-episode-0.mp4"))
	 (rename-file "rl-video-episode-0.mp4" video-path))))
    (values episode-reward states)))
       
		  
		  
    
	 
  



