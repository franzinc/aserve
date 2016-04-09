;;
;; copyright (c) 2012-2015 Franz Inc, Oakland, CA - All rights reserved.
;;
;; This code is free software; you can redistribute it and/or
;; modify it under the terms of the version 2.1 of
;; the GNU Lesser General Public License as published by 
;; the Free Software Foundation, as clarified by the AllegroServe
;; prequel found in license-allegroserve.txt.
;;
;; This code is distributed in the hope that it will be useful,
;; but without any warranty; without even the implied warranty of
;; merchantability or fitness for a particular purpose.  See the GNU
;; Lesser General Public License for more details.
;;
;; Version 2.1 of the GNU Lesser General Public License is in the file 
;; license-lgpl.txt that was distributed with this file.
;; If it is not present, you can access it from
;; http://www.gnu.org/copyleft/lesser.txt (until superseded by a newer
;; version) or write to the Free Software Foundation, Inc., 59 Temple Place, 
;; Suite 330, Boston, MA  02111-1307  USA

(in-package :net.aserve)

#+(version= 8 2)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (handler-case (dequeue (make-instance 'mp:queue) :timeout 0 :empty-queue-result :foo)
    (error ()
      (pushnew 'queue-does-not-timeout *features*))))

#-(and (version>= 8 2) (not net.aserve::queue-does-not-timeout))
(progn

(defclass queue-with-timeout ()
  ((items :initform nil :accessor items-of)
   (gate :initform (mp:make-gate nil) :reader gate-of)
   (dequeue-lock :initform (mp:make-process-lock) :reader dequeue-lock-of)))

(defun make-queue-with-timeout ()
  (make-instance 'queue-with-timeout))
 
(defun enqueue (queue thing)
  (mp:with-process-lock ((dequeue-lock-of queue))
    (push thing (items-of queue))
    (mp:open-gate (gate-of queue)))
  thing)


(defun dequeue (queue &key (wait t))
  (flet ((dequeue-without-waiting ()
	   (mp:with-process-lock ((dequeue-lock-of queue))
	     (unless (null (items-of queue))
	       (return-from dequeue
		 (multiple-value-prog1 (values (pop (items-of queue)) t)
		   (if* (null (items-of queue))
		      then (mp:close-gate (gate-of queue)))))))))
    (if* wait
       then (let* ((timeout (and (numberp wait) wait))
		   (started-at (get-internal-real-time))
		   (wait-until (if* timeout
				  then (+ started-at (* timeout internal-time-units-per-second))))
		   (timeout-remaining timeout))
	      (dequeue-without-waiting)
	      (while (or (null timeout-remaining)
			 (> timeout-remaining 0.08))
		(if* timeout
		   then (mp:process-wait-with-timeout "Waiting for gate on potentially timeoutable queue"
						      timeout-remaining
						      #'mp:gate-open-p
						      (gate-of queue))
			(setf timeout-remaining (max 0 (/ (- wait-until (get-internal-real-time)) internal-time-units-per-second)))
                   
		   else (mp:process-wait "Waiting for gate on potentially timeoutable queue"
					 #'mp:gate-open-p
					 (gate-of queue)))
		(dequeue-without-waiting))
	      (values nil nil))
       else (dequeue-without-waiting)))))


#+(and (version>= 8 2) (not net.aserve::queue-does-not-timeout))
(progn

(defun make-queue-with-timeout ()
  (make-instance 'mp:queue))

(defun enqueue (queue thing)
  (mp:enqueue queue thing))

(defun dequeue (queue &key (wait t))
  (let* ((failure '#:failure)
	 (result (mp:dequeue queue :wait wait :empty-queue-result failure
			     :whostate "Waiting on potentially timeoutable queue")))
    (if* (eql result failure)
       then (values nil nil)
       else (values result t)))))
