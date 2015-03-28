(defparameter *nodes* 
	'(
		(
			living-room (You are in the living-room.  A W izard is snoring loudly on the couch.)
		)
		(
			garden (You are in a beautiful garden.  There is a well in front of you.)
		)
		(
			attic (You are in the attic.  There is a giant welding torch in the corner.)
		)
	)
)

(defparameter *edges* 
	'(
		(
			living-room 
				( garden west door)
				(attic upstairs ladder)
		)
		(
			garden
				(living-room east door)
		)
		(
			attic
				(living-room downstairs ladder)
		)
	)
)

(defparameter *objects* 
	'(
		Whiskey
		Bucket
		Frog
		Chain
	)
)

(defparameter *object-locations*
	'(
		(whiskey living-room)
		(bucket living-room)
		(chain garden)
		(frog garden)
	)
)

(defparameter *players-location* 'living-room)

(defun describe-location (location nodes)
	(cadr(assoc location nodes))
)

(defun describe-path (edge)
	`(There is a ,(caddr edge) going ,(cadr edge) from here.)
)

(defun describe-paths (location edges)
	(
		apply #'append 
		(
			mapcar #'describe-path 
				(
					cdr (assoc location edges)
				)
		)
	)
)

(defun objects-at (location objects object-locations)
	( labels
		(
			(at-loc-p(object)
				(eq
					(cadr 
						(assoc object object-locations)
					)
				location)
			)
		)
		(
			remove-if-not #'at-loc-p objects
		)
	)
)

(defun describe-objects (location objects object-locations)
	(labels
		(
			(describe-object (object)
				`(You see a ,object on the floor)
			)
		)
		(apply #'append 
			(mapcar 
				#'describe-object (objects-at location objects object-locations)
			)
		)
	)
)

(defun look()
	(append
		( describe-location *players-location* *nodes*)
		( describe-paths *players-location* *edges*)
		( describe-objects *players-location* *objects* *object-locations*)
	)
)

(defun walk (direction)
	(let 
		(
			(next
				(find direction
					(cdr
						(assoc *players-location* *edges*)
					)
				:key #'cadr)
			)
		)
		(if  next
			(progn
				(setf *players-location* 
					(car next)
				)
			(look)
			)
		'(You cannot go that way)
		)
	)
)

(defun inventory()
	(objects-at 'body *objects*  *object-locations*)
)

(defun pickup_object(object)
	(cond 
		(
			(member object
				(objects-at *players-location* *objects* *object-locations*)
			)
			(push (list object 'body) *object-locations*)
			`(you are now carrrying the ,object)
		)
		(t
			'(you cannot get that.)
		)
	)
)

(defun drop_object(object)
	(cond 
		(
			(member object
				(inventory)
			)
			(push (list object *players-location*) *object-locations*)
			`(you have dropped the ,object)
		)
		(t
			'(you cannot drop that.)
		)
	)
)