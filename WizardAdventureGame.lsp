(defparameter *nodes* 
	'(
		(
			living-room (You are in the living-room.  A Wizard is snoring loudly on the couch.)
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
				(attic upsairs ladder)
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
					(cadr (assoc object object-locactions))
				location)
			)
		)
		(
			remove-if-not #'at-loc-p objects
		)
	)
)