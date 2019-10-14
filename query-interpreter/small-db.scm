(define *db*
  '(
    (address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))
    (job (Bitdiddle Ben) (computer wizard))
    (salary (Bitdiddle Ben) 60000)

    (address (Hacker Alyssa P) (Cambridge (Mass Ave) 78))
    (job (Hacker Alyssa P) (computer programmer))
    (salary (Hacker Alyssa P) 40000)
    (supervisor (Hacker Alyssa P) (Bitdiddle Ben))

    (address (Fect Cy D) (Cambridge (Ames Street) 3))
    (job (Fect Cy D) (computer programmer))
    (salary (Fect Cy D) 35000)
    (supervisor (Fect Cy D) (Bitdiddle Ben))

    (address (Reasoner Louis) (Slumerville (Pine Tree Road) 80))
    (job (Reasoner Louis) (computer programmer trainee))
    (salary (Reasoner Louis) 30000)
    (supervisor (Reasoner Louis) (Hacker Alyssa P))

    (supervisor (Bitdiddle Ben) (Warbucks Oliver))

    (address (Warbucks Oliver) (Swellesley (Top Heap Road)))
    (job (Warbucks Oliver) (administration big wheel))
    (salary (Warbucks Oliver) 150000)

    (address (Aull DeWitt) (Slumerville (Onion Square) 5))
    (job (Aull DeWitt) (administration secretary))
    (salary (Aull DeWitt) 25000)
    (supervisor (Aull DeWitt) (Warbucks Oliver))

    (can-do-job (computer wizard) (computer programmer))
    (can-do-job (computer wizard) (computer technician))

    (can-do-job (computer programmer)
		(computer programmer trainee))

    (can-do-job (administration secretary)
		(administration big wheel))

    (rule (lives-near ?person-1 ?person-2)
	  (and (address ?person-1 (?town . ?rest-1))
	       (address ?person-2 (?town . ?rest-2))
	       (not (same ?person-1 ?person-2))))

    (rule (same ?x ?x))

    (rule (replaceable-by ?person1 ?person2)
	  (and
	   (or
	    (and
	     (job ?person1 . ?jobdescr)
	     (job ?person2 . ?jobdescr))
	    (and
	     (can-do-job ?jobdesc1 ?jobdesc2)
	     (job ?person1 ?jobdesc1)
	     (job ?person2 ?jobdesc2)))
	   (not (same ?person1 ?person2))))

    (rule (wheel ?person)
	  (and (supervisor ?middle-manager ?person)
	       (supervisor ?x ?middle-manager)))

    (rule (outranked-by ?staff-person ?boss)
	  (or (supervisor ?staff-person ?boss)
	      (and (supervisor ?staff-person ?middle-manager)
		   (outranked-by ?middle-manager ?boss))))))

(for-each
 (lambda (entry)
   (let ((assertion (list 'assert! entry)))
     (display assertion)
     (newline)))
 *db*)


  
