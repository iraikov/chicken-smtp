
;; An example MTA implementation

(import  datatype srfi-1 abnf smtp srfi-13)

(define domain    "example.net")
(define host      "chicken-mta")
(define mailfrom  (make-parameter #f))
(define rcpto     (make-parameter '()))
(define data      (make-parameter #f))


(define (handle-event ev)
  (cases event ev
	 (SayHelo (s)
	  (Reply (Code (Success) (MailSystem) 0)
		 (list host " " "Hello " s)))
	 
	 (SayHeloAgain (s)
	  (Reply (Code (Success) (MailSystem) 0)
		 (list host " " "Hello " s)))

	 (SayEhlo (s)
	  (Reply (Code (Success) (MailSystem) 0)
		 (list host " " "Hello " s)))
	 
	 (SayEhloAgain (s)
	  (Reply (Code (Success) (MailSystem) 0)
		 (list host " " "Hello " s)))
	 
	 (SetMailFrom (m)
	   (mailfrom m)
	   (Reply (Code (Success) (MailSystem) 0) 
		  (list "OK")))

	 (AddRcptTo (m)
	    (if (not (mailfrom))
	       (Reply (Code (PermanentFailure) (Syntax) 3)
	   	      (list "command out of sequence"))
	       (begin
		 (rcpto (cons m (rcpto)))
		 (Reply (Code (Success) (MailSystem) 0) 
			(list "Accepted")))))

	 (StartData ()
	    (if (not (rcpto))
	       (Reply (Code (PermanentFailure) (MailSystem) 4)
	   	      (list "no valid recipients"))
	       (begin
		 (data (list))
		 (Reply (Code (IntermediateSuccess) (MailSystem) 4)
			(list "Ready")))))

	 (NeedHeloFirst ()
	   (Reply (Code (PermanentFailure) (Syntax) 3)
	   	      (list "command out of sequence: "
			    "need HELO first")
		      ))

	 (NeedMailFromFirst ()
	   (Reply (Code (PermanentFailure) (Syntax) 3)
	   	      (list "command out of sequence: "
			    "need MAIL first")
		      ))

	 (NeedMailRcptToFirst ()
	   (Reply (Code (PermanentFailure) (Syntax) 3)
	   	      (list "command out of sequence: "
			    "need RCPT first")
		      ))

	 (NotImplemented ()
	   (Reply (Code (PermanentFailure) (Syntax) 2)
		  (list "command not implemented")))


	 (ResetState ()
	     (mailfrom #f)
	     (rcpto    #f)
	     (data     #f)
	     (Reply (Code (Success) (MailSystem) 0) 
		    (list "Reset OK")))

	 (SayOK ()
	     (Reply (Code (Success) (MailSystem) 0) 
		    (list "OK")))

	 (SeeksHelp (s)
	     (Reply (Code (Success) (Information) 4) 
		    (list "Commands supported:"
			  "HELO EHLO MAIL RCPT DATA QUIT RSET NOOP HELP")))

	 (Shutdown ()
	    (Reply (Code (Success) (MailSystem) 1)
		   (list host " closing connection")))

	 (SyntaxErrorIn (s)
	    (Reply (Code (PermanentFailure) (Syntax) 1)
		   (list "syntax error in " s)))

	 (Unrecognized (s)
	    (Reply (Code (PermanentFailure) (Syntax) 0)
		   (list "Unrecognized " s)))
	 ))

;; from SSAX lib
(define (peek-next-char port)
  (read-char port) 
  (peek-char port))

(define (read-smtp-line port)
  (let loop ((cs (list)))
    (let ((c (peek-char port)))
    (if (eof-object? c) (reverse cs)
	(let ((n (peek-next-char port)))
	  (cond ((and (eq? n #\newline) (eq? c #\return))
		 (begin
		   (read-char port)
		   (reverse (cons* n c cs)))
		 )
		(else (loop (cons c cs)))))))))

(define data-end (list #\. #\return #\newline))
      
(define (handle-data in out cont)
  (let loop ((tempdata (list)))
    (let ((line (read-smtp-line in)))
      (if (equal? line data-end)
	  (begin (data (reverse tempdata))
		 (fprintf out "~A" 
			  (Reply (Code (Success) (MailSystem) 0) (list "OK")))
		 (cont)) 
	  (loop (cons (list->string line) tempdata))))))

(define (main in out)  
  (let loop ((fsm (start-session)))
    (let ((line     (read-smtp-line in)))
      (if (null? line) (loop fsm)
	  (let ((instream `(() ,line)))
	    (let-values
	     (((reply ev fsm)
	       (cases session-fsm (fsm instream)
		      (Event (ev)
			     (let ((reply (handle-event ev)))
			       (values reply ev fsm)))
		      (Trans (ev fsm)
			     (let ((reply (handle-event ev)))
			       (values reply ev fsm))))))
	     (fprintf out "~A" reply)
	     (cases event ev
		    (StartData ()
			       (handle-data in out (lambda () (loop fsm))))
		    (Shutdown ()
			      (begin))
		    (else (loop fsm)))))))))
		     
