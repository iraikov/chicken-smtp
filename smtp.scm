;;
;;  Parser and state machine for the grammar defined in RFC 5321,
;;  "Simple Mail Transfer Protocol".
;;
;;  Based on the Haskell Rfc2821 module by Peter Simons.
;;
;;  Copyright 2009-2018 Ivan Raikov
;;
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; A full copy of the GPL license can be found at
;; <http://www.gnu.org/licenses/>.

(module smtp

	(
	 mailbox? Mailbox 

	 reply? Reply make-reply 
	 reply-success? reply-failure? reply-shutdown?

	 success-code? success-code-inject success-code-project 
	 Unused PreliminarySuccess Success IntermediateSuccess
	 TransientFailure PermanentFailure

	 category? category-inject category-project
	 Syntax Information Connection Unspecified3 Unspecified4
	 MailSystem

	 code? Code

	 event event?
  
	 cmd? Helo Ehlo MailFrom RcptTo Data Rset Send Soml Saml
	 Vrfy Expn Help Noop Quit Turn WrongArg wrong-arg

	 session-fsm session-fsm?
         parse-cmd start-session
        )

	(import scheme (chicken base) (chicken format) (chicken memory representation)
                srfi-1 utf8 datatype matchable 
                (prefix abnf abnf:) 
		(prefix abnf-consumers abnf:) 
		(only utf8-srfi-13 string-concatenate)
		(only utf8-srfi-14
		      char-set char-set-difference char-set-union
		      char-set:graphic char-set:printing)
		)
        (import-for-syntax matchable)


(define consumed-objects-lift-any
  (abnf:consumed-objects-lift
   (abnf:consumed-objects identity)))

(define (list->domain-string lst)
  (if (and (pair? lst) (char=? (last lst) #\-))
      (error "domain string ends with - " 
	     (list->string lst))
      (list->string lst)))

(define-syntax bind-consumed->domain-string
  (syntax-rules () 
    ((_ p)    (abnf:bind 
	       (abnf:consumed-chars->list list->domain-string)
	       p))
    ))


(define-syntax define-enumerated-type
  (er-macro-transformer
   (lambda (x r c)
     (match-let (((_ typename pred vector inject project . rest) x))
                (let ((%define  (r 'define))
                      (%begin   (r 'begin))
                      (%if      (r 'if)))
                  `(,%begin
                    (,%define (,pred x)    (##sys#structure? x ',typename))
                    (,%define (,project x) (##sys#slot x 2))
                    (,%define (,inject i)  
                              (and (integer? i) (positive? i) (< i (vector-length ,vector)) 
                                   (vector-ref ,vector i)))
                    ,(let loop ((variants rest) (i 0) (defs (list)))
                       (if (null? variants) 
                           `(,%begin ,@defs)
                           (let* ((variant  (car variants))
                                  (def  `(,%define ,variant   
                                                   (##sys#make-structure ',typename ',(car variant) ,i))))
                             (loop (cdr variants) (+ i 1) (cons def defs)))))
                    ,(let loop ((variants rest) (defs (list)))
                       (if (null? variants) 
                           `(,%define ,vector (vector ,@(reverse defs)))
                           (let* ((variant  (car variants))
                                  (def  `(,(car variant))))
                             (loop (cdr variants) (cons def defs)))))
                    ))))
   ))

(define-datatype mailbox mailbox?
  (Mailbox (local-part string?) 
	   (domain string?)))


(define-record-printer (mailbox x out)
  (match x 
	 (($ smtp#mailbox 'Mailbox "" "" )  (fprintf out "<>"))
	 (($ smtp#mailbox 'Mailbox "postmaster" "" )  (fprintf out "<postmaster>"))
	 (($ smtp#mailbox 'Mailbox l d )  
	  (let ((mbox  (sprintf "~A@~A" l d)))
	    (fprintf out "<~A>" mbox)))))

(define (null-mailbox) (Mailbox "" ""))

(define (postmaster . rest) 
  (let-optionals rest ((domain ""))
   (Mailbox "postmaster" domain )))


;; An SMTP reply is a three-digit return code plus some waste of
;; bandwidth called "comments". This is what the list of strings is
;; for; one string per line in the reply.  the record printer will
;; append an CRLF end-of-line marker to each entry in that list, so
;; that the resulting string is ready to be sent back to the peer.
;;
;; Here is an example:
;;
;; > (print (Reply (Code (Success) (MailSystem) 0)
;;                     (list "worked" "like" "a charm")))
;; 250-worked
;; 250-like
;; 250 a charm

(define-datatype reply reply?
  (Reply (code code?) (msg list?)))

(define-enumerated-type 
  success-code success-code? success-vector 
  success-code-inject success-code-project 
  (Unused)
  (PreliminarySuccess)
  (Success)
  (IntermediateSuccess)
  (TransientFailure)
  (PermanentFailure))

(define-enumerated-type 
  category category? category-vector
  category-inject category-project
  (Syntax)
  (Information)
  (Connection)
  (Unspecified3)
  (Unspecified4)
  (MailSystem))

(define-datatype code code?
  (Code (suc success-code?) (cat category?) (num integer?)))

(define-record-printer (reply x out)
  (match x 
	 (($ smtp#reply 'Reply (and c ($ smtp#code 'Code suc cat _)) ())
	  (let ((msg (sprintf "~A in category ~A" suc cat)))
	    (fprintf out "~A" (Reply c (list msg)))))

	 (($ smtp#reply 'Reply code msg) 
	  (let ((prefix-con (sprintf "~A-" code))
		(prefix-end (sprintf "~A " code))
		(fmt        (lambda (p) (lambda (l) (sprintf "~A~A\r\n" p l)))))
	    (match-let (((x . xs) (reverse msg)))
		       (let* ((msg-con (map (fmt prefix-con) xs))
			      (msg-end ((fmt prefix-end) x))
			      (msg1    (reverse (cons msg-end msg-con))))
			 (fprintf out "~A" (string-concatenate msg1))))))
	 ))

(define-record-printer (code x out)
  (cases code x
	 (Code (suc cat n)  
	       (fprintf out "~A~A~A" (success-code-project suc) 
			(category-project cat) n))))
	  
;; Constructs a Reply. 

(define (in-range-incl? lo hi)
  (if (< hi lo) (in-range-incl? hi lo)
      (lambda (x) (and (<= lo x) (<= x hi)))))

(define check-suc  (in-range-incl? 0 5))
(define check-cat  (in-range-incl? 0 5))
(define check-code (in-range-incl? 0 9))

(define (make-reply suc cat n msg)
  (or (and (check-suc suc) (check-cat cat) (check-code n)
	   (Reply (Code (success-code-inject suc) (category-inject cat) n) msg))
      (error 'make-reply "arguments out of range: " suc cat n)))


;; A reply constitutes success if the status code is any of
;; PreliminarySuccess, Success, or IntermediateSuccess.

(define (reply-success? r)
  (match r (($ smtp#reply 'Reply 
	       ($ smtp#code 'Code 
		  ($ smtp#success-code (or 'PreliminarySuccess 
				      'IntermediateSuccess 'Success _) _ _) _))
	    #t)
	 (else #f)))

;; A reply constitutes failure if the status code is either
;; PermanentFailure or TransientFailure.

(define (reply-failure? r)
  (match r (($ smtp#reply 'Reply 
	       ($ smtp#code 'Code 
		  ($ smtp#success-code (or 'PermanentFailure 
				      'TransientFailure _) _ _) _))
	    #t)
	 (else #f)))

;; The replies 221 and 421 signify Shutdown.

(define (reply-shutdown? r)
  (match r (($ smtp#reply 'Reply 
	       ($ smtp#code 'Code ($ smtp#success-code (or 'Success 
						 'TransientFailure) _) 
		  ($ smtp#category 'Connection _) 1) _)
	    #t)
	 (else #f)))


;; Argument Parsers

;; Match any US-ASCII character except for control characters,
;; specials, or space. atom and dot-atom are made up of this.

(define atext
  (abnf:alternatives
   abnf:alpha abnf:decimal
   (abnf:set-from-string "!#$%&'*+-/=?^_`{|}~")))

(define Atom
  (abnf:bind-consumed->string (abnf:repetition1 atext)))

(define Dot-string
  (abnf:bind-consumed->string 
   (abnf:concatenation
    (abnf:repetition1 atext) 
    (abnf:repetition
     (abnf:concatenation
      (abnf:char #\.) 
      (abnf:repetition1 atext))))))

;; backslash followed by any ASCII graphic (including itself) or space
(define quoted-pairSMTP
  (abnf:concatenation
   (abnf:char #\\) 
   (abnf:set char-set:printing)))

;; within a quoted string, any ASCII graphic or space is permitted
;; without blackslash-quoting except double-quote and the backslash
;; itself.
(define qtextSMTP
  (abnf:set
   (char-set-difference 
    char-set:printing
    (char-set #\" #\\))))

(define QcontentSMTP
  (abnf:alternatives qtextSMTP quoted-pairSMTP))

(define Quoted-string
  (abnf:bind-consumed->string 
   (abnf:concatenation
    (abnf:drop-consumed abnf:dquote) 
    (abnf:repetition QcontentSMTP) 
    (abnf:drop-consumed abnf:dquote))))

(define (String Atom Quoted-string)
  (abnf:alternatives Atom Quoted-string))

(define esmtp-keyword
  (abnf:bind-consumed->symbol
   (abnf:concatenation
    (abnf:alternatives abnf:alpha abnf:decimal) 
    (abnf:repetition
     (abnf:alternatives 
      abnf:alpha abnf:decimal (abnf:char #\-))))))


(define esmtp-value
  (abnf:bind-consumed->string
   (abnf:repetition1 
    (abnf:set (char-set-difference
               char-set:graphic (char-set #\= #\space))))))

;; any CHAR excluding "=", SP, and control
;; characters.  If this string is an email address,
;; i.e., a Mailbox, then the "xtext" syntax [32]
;; SHOULD be used.

(define esmtp-param
  (abnf:bind-consumed-strings->list
   (abnf:concatenation
    esmtp-keyword 
    (abnf:optional-sequence
     (abnf:concatenation 
      (abnf:drop-consumed (abnf:char #\=)) 
      esmtp-value)))))



(define Mail-parameters
  (abnf:bind-consumed-pairs->list
   (abnf:concatenation
    esmtp-param
    (abnf:repetition 
     (abnf:concatenation 
      (abnf:drop-consumed abnf:sp) 
      esmtp-param)))))


(define Ldh-str
  (bind-consumed->domain-string
   (abnf:concatenation 
    abnf:alpha 
    (abnf:repetition
     (abnf:alternatives
      abnf:alpha abnf:decimal (abnf:char #\-))))))

(define sub-domain     Ldh-str)

(define domain
  (abnf:bind-consumed-strings->list
   (lambda (l) 
     (string-concatenate (intersperse l ".")))
   (abnf:concatenation 
    sub-domain
    (abnf:repetition
     (abnf:concatenation 
      (abnf:drop-consumed (abnf:char #\.)) 
      sub-domain)))))

(define At-domain
  (abnf:concatenation 
   (abnf:drop-consumed (abnf:char #\@)) 
   domain))

(define A-d-l
  (abnf:bind-consumed-strings->list 
   (abnf:concatenation
    At-domain
    (abnf:repetition
     (abnf:concatenation
      (abnf:drop-consumed (abnf:char #\,)) 
      At-domain)))))

(define Local-part
  (abnf:alternatives
   Dot-string
   Quoted-string))

(define IPv6-hex
  (abnf:bind-consumed->string 
   (abnf:variable-repetition 1 4 abnf:hexadecimal)))

(define cIPv6-hex
  (abnf:concatenation
   (abnf:drop-consumed (abnf:char #\:)) 
   IPv6-hex))

(define IPv6-full
  (abnf:bind-consumed-strings->list 
   (abnf:concatenation 
    IPv6-hex
    (abnf:repetition-n 7 cIPv6-hex))))

(define IPv6-comp
  (abnf:bind-consumed-strings->list
   (abnf:concatenation
    (abnf:optional-sequence
     (abnf:concatenation 
      IPv6-hex
      (abnf:variable-repetition 0 5 cIPv6-hex)))
    (abnf:bind-consumed->string (abnf:lit "::"))
    (abnf:optional-sequence
     (abnf:concatenation 
      IPv6-hex
      (abnf:variable-repetition 0 5 cIPv6-hex))))))

;; The "::" represents at least 2 16-bit groups of zeros.  No more
;; than 6 groups in addition to the "::" may be present.

(define Snum
  (abnf:bind-consumed->string
   (abnf:variable-repetition 1 3 abnf:decimal)))

(define IPv4-address-literal
  (abnf:concatenation 
   Snum 
   (abnf:repetition-n 
    3 (abnf:concatenation 
       (abnf:drop-consumed (abnf:char #\.))  
       Snum))))


(define IPv6v4-full
  (abnf:bind-consumed-strings->list
   (abnf:concatenation 
    IPv6-hex (abnf:repetition-n 5 cIPv6-hex) 
    (abnf:drop-consumed (abnf:char #\:))
    IPv4-address-literal)))


(define IPv6v4-comp
  (abnf:bind-consumed-strings->list
   (abnf:concatenation 
    (abnf:optional-sequence
     (abnf:concatenation
      IPv6-hex
      (abnf:variable-repetition 0 3 cIPv6-hex)))
    (abnf:bind-consumed->string (abnf:lit "::"))
    (abnf:optional-sequence
     (abnf:concatenation
      IPv6-hex
      (abnf:variable-repetition 0 3 cIPv6-hex) 
      (abnf:drop-consumed (abnf:char #\:))))
    IPv4-address-literal)))


;; The "::" represents at least 2 16-bit groups of zeros.  No more
;; than 4 groups in addition to the "::" and IPv4-address-literal may
;; be present.

(define  IPv6-addr
  (abnf:alternatives IPv6-full IPv6-comp IPv6v4-full IPv6v4-comp))

(define IPv6-address-literal
  (abnf:concatenation
   (abnf:bind-consumed->string (abnf:lit "IPv6:")) IPv6-addr))

(define dcontent
  (abnf:set (char-set-difference
             char-set:printing 
             (char-set #\[ #\] #\\))))


(define Standardized-tag
  (abnf:bind-consumed->symbol Ldh-str))
;; Standardized-tag MUST be specified in a Standards-Track RFC and
;; registered with IANA


(define General-address-literal
  (abnf:concatenation
   Standardized-tag (abnf:drop-consumed (abnf:char #\:))
   (abnf:repetition1 dcontent)))


(define address-literal
  (abnf:concatenation
   (abnf:char #\[) 
   (abnf:alternatives
    IPv4-address-literal 
    IPv6-address-literal 
    General-address-literal)
   (abnf:char #\])))

;; See Section 4.1.3

(define Mailbox-p
  (abnf:bind
   (consumed-objects-lift-any 
    (lambda (x) (Mailbox (first x) (second x))))
   (abnf:concatenation 
    Local-part
    (abnf:drop-consumed (abnf:char #\@) )
    (abnf:alternatives domain address-literal))))

(define Path-p
  (abnf:bind 
   (consumed-objects-lift-any first)
   (abnf:concatenation
    (abnf:drop-consumed (abnf:char #\<) )
    (abnf:optional-sequence 
     (abnf:drop-consumed
      (abnf:concatenation 
       A-d-l 
       (abnf:char #\:))))
    Mailbox-p
    (abnf:drop-consumed (abnf:char #\>)))))

(define Forward-path   Path-p)

(define Reverse-path
  (abnf:alternatives 
   (abnf:bind
    (consumed-objects-lift-any 
     (lambda x (null-mailbox)))
    (abnf:concatenation
     (abnf:char #\<) (abnf:char #\>)))
   Path-p))

(define from-path
  (abnf:concatenation
   (abnf:drop-consumed (abnf:lit "FROM:"))
   Reverse-path))

(define to-path
  (abnf:concatenation
   (abnf:drop-consumed (abnf:lit "TO:"))
   (abnf:alternatives 
    
    (abnf:bind
     (consumed-objects-lift-any 
      (lambda (x) (postmaster)))
     (abnf:concatenation
      (abnf:char #\<) 
      (abnf:lit "Postmaster") 
      (abnf:char #\>)))
    
    (abnf:bind
     (consumed-objects-lift-any 
      (lambda (x) (postmaster (first x))))
     (abnf:concatenation
      (abnf:drop-consumed (abnf:char #\<) )
      (abnf:drop-consumed (abnf:lit "Postmaster@") )
      domain 
      (abnf:drop-consumed (abnf:char #\>))))

    Forward-path)))


;; ESMTP sessions, events, commands

(define-datatype session-state session-state?
  (Unknown)
  (HaveHelo)
  (HaveMailFrom)
  (HaveRcptTo)
  (HaveData)
  (HaveQuit))

(define-record-printer (session-state x out)
  (fprintf out "<#session-state ~A>" 
	   (cases session-state x
		  (Unknown ()      "Unknown")
		  (HaveHelo ()     "HaveHelo")
		  (HaveMailFrom () "HaveMailFrom")
		  (HaveRcptTo   () "HaveRcptTo")
		  (HaveData     () "HaveData")
		  (HaveQuit     () "HaveQuit"))))

(define-datatype event event?
  (SayHelo       (s string?))
  (SayHeloAgain  (s string?))
  (SayEhlo       (s string?))
  (SayEhloAgain  (s string?))
  (SetMailFrom   (m mailbox?) (parameters? list?))
  (AddRcptTo     (m mailbox?) (parameters? list?))
  (StartData)
  (NeedHeloFirst)
  (NeedMailFromFirst)
  (NeedRcptToFirst)
  (NotImplemented) ;; Turn, Send, Soml, Saml, Vrfy, Expn.
  (ResetState)
  (SayOK)
  ;; Triggered in case of Noop or when Rset is used before
  ;; we even have a state.
  (SeeksHelp    (s string?))
  (Shutdown)
  (SyntaxErrorIn (s string?))
  (Unrecognized  (s string?)))
  
(define-datatype cmd cmd?
  (Helo (s string?))
  (Ehlo (s string?))
  (MailFrom (m mailbox?) (parameters list?))
  (RcptTo   (m mailbox?) (parameters list?))
  (Data)
  (Rset)
  (Send  (m mailbox?))
  (Soml  (m mailbox?))
  (Saml  (m mailbox?))
  (Vrfy  (s string?))
  (Expn  (s string?))
  (Help  (s string?))
  (Noop)
  (Quit)
  (Turn)
;; When a valid command has been recognized, but the
;; argument parser fails, then this type will be
;; returned. 
  (WrongArg (cmd string?)  (message string?)))

(define-record-printer (cmd x out)
  (cases cmd x 
	 (Helo (s)        (fprintf out "HELO ~A" s))
	 (Ehlo (s)        (fprintf out "EHLO ~A" s))
	 (MailFrom (m p)  (fprintf out "MAIL FROM:~A" m))
	 (RcptTo (m p)    (fprintf out "RCPT TO: ~A" m))
	 (Data ()         (fprintf out "DATA"))
	 (Rset ()         (fprintf out "RSET"))
	 (Send (m)        (fprintf out "SEND ~A" m))
	 (Soml (m)        (fprintf out "SOML ~A" m))
	 (Saml (m)        (fprintf out "SAML ~A" m))
	 (Vrfy (s)        (fprintf out "VRFY ~A" s))
	 (Expn (s)        (fprintf out "EXPN ~A" s))
	 (Noop ()         (fprintf out "NOOP"))
	 (Quit ()         (fprintf out "QUIT"))
	 (Turn ()         (fprintf out "TURN"))
	 (Help (s)        (fprintf out "HELP ~A" s))
	 (WrongArg (s)    (fprintf out "Syntax error in argument of ~A." s))))

;; Command Parsers

;; Constructs a parser for a command without arguments.

(define (mkcmdp0 s kons)  
    (define (ignore x) (kons))
    (let ((ss (->string s)))
      (abnf:bind (consumed-objects-lift-any ignore)
		 (abnf:concatenation
		  (abnf:bind-consumed->symbol (abnf:lit ss))
		  (abnf:drop-consumed (abnf:repetition abnf:sp))
		  (abnf:drop-consumed abnf:crlf)
		  ))
      ))

;; Constructs a WrongArg command
(define (wrong-arg cmd)
  (abnf:bind (lambda (x) (list (WrongArg cmd "")))
	     abnf:pass))

;; Constructs a parser for a command with an argument, which the given
;; parser will handle. The result of the argument parser will be
;; applied to the given constructor procedure before returning.

(define (mkcmdp1 s kons p . r)
    (let ((ss (->string s))
	  (make (if (null? r)
		    (lambda (x) (kons (first x)))
		    (lambda (x)
		      (match x ((x r) (kons x r))
			     ((x) (kons x (list)))
			     )))))

      (abnf:bind (consumed-objects-lift-any make) 
		 
		 (abnf:concatenation
		  
		  (abnf:drop-consumed (abnf:lit ss))
		  (abnf:drop-consumed (abnf:repetition abnf:sp))
	
		  (abnf:alternatives p (wrong-arg ss) )
		  
		  (if (null? r)
		      (abnf:drop-consumed abnf:crlf)
		      (abnf:concatenation
		       (abnf:optional-sequence 
			(abnf:concatenation
			 (abnf:drop-consumed (abnf:repetition abnf:sp))
			 (car r)))
		       (abnf:drop-consumed abnf:crlf)))

		  ))
      ))



;; Parsers for (optional) argument strings

(define Arg-string
  (abnf:concatenation 
   (abnf:drop-consumed abnf:sp) String))

(define Opt-string
  (abnf:optional-sequence
   (abnf:concatenation 
    (abnf:drop-consumed abnf:sp) String)))

;; ESMTP State Machine

(define-datatype session-fsm session-fsm?
  (Event (ev event?))
  (Trans (ev event?) (fsm procedure?)))

(define smtp-cmd
  (let (
        (data (mkcmdp0 "DATA" Data))
        (rset (mkcmdp0 "RSET" Rset))
        (quit (mkcmdp0 "QUIT" Quit))
        (turn (mkcmdp0 "TURN" Turn))
        (helo (mkcmdp1 "HELO" Helo     domain))
        (ehlo (mkcmdp1 "EHLO" Ehlo     domain))
        (vrfy (mkcmdp1 "VRFY" Vrfy     Arg-string))
        (expn (mkcmdp1 "EXPN" Expn     Arg-string))
        
        (rcpt (mkcmdp1 "RCPT" RcptTo   to-path Mail-parameters))
        (mail (mkcmdp1 "MAIL" MailFrom from-path Mail-parameters))
        (send (mkcmdp1 "SEND" Send     from-path))
        (soml (mkcmdp1 "SOML" Soml     from-path))
        (saml (mkcmdp1 "SAML" Saml     from-path))
        
        (help (mkcmdp1 "HELP" (lambda (x) (if (null? x) (Help) (Help (car x))))
                       Opt-string))
        
        (noop0 (mkcmdp1 "NOOP" (lambda (x) (Noop)) Opt-string))
        )

    (abnf:alternatives
     data rset noop0 quit turn helo mail rcpt 
     send soml saml vrfy expn help ehlo)
    ))


(define (parse-cmd k s)
  (smtp-cmd (compose k caar) identity s))

(define start-session
;; Parses an SMTP protocol line and runs handle-cmd to determine the
;; event. In case of syntax errors, SyntaxErrorIn or Unrecognized will
;; be returned.  Inputs must be terminated with CRLF.
  (letrec* ((fsm            (lambda (st) (let ((k (handle-cmd st))) (lambda (s) (parse-cmd k s)))))
            (event          Event)
            (trans          (lambda (st ev) (Trans ev (fsm st))))
            (start-session  (lambda () (fsm (Unknown))))
            (handle-cmd     (lambda (st) 
			       (lambda (cmd)
				 (match (list st cmd )
					((($ smtp#session-state 'HaveQuit) _)  (event (Shutdown)))
					
					((_       ($ smtp#cmd 'WrongArg c _))  (event (SyntaxErrorIn c)))
					((_       ($ smtp#cmd 'Quit))          (trans (HaveQuit) (Shutdown)))
					((_       ($ smtp#cmd 'Noop))          (event (SayOK) ))
					
					((_       ($ smtp#cmd 'Turn))          (event (NotImplemented) ))
					((_       ($ smtp#cmd 'Send _))        (event (NotImplemented) ))
					((_       ($ smtp#cmd 'Soml _))        (event (NotImplemented) ))
					((_       ($ smtp#cmd 'Saml _))        (event (NotImplemented) ))
					((_       ($ smtp#cmd 'Vrfy _))        (event (NotImplemented) ))
					((_       ($ smtp#cmd 'Expn _))        (event (NotImplemented) ))
					
					((_       ($ smtp#cmd 'Help x))        (event (SeeksHelp x) ))
				       
				       ((($ smtp#session-state 'Unknown)   ($ smtp#cmd 'Rset))          
					(event (SayOK) ))
				       ((($ smtp#session-state 'HaveHelo)  ($ smtp#cmd 'Rset))          
					(event (SayOK) ))
				       ((_                            ($ smtp#cmd 'Rset))          
					(trans (HaveHelo) (ResetState )))
				       
				       ((($ smtp#session-state 'Unknown)   ($ smtp#cmd 'Helo x))        
					(trans (HaveHelo) (SayHelo x)))
				       ((_                            ($ smtp#cmd 'Helo x))        
					(trans (HaveHelo) (SayHeloAgain x)))
				       ((($ smtp#session-state 'Unknown)   ($ smtp#cmd 'Ehlo x))        
					(trans (HaveHelo) (SayEhlo x)))
				       ((_                            ($ smtp#cmd 'Ehlo x))        
					(trans (HaveHelo) (SayEhloAgain x)))

				       ((($ smtp#session-state 'Unknown)   ($ smtp#cmd 'MailFrom . _))  
					(event (NeedHeloFirst)))
				       ((_                            ($ smtp#cmd 'MailFrom x p))  
					(trans (HaveMailFrom) (SetMailFrom x p)))
				       
				       ((($ smtp#session-state 'Unknown)   ($ smtp#cmd 'RcptTo . _))    
					(event (NeedHeloFirst)))
				       ((($ smtp#session-state 'HaveHelo)  ($ smtp#cmd 'RcptTo . _))    
					(event (NeedMailFromFirst)))
				       ((_                            ($ smtp#cmd 'RcptTo x p))    
					(trans (HaveRcptTo) (AddRcptTo x p)))
				       
				       ((($ smtp#session-state 'Unknown)      ($ smtp#cmd 'Data))     
					(event (NeedHeloFirst)))
				       ((($ smtp#session-state 'HaveHelo)     ($ smtp#cmd 'Data))     
					(event (NeedMailFromFirst)))
				       ((($ smtp#session-state 'HaveMailFrom) ($ smtp#cmd 'Data))     
					(event (NeedRcptToFirst)))
				       ((($ smtp#session-state 'HaveRcptTo)   ($ smtp#cmd 'Data))     
					(trans (HaveData) (StartData)))
				       
				       ((($ smtp#session-state 'HaveData)   _)     (event (StartData)))

				       ((_  _)                           (event (Unrecognized "")))
				       
				       ))
			      ))
	     )
      start-session
      ))

    

)
