# smtp

## Description

`smtp` is a collection of parser combinators and state machine
primitives for the grammar defined in
RFC 5321 (http://www.ietf.org/rfc/rfc5321.txt)
(Simple Mail Transfer Protocol).

## Data Types for SMTP Commands

Represenation of SMTP commands:

```scheme
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
  (WrongArg (cmd string?)  (message string?)))
```

The record printer defined for this datatype prints values of this
type in a format conforming to the RFC. For example:

```
  csi> (print (Helo "myhost.org"))
  HELO myhost.org
```

The `mailbox` datatype has the following definition:

```
 (define-datatype mailbox mailbox?
   (Mailbox (local-part string?) 
            (domain string?)))
```

## Data Types for SMTP Replies 

Representation of SMTP replies:

```scheme
  (define-datatype reply reply?
   (Reply (code code?) (msg list?)))
```

An SMTP reply is a three-digit return code plus comments. This is what
the list of strings is for; one string per line in the reply.  the
record printer will append an CRLF end-of-line marker to each entry in
that list, so that the resulting string is ready to be sent back to
the peer.


For example:

```
 > (print (Reply (Code (Success) (MailSystem) 0)
                     (list "worked" "like" "a charm")))
 250-worked
 250-like
 250 a charm
```

The `code` datatype consists of success code, category and
supplemental code:

```scheme
 (define-datatype code code?
   (Code (suc success-code?) (cat category?) (num integer?)))
```

In addition, the `success-code` and `category` datatypes can be
used to map symbolic identifiers to integers and vice versa:

```scheme
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
```

`define-enumerated-type` defines a new record type, with as many
instances as there are instance names. `name-vector` is bound to a
vector containing the instances of the type in the same order as the
`instance-name` list. `name-inject` and `name-project` are
procedures that map integers to an instance and vice versa.

## ESMTP State Machine

<procedure>(start-session)</procedure> 

Procedure `start-session` returns an ESMTP state machine object (a
procedure), which takes in a stream containing an SMTP command and
returns an appropriate `session-fsm` value:

  (define-datatype session-fsm session-fsm?
    (Event (ev event?))
    (Trans (ev event?) (fsm procedure?)))

A stream in this case is defined as the representation used by the
[[abnf]] library.

The `Event` variant signals an event that must be processed by the
calling library, while `Trans` signals an event and a state machine
transition. The following events can be returned by this state
machine:

```scheme
 (define-datatype event event?
   (SayHelo       (s string?))
   (SayHeloAgain  (s string?))
   (SayEhlo       (s string?))
   (SayEhloAgain  (s string?))
   (SetMailFrom   (m mailbox?) (parameters? list))
   (AddRcptTo     (m mailbox?) (parameters? list))
   (StartData)
   (NeedHeloFirst)
   (NeedMailFromFirst)
   (NeedRcptToFirst)
   (NotImplemented) ;; Turn, Send, Soml, Saml, Vrfy, Expn.
   (ResetState)
   (SayOK)
   (SeeksHelp    (s string?))
   (Shutdown)
   (SyntaxErrorIn (s string?))
   (Unrecognized  (s string?)))
```


## Examples

```scheme

;; An example MTA implementation

(import datatype smtp abnf)

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
	  (let ((instream (list `(() ,line))))
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
		     
```

## Version History

- 5.0-5.1 Ported to CHICKEN 5
- 3.2 Ensure test script returns proper exit status
- 3.1 Compatibility with improved CharLex->CoreABNF constructor
- 3.0 Compatibility with abnf 5
- 2.1 Small fixes in the predicates of the RcptTo and MailFrom constructors
- 2.0 Implemented typeclass interface
- 1.0 Initial release

## License

Based on the Haskell Rfc2821 module by Peter Simons.

  Copyright 2009-2019 Ivan Raikov.


  This program is free software: you can redistribute it and/or
  modify it under the terms of the GNU General Public License as
  published by the Free Software Foundation, either version 3 of the
  License, or (at your option) any later version.

  This program is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  General Public License for more details.

  A full copy of the GPL license can be found at
  <http://www.gnu.org/licenses/>.

