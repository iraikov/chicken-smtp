
(import scheme (chicken base) (chicken format) test smtp)

(include "mta.scm")

(define rfc-D.1
  `(
    "EHLO bar.com\r\n"
    "MAIL FROM:<Smith@bar.com>\r\n"
    "RCPT TO:<Jones@foo.com>\r\n"
    "RCPT TO:<Green@foo.com>\r\n"
    "RCPT TO:<Brown@foo.com>\r\n"
    "DATA\r\n"
    "Blah blah blah...\r\n...etc. etc. etc.\r\n"
    ".\r\n"
    "QUIT\r\n"
    ))

(define ehlo-with-ipv4-address
  `(
    "EHLO [127.0.0.1]\r\n"
    "QUIT\r\n"
    ))

(test-group
 "chicken-mta"

 (test
  "rfc-D.1"
  (string-append
   "250-chicken-mta\r\n"
   "250- \r\n"
   "250-Hello \r\n"
   "250 bar.com\r\n"
   "250 OK\r\n"
   "250 Accepted\r\n"
   "250 Accepted\r\n"
   "250 Accepted\r\n"
   "354 Ready\r\n"
   "250 OK\r\n"
   "251-chicken-mta\r\n"
   "251  closing connection\r\n")
  (let ((output (open-output-string)))
    (main (open-input-string (string-concatenate rfc-D.1)) output)
    (get-output-string output)))

 (test
  "ehlo with ipv4 address"
  (string-append
   "250-chicken-mta\r\n"
   "250- \r\n"
   "250-Hello \r\n"
   "250 127.0.0.1\r\n"
   "251-chicken-mta\r\n"
   "251  closing connection\r\n")
  (let ((output (open-output-string)))
    (main (open-input-string (string-concatenate ehlo-with-ipv4-address)) output)
    (get-output-string output))))

(test-exit)
