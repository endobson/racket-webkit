#lang racket/gui

(require "main.rkt")



(define frame0 (new frame% (label "test-frame") (width 900) (height 700) (x 0) (y 0)))
(define frame1 (new frame% (label "test-frame") (width 300) (height 300) (x 900) (y 0)))
(define frame2 (new frame% (label "test-frame") (width 300) (height 300) (x 900) (y 300)))

(define webkit0
  (new webkit-window%
       [parent frame0]))
(define webkit1
  (new webkit-window%
       [parent frame1]))
(define webkit2
  (new webkit-window%
       [parent frame2]))

(define page1-xexpr
 '(xhtml ((xmlns "http://www.w3.org/1999/xhtml"))
   (head
     (script ((type "text/javascript"))

       "function addDiv() {
             newDiv = document.createElement('div');
             newDiv.innerHTML = 'Click the link';
             document.body.appendChild(newDiv)
        }"))
   (body 
     (p (b "Hello World"))
     (p (a ((onclick "addDiv()")) "Click me"))
     (a ((href "http://www.google.com")) "Google"))))



(define page2-xexpr
 '(xhtml ((xmlns "http://www.w3.org/1999/xhtml"))
   (head
     (script ((src "page2.js")
              (type "text/javascript"))))
   (body ((onload "onload()"))
     (p "This uses a separate javascript file")
     (p (a ((onclick "addDiv()")) "Click me")))))

(define page2-js
       "function addDiv() {
             newDiv = document.createElement('div');
             newDiv.innerHTML = 'This is a new Div';
             document.body.appendChild(newDiv)
        }")



(define archive
  (web-archive
   (xhtml-resource page2-xexpr "/page2.html")
   (list
     (js-resource page2-js "/page2.js"))))



(define page2-resource (xhtml-resource page2-xexpr "/page2.html"))

(send webkit0 load-url "http://docs.racket-lang.org")
(send webkit1 load-xhtml-xexpr page1-xexpr)
(send webkit2 load-archive archive)
(send frame2 show #t)
(send frame1 show #t)
(send frame0 show #t)

