#!/usr/bin/env gracket
#lang racket/gui

(require
  racket/cmdline
  racket/async-channel
  setup/xref
  scribble/xref
  net/url
  "jscore/proxy.rkt"
  "main.rkt")
(define port-number 40000)
  
(command-line #:once-each (("-p" "--port") port "Port number to listen on"
                           (let ((v (string->number port)))
                             (when v
                               (set! port-number v)))))


(define listener (tcp-listen port-number 4 #t))

(define xref (load-collections-xref))
(define id-channel (make-async-channel))
(define namespace (make-base-namespace))

(define frame (new frame% (label "Documentation viewer") (height 300) (width 600)))
(define webkit (new webkit-window% (parent frame)))


(define (call-method obj method-name . args)
  (apply (js-method-ref obj method-name) args))

(define (child n x)
  (call-method (dict-ref x "childNodes") "item" n))


(void (thread (lambda ()
  (let loop ()
    (define-values (from-client to-client) (tcp-accept listener))
    (thread (lambda ()
      (parameterize ((current-namespace namespace))
        (let loop ()
          (define line (read-line from-client))
          (unless (eof-object? line)
            (async-channel-put  id-channel
             (namespace-symbol->identifier (string->symbol line)))
            (loop)))
        (close-input-port from-client)
        (close-output-port to-client))))
    (loop)))))

(define load-delegate
  (frame-load-delegate
    #:on-load
    (lambda (view frame)
     (let* ((window (send webkit js-object))
            (body (dict-ref (dict-ref window "document") "body")))
       (call-method body "removeChild"
                    (child 0 body))
       (call-method (dict-ref (child 0 body) "style")
           "setProperty" "margin-left" "0 em" "")))))
(send webkit set-frame-load-delegate! load-delegate)


(void (thread (lambda ()
  (let loop ()
    (define id (async-channel-get id-channel))
    (define tag (xref-binding->definition-tag xref id 0))
    (when tag
      (define-values (path anchor) (xref-tag->path+anchor xref tag))
      (when path
        (define base-url (string->url (path->string path)))
        (define full-url (struct-copy url base-url (fragment anchor)))
        (define full-url-string (url->string full-url))
        (send webkit load-url full-url-string)))
    (loop)))))


(send frame center)
(send frame show #t)




