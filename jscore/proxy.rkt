#lang racket/base

(require "ffi.rkt")
(require
  racket/port
  racket/contract
  unstable/contract
  racket/dict
  racket/match
  (except-in ffi/unsafe ->))

(provide
 (contract-out
  (_jsproxy ctype?)
  (jsproxy? predicate/c)
  (js-key? predicate/c)))



;; Scheme to JavaScript marshalling

(define (call-with-exception-protection v fun)
  (call-with-exception-handler
    (lambda (exn)
      (log-error 
        (with-output-to-string
          (lambda ()
            (displayln "Error raised in JSCore callback:")
            (displayln exn))))
      v)
    fun))

(define (call-with-exception-cell context exception thunk)
  (let/ec escape
    (call-with-exception-handler
     (λ (e)
       (ptr-set! exception _jsvalue (scheme->js context e))
       (escape #f))
     thunk)))

(define (dict-has-js-key-of-string? dict str)
  (or
    (dict-has-key? dict str)
    (dict-has-key? dict (string->symbol str)
    (dict-has-key? dict (string->number str)))))

(define (dict-ref-js-key-of-string dict str failure)
 (dict-ref dict str
  (lambda ()
   (dict-ref dict (string->symbol str)
    (lambda ()
     (dict-ref dict (string->number str)
      failure))))))

(define (js-key->string v)
  (cond
   ((string? v) v)
   ((symbol? v) (symbol->string v))
   ((number? v) (number->string v))))


(define js-key? (or/c string? symbol? number?))


(define jsclass:scheme-proxy
  (make-jsclass
   'SchemeProxy
   (vector
    #;on-initialize
    #f
    #;on-has-key
    (λ (context object key)
       (let ((object (jsobject-data object)))
         (call-with-exception-protection #f
          (lambda ()
            (if (dict? object)
                (dict-has-js-key-of-string?
                  object (jsstring->string key))
                #f)))))
    #;on-ref
    (λ (context object key exception)
      (call-with-exception-cell
       context exception
       (λ ()
         (let ((object (jsobject-data object)))
           (and (dict? object)
                (let/ec exit
                 (scheme->js context
                   (dict-ref-js-key-of-string
                     object (jsstring->string key)
                     (lambda ()
                       (exit #f))))))))))
    #;on-set
    (λ (context object key value exception)
      (call-with-exception-cell
       context exception
       (λ ()
         (let ((object (jsobject-data object)))
           (and (dict? object)
                (begin
                  (dict-set! object (jsstring->string key) (js->scheme context value))
                  #t))))))
    #;on-remove
    (λ (context object key exception)
      (call-with-exception-cell
       context exception
       (λ ()
         (let ((object (jsobject-data object)))
           (and (dict? object)
                (begin
                  (dict-remove! object (jsstring->string key))
                  #t))))))
    #;on-keys
    (λ (context object keys)
      (let ((object (jsobject-data object)))
        (call-with-exception-protection (void)
         (λ ()
           (when (dict? object)
             (for ([key (in-dict-keys object)])
               (jskey-accumulator-add! keys (js-key->string key))))))))
    #;on-apply
    (λ (context object this narguments arguments exception)
      (call-with-exception-cell
       context exception
       (λ ()
         (scheme->js
          context
          (apply (jsobject-data object)
                (build-list
                 narguments
                 (λ (i)
                   (js->scheme context (ptr-ref arguments _jsvalue i)))))))))
    #;on-make
    #f
    #;on-instanceof
    #f
    #;on-convert-to-type
    (λ (context object type exception)
      (call-with-exception-cell
       context exception
       (λ ()
         (case type
           [(string)
            (make-jsvalue-string context (format "~s" (jsobject-data object)))]
           [else
            #f])))))))


(define (scheme->js context v)
  (cond
    [(void? v)
     (make-jsvalue-undefined context)]
    [(eqv? v #\null)
     (make-jsvalue-null context)]
    [(boolean? v)
     (make-jsvalue-boolean context v)]
    [(number? v)
     (make-jsvalue-number context v)]
    [(string? v)
     (make-jsvalue-string context v)]
    [(jsproxy? v)
     (jsproxy-object v)]
    [else
     (make-jsobject context jsclass:scheme-proxy v)]))


;; JavaScript to Scheme marshalling
(define-struct jsproxy
  (context object)
  #:guard
  (λ (context object type)
    (jscontext-retain! context)
    (jsvalue-protect! context object)
    (values context object))
  #:property prop:custom-write
  (λ (proxy out mode)
    (match-let ([(jsproxy context object) proxy])
      (when mode (display "#<jsproxy:" out))
      (display (jsvalue->string context object) out)
      (when mode (display #\> out))))
  #:property prop:dict/contract
  (list 
    (vector-immutable
     #;on-ref
     (λ (proxy key [failure-result
                    (λ ()
                      (raise
                       (make-exn:fail:contract
                        (format
                         "~s: no value found for key: ~e" 'dict-ref key)
                        (current-continuation-marks))))])
       (let/ec return
         ((let/ec tail-call
            (call-with-exception-handler
             (λ (e)
               (if (procedure? failure-result)
                 (tail-call failure-result)
                 (return failure-result)))
             (λ ()
               (match-let ([(jsproxy context object) proxy])
                 (return (jsobject-ref context object key)))))))))
     #;on-set!
     (λ (proxy key value)
       (match-let ([(jsproxy context object) proxy])
         (jsobject-set! context object key (scheme->js value))))
     #;on-set
     #f
     #;on-remove!
     (λ (proxy key)
       (match-let ([(jsproxy context object) proxy])
         (jsobject-remove! context object key)))
     #;on-remove
     #f
     #;on-count
     (λ (proxy)
       (match-let ([(jsproxy context object) proxy])
         (jskey-array-length (jsobject-keys context object))))
     #;on-iterate-first
     (λ (proxy)
       (match-let ([(jsproxy context object) proxy])
         (let ([i 0] [keys (jsobject-keys context object)])
           (and (< i (jskey-array-length keys))
                (cons i keys)))))
     #;on-iterate-next
     (λ (proxy pos)
       (let ([i (add1 (car pos))] [keys (cdr pos)])
         (and (< i (jskey-array-length keys))
              (cons i keys))))
     #;on-iterate-key
     (λ (proxy pos)
       (jsstring->string (jskey-array-ref (cdr pos) (car pos))))
     #;on-iterate-value
     (λ (proxy pos)
       (match-let ([(jsproxy context object) proxy])
         (jsobject-ref context object (jskey-array-ref (cdr pos) (car pos))))))
  (vector-immutable
    js-key?
    any/c
    (cons/c exact-nonnegative-integer? jskey-array?)
    #f
    #f
    #f))
  #:property prop:procedure
  (λ (proxy #:this (this #f) . arguments)
    (match-let ([(jsproxy context object) proxy])
      (jsobject-apply context object this arguments))))

(define (js->scheme context v)
  (case (jsvalue-type context v)
    [(undefined)
     (void)]
    [(null)
     #\null]
    [(boolean)
     (jsvalue->boolean context v)]
    [(number)
     (jsvalue->number context v)]
    [(string)
     (jsvalue->string context v)]
    [(object)
     (if (jsvalue-is-a? context v jsclass:scheme-proxy)
         (jsobject-data v)
         (make-jsproxy context v))]))

(define _jsproxy
  (make-ctype _jsobject scheme->js js->scheme))

