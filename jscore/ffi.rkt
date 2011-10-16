#lang racket/base
(require
 srfi/2
 srfi/17
 srfi/26
 racket/dict
 racket/match
 ffi/unsafe
 ffi/unsafe/define
 ffi/unsafe/alloc)

(define-ffi-definer define/native
  (ffi-lib "libwebkit-1.0" '("2" #f)))

;; Contexts
(define-cpointer-type _jscontext)

(define/native jscontext-release!
  (_fun [context : _jscontext] -> _void)
  #:wrap (releaser)
  #:c-id JSGlobalContextRelease)

(define/native jscontext-retain!
  (_fun [context : _jscontext] -> _void)
  #:wrap (retainer jscontext-release!)
  #:c-id JSGlobalContextRetain)

(define (import-jscontext v)
  (cast v _pointer _jscontext))

(provide
  _jscontext _jscontext/null jscontext?
  jscontext-retain! jscontext-release!
  import-jscontext)

;; Strings
(define-cpointer-type _jsstring)

(define/native jsstring-release!
  (_fun [string : _jsstring] -> _void)
  #:wrap (releaser)
  #:c-id JSStringRelease)

(define/native string->jsstring
  (_fun [data : _string/utf-8] -> [string : _jsstring])
  #:wrap (allocator jsstring-release!)
  #:c-id JSStringCreateWithUTF8CString)

(define/native jsstring-size/utf-8
  (_fun [string : _jsstring] -> [size : _ulong])
  #:c-id JSStringGetMaximumUTF8CStringSize)

(define/native jsstring->string
  (_fun (string)
        :: [string : _jsstring]
           [data : _bytes = (make-bytes (jsstring-size/utf-8 string))]
           [size : _ulong = (bytes-length data)]
        -> [size : _ulong]
        -> (bytes->string/utf-8 data #f 0 (sub1 size)))
  #:c-id JSStringGetUTF8CString)

(provide
 _jsstring _jsstring/null jsstring?
 jsstring-release!
 string->jsstring jsstring->string)

;; Generic values and objects
(define _jstype
  (_enum '(undefined null boolean number string object)))

(define-cpointer-type _jsvalue)

(define-values (_jsobject _jsobject/null jsobject?)
  (values _jsvalue _jsvalue/null jsvalue?))

(define/native jsvalue-unprotect!
  (_fun [context : _jscontext] [value : _jsvalue] -> _void)
  #:wrap (releaser)
  #:c-id JSValueUnprotect)

(define/native jsvalue-protect!
  (_fun [context : _jscontext] [value : _jsvalue] -> _void)
  #:wrap (retainer jsvalue-unprotect!)
  #:c-id JSValueProtect)

(define (import-jsvalue v)
  (cast v _pointer _jsvalue))

(define/native make-jsvalue-undefined
  (_fun [context : _jscontext] -> [value : _jsvalue])
  #:c-id JSValueMakeUndefined)

(define/native make-jsvalue-null
  (_fun [context : _jscontext] -> [value : _jsvalue])
  #:c-id JSValueMakeNull)

(define/native make-jsvalue-boolean
  (_fun [context : _jscontext] [v : _bool] -> [value : _jsvalue])
  #:c-id JSValueMakeBoolean)

(define/native make-jsvalue-number
  (_fun [context : _jscontext] [v : _double*] -> [value : _jsvalue])
  #:c-id JSValueMakeNumber)

(define/native make-jsvalue-string
  (_fun (context v)
        :: [context : _jscontext] [v : _jsstring = (string->jsstring v)]
        -> [value : _jsvalue])
  #:c-id JSValueMakeString)

(define/native jsvalue-type
  (_fun [context : _jscontext] [value : _jsvalue] -> _jstype)
  #:c-id JSValueGetType)

(define/native jsvalue->boolean
  (_fun [context : _jscontext] [value : _jsvalue] -> [v : _bool])
  #:c-id JSValueToBoolean)

(define/native jsvalue->number
  (_fun [context : _jscontext] [value : _jsvalue]
        [e : (_ptr io _jsvalue/null) = #f] -> [v : _double]
        -> (if e
               (raise (js->scheme context e))
               v))
  #:c-id JSValueToBoolean)

(define/native jsvalue->string
  (_fun [context : _jscontext] [value : _jsvalue]
        [e : (_ptr io _jsvalue/null) = #f] -> [v : _jsstring/null]
        -> (if e
               (raise (js->scheme context e))
               (begin0
                 (jsstring->string v)
                 (jsstring-release! v))))
  #:c-id JSValueToStringCopy)

(define/native jsvalue->jsobject
  (_fun [context : _jscontext] [value : _jsvalue]
        [e : (_ptr io _jsvalue/null) = #f] -> [v : _jsobject/null]
        -> (if e
               (raise (js->scheme context e))
               v))
  #:c-id JSValueToObject)

(provide
 _jsvalue _jsvalue/null jsvalue?
 _jsobject _jsobject/null jsobject?
 jsvalue-protect!
 jsvalue-unprotect!
 import-jsvalue
 make-jsvalue-undefined
 make-jsvalue-null
 make-jsvalue-boolean
 make-jsvalue-number
 make-jsvalue-string
 jsvalue-type
 jsvalue->boolean
 jsvalue->number
 jsvalue->string
 jsvalue->jsobject)

;; Property name arrays
(define-cpointer-type _jskey-array)

(define/native jskey-array-release!
  (_fun [array : _jskey-array] -> _void)
  #:wrap (releaser)
  #:c-id JSPropertyNameArrayRelease)

(define/native jskey-array-length
  (_fun [array : _jskey-array] -> [n : _uint])
  #:c-id JSPropertyNameArrayGetCount)

(define/native jskey-array-ref
  (_fun [array : _jskey-array] [i : _uint] -> [key : _jsstring])
  #:c-id JSPropertyNameArrayGetNameAtIndex)

(provide
 _jskey-array _jskey-array/null jskey-array?
 jskey-array-release!
 jskey-array-length
 jskey-array-ref)

;; Property name accumulators
(define-cpointer-type _jskey-accumulator)

(define/native jskey-accumulator-add!
  (_fun (accumulator key)
        :: [accumulator : _jskey-accumulator]
           [string : _jsstring = (string->jsstring key)]
        -> _void)
  #:c-id JSPropertyNameAccumulatorAddName)

;; Classes
(define-cpointer-type _jsclass)

(define/native jsclass-release!
  (_fun [class : _jsclass] -> _void)
  #:wrap (releaser)
  #:c-id JSClassRelease)

(define-cstruct _jsclass-info
  ([version _int]
   [attributes (_bitmask '(no-automatic-prototype = 2))]
   [name _symbol]
   [parent _jsclass/null]
   [static-values _pointer]
   [static-functions _pointer]
   [on-initialize
    (_fun [context : _jscontext] [object : _jsobject]
          -> _void)]
   [on-finalize
    _fpointer]
   [on-has-key
    (_fun [context : _jscontext] [object : _jsobject]
          [key : _jsstring]
          -> [? : _bool])]
   [on-ref
    (_fun [context : _jscontext] [object : _jsobject]
          [key : _jsstring]
          [e : _pointer] -> [v : _jsvalue])]
   [on-set
    (_fun [context : _jscontext] [object : _jsobject]
          [key : _jsstring] [v : _jsvalue]
          [e : _pointer] -> [ok? : _bool])]
   [on-remove
    (_fun [context : _jscontext] [object : _jsobject]
          [key : _jsstring]
          [e : _pointer] -> [ok? : _bool])]
   [on-keys
    (_fun [context : _jscontext] [object : _jsobject]
          [keys : _jskey-accumulator] -> _void)]
   [on-apply
    (_fun [context : _jscontext] [object : _jsobject] [this : _jsobject]
          [narguments : _uint] [arguments : _pointer]
          [e : _pointer] -> [v : _jsvalue])]
   [on-make
    (_fun [context : _jscontext] [object : _jsobject]
          [narguments : _uint] [arguments : _pointer]
          [e : _pointer] -> [v : _jsvalue])]
   [on-instanceof
    (_fun [context : _jscontext] [object : _jsobject] [instance : _jsobject]
          [e : _pointer] -> [? : _bool])]
   [on-convert-to-type
    (_fun [context : _jscontext] [object : _jsobject] [type : _jstype]
          [e : _pointer] -> [v : _jsvalue])]))

(define/native jsclass-info:empty
  _jsclass-info
  #:c-id kJSClassDefinitionEmpty)

(define/native jsobject-data-cell
  (_fun [object : _jsobject] -> [data : _pointer])
  #:c-id JSObjectGetPrivate)

(define (jsobject-scheme-finalize! object)
  (free-immobile-cell (jsobject-data-cell object)))

(define/native make-jsclass
  (let ([on-finalize (cast
                      jsobject-scheme-finalize!
                      (_fun [object : _jsobject] -> _void) _fpointer)])
    (_fun (name callbacks)
          :: [class-info : _jsclass-info-pointer
                         = (let ([class-info (cast
                                              (malloc _jsclass-info)
                                              _pointer _jsclass-info-pointer)])
                             (memcpy
                              class-info jsclass-info:empty
                              1 _jsclass-info)
                             (set-jsclass-info-name! class-info
                               name)
                             (set-jsclass-info-on-finalize! class-info
                               on-finalize)
                             (cond
                               [(vector-ref callbacks 0)
                                => (cut set-jsclass-info-on-initialize!
                                        class-info <>)])
                             (cond
                               [(vector-ref callbacks 1)
                                => (cut set-jsclass-info-on-has-key!
                                        class-info <>)])
                             (cond
                               [(vector-ref callbacks 2)
                                => (cut set-jsclass-info-on-ref!
                                        class-info <>)])
                             (cond
                               [(vector-ref callbacks 3)
                                => (cut set-jsclass-info-on-set!
                                        class-info <>)])
                             (cond
                               [(vector-ref callbacks 4)
                                => (cut set-jsclass-info-on-remove!
                                        class-info <>)])
                             (cond
                               [(vector-ref callbacks 5)
                                => (cut set-jsclass-info-on-keys!
                                        class-info <>)])
                             (cond
                               [(vector-ref callbacks 6)
                                => (cut set-jsclass-info-on-apply!
                                        class-info <>)])
                             (cond
                               [(vector-ref callbacks 7)
                                => (cut set-jsclass-info-on-make!
                                        class-info <>)])
                             (cond
                               [(vector-ref callbacks 8)
                                => (cut set-jsclass-info-on-instanceof!
                                        class-info <>)])
                             (cond
                               [(vector-ref callbacks 9)
                                => (cut set-jsclass-info-on-convert-to-type!
                                        class-info <>)])
                             class-info)]
          -> [class : _jsclass]))
  #:wrap (allocator jsclass-release!)
  #:c-id JSClassCreate)
  
(define/native make-jsobject
  (_fun (context class data)
        :: [context : _jscontext] [class : _jsclass]
           [data : _pointer = (malloc-immobile-cell data)]
        -> [object : _jsobject])
  #:c-id JSObjectMake)

(define (jsobject-data object)
  (ptr-ref (jsobject-data-cell object) _scheme))

(define/native jsvalue-is-a?
  (_fun [context : _jscontext] [value : _jsvalue] [class : _jsclass]
        -> [? : _bool])
  #:c-id JSValueIsObjectOfClass)

;; JavaScript property and function access
(define (ensure-jskey name v)
  (cond
    [(jsstring? v)
     v]
    [(string? v)
     (string->jsstring v)]
    [(symbol? v)
     (string->jsstring (symbol->string v))]
    [(number? v)
     (string->jsstring (number->string v))]
    [else
     (raise-type-error name "jsstring, string, symbol or number" v)]))

(define/native jsobject-set!
  (_fun (context object key v [attributes null])
        :: [context : _jscontext]
           [object : _jsobject]
           [key : _jsstring = (ensure-jskey 'jsobject-set! key)]
           [v : _jsvalue = (scheme->js context v)]
           [attributes : (_bitmask '(read-only = 2
                                     dont-enumerate = 4
                                     dont-remove = 8))]
        [e : (_ptr io _jsvalue/null) = #f] -> _void
        -> (when e
             (raise (js->scheme context e))))
  #:c-id JSObjectSetProperty)

(define/native jsobject-ref
  (_fun (context object key)
        :: [context : _jscontext]
           [object : _jsobject]
           [key : _jsstring = (ensure-jskey 'jsobject-ref key)]
        [e : (_ptr io _jsvalue/null) = #f] -> [v : _jsvalue/null]
        -> (if e
               (raise (js->scheme context e))
               (js->scheme context v object)))
  #:wrap (cut getter-with-setter <> jsobject-set!)
  #:c-id JSObjectGetProperty)

(define/native jsobject-remove!
  (_fun (context object key)
        :: [context : _jscontext]
           [object : _jsobject]
           [key : _jsstring = (ensure-jskey 'jsobject-remove! key)]
        [e : (_ptr io _jsvalue/null) = #f] -> [ok? : _bool]
        -> (if e
               (raise (js->scheme context e))
               ok?))
  #:c-id JSObjectDeleteProperty)

(define/native jsobject-keys
  (_fun [context : _jscontext] [object : _jsobject] -> [keys : _jskey-array])
  #:wrap (allocator jskey-array-release!)
  #:c-id JSObjectCopyPropertyNames)

(define/native jsobject-apply
  (_fun (context object this arguments)
        :: [context : _jscontext]
           [object : _jsobject] [this : _jsobject/null]
           [narguments : _uint
                       = (length arguments)]
           [arguments : (_list i _jsvalue)
                      = (map (cut scheme->js context <>) arguments)]
        [e : (_ptr io _jsvalue/null) = #f] -> [v : _jsvalue/null]
        -> (if e
               (raise (js->scheme context e))
               v))
  #:c-id JSObjectCallAsFunction)

(provide
 jsobject-ref jsobject-set!
 jsobject-remove!
 jsobject-keys
 jsobject-apply)

;; Scheme to JavaScript marshalling
(define (call-with-exception-value value thunk)
  (let/ec escape
    (call-with-exception-handler
     (Î» (e)
       (escape (if (procedure? value) (value) value)))
     thunk)))

(define (call-with-exception-cell context exception thunk)
  (let/ec escape
    (ptr-set! exception _jsvalue/null #f)
    (call-with-exception-handler
     (Î» (e)
       (ptr-set! exception _jsvalue/null (scheme->js context e))
       (escape #f))
     thunk)))

(define (ensure-dict+key name dict v)
  (cond
    [(and (string? v)
          (or (and (dict-has-key? dict v) v)
              (and-let* ([v (string->symbol v)] [(dict-has-key? dict v)]) v)
              (and-let* ([v (string->number v)] [(dict-has-key? dict v)]) v)))
     => (cut values dict <>)]
    [else
     (raise-type-error
      name "string matching string, symbol or number key" 1 dict v)]))

(define jsclass:scheme-proxy
  (make-jsclass
   'SchemeProxy
   (vector
    #;on-initialize
    #f
    #;on-has-key
    (Î» (context object key)
      (call-with-exception-value
       #f
       (Î» ()
         (ensure-dict+key 'dict-has-key?
                          (jsobject-data object) (jsstring->string key))
         #t)))
    #;on-ref
    (Î» (context object key exception)
      (call-with-exception-cell
       context exception
       (Î» ()
         (scheme->js
          context
          (call-with-values
           (cut ensure-dict+key 'dict-ref
                (jsobject-data object) (jsstring->string key))
           dict-ref)))))
    #;on-set
    (Î» (context object key value exception)
      (call-with-exception-cell
       context exception
       (Î» ()
         (call-with-values
          (cut ensure-dict+key 'dict-set!
               (jsobject-data object) (jsstring->string key))
          (cut dict-set! <> <> (js->scheme context value)))
         #t)))
    #;on-remove
    (Î» (context object key exception)
      (call-with-exception-cell
       context exception
       (Î» ()
         (call-with-values
          (cut ensure-dict+key 'dict-remove!
               (jsobject-data object) (jsstring->string key))
          dict-remove!)
         #t)))
    #;on-keys
    (Î» (context object keys)
      (call-with-exception-value
       void
       (Î» ()
         (for ([key (in-dict-keys (jsobject-data object))])
           (jskey-accumulator-add! keys key)))))
    #;on-apply
    (Î» (context object this narguments arguments exception)
      (call-with-exception-cell
       context exception
       (Î» ()
         (scheme->js
          context
          (apply (jsobject-data object)
                (build-list
                 narguments
                 (Î» (i)
                   (js->scheme context (ptr-ref arguments _jsvalue i)))))))))
    #;on-make
    #f
    #;on-instanceof
    #f
    #;on-convert-to-type
    (Î» (context object type exception)
      (call-with-exception-cell
       context exception
       (Î» ()
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

(provide
 scheme->js)

;; JavaScript to Scheme marshalling
(define-struct jsproxy
  (context
   object this)
  #:guard
  (Î» (context object this type)
    (jscontext-retain! context)
    (jsvalue-protect! context object)
    (when this
      (jsvalue-protect! context this))
    (values context object this))
  #:property prop:custom-write
  (Î» (proxy out mode)
    (match-let ([(jsproxy context object _) proxy])
      (when mode (display "#<jsproxy:" out))
      (display (jsvalue->string context object) out)
      (when mode (display #\> out))))
  #:property prop:dict
  (vector
   #;on-ref
   (Î» (proxy key [failure-result
                  (Î» ()
                    (raise
                     (make-exn:fail:contract
                      (format
                       "~s: no value found for key: ~e" 'dict-ref key)
                      (current-continuation-marks))))])
     (let/ec escape
       (call-with-exception-handler
        (Î» (e)
          (escape
           (if (procedure? failure-result) (failure-result) failure-result)))
        (Î» ()
          (match-let ([(jsproxy context object _) proxy])
            (jsobject-ref context object key))))))
   #;on-set!
   (Î» (proxy key value)
     (match-let ([(jsproxy context object _) proxy])
       (jsobject-set! context object key value)))
   #;on-set
   #f
   #;on-remove!
   (Î» (proxy key)
     (match-let ([(jsproxy context object _) proxy])
       (jsobject-remove! context object key)))
   #;on-remove
   #f
   #;on-count
   (Î» (proxy)
     (match-let ([(jsproxy context object _) proxy])
       (jskey-array-length (jsobject-keys context object))))
   #;on-iterate-first
   (Î» (proxy)
     (match-let ([(jsproxy context object _) proxy])
       (let ([i 0] [keys (jsobject-keys context object)])
         (and (< i (jskey-array-length keys))
              (cons i keys)))))
   #;on-iterate-next
   (Î» (proxy pos)
     (let ([i (add1 (car pos))] [keys (cdr pos)])
       (and (< i (jskey-array-length keys))
            (cons i keys))))
   #;on-iterate-key
   (Î» (proxy pos)
     (jskey-array-ref (cdr pos) (car pos)))
   #;on-iterate-value
   (Î» (proxy pos)
     (match-let ([(jsproxy context object _) proxy])
       (jsobject-ref context object (jskey-array-ref (cdr pos) (car pos))))))
  #:property prop:procedure
  (Î» (proxy . arguments)
    (match-let ([(jsproxy context object this) proxy])
      (jsobject-apply context object this arguments))))

(define (js->scheme context v [this #f])
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
         (make-jsproxy context v this))]))

(provide
 jsproxy?
 js->scheme)
