#lang racket/base
(require
 srfi/2
 srfi/17
 srfi/26
 (rename-in racket/contract (-> c:->))
 unstable/contract
 racket/dict
 racket/match
 ffi/unsafe
 ffi/unsafe/define
 ffi/unsafe/alloc)


(provide
 (contract-out
  (_jscontext ctype?)
  (_jscontext/null ctype?)
  (jscontext? predicate/c)
  (jscontext-retain! any/c) ;TODO
  (jscontext-release! any/c) ;TODO
  (import-jscontext (c:-> cpointer? jscontext?))

  (_jsstring ctype?)
  (_jsstring/null ctype?)
  (jsstring? predicate/c)
  (jsstring-release! any/c)
  (string->jsstring (c:-> string? jsstring?))
  (jsstring->string (c:-> jsstring? string?)))

 _jsvalue
 _jsvalue/null
 jsvalue?
 _jsobject
 _jsobject/null
 jsobject?
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
 jsvalue->jsobject

 _jskey-array
 _jskey-array/null
 jskey-array?
 jskey-array-release!
 jskey-array-length
 jskey-array-ref

 jskey-accumulator-add!

 jsobject-ref 
 jsobject-set!
 jsobject-remove!
 jsobject-keys
 jsobject-apply
 
 make-jsclass
 make-jsobject
 jsobject-data
 jsvalue-is-a?)




(define-ffi-definer define/native
  (ffi-lib "/System/Library/Frameworks/WebKit.framework/WebKit"))

(struct exn:fail:js exn:fail (value) #:transparent)
(provide
 (contract-out
  (struct (exn:fail:js exn:fail) ((message string?)
                                  (continuation-marks continuation-mark-set?)
                                  (value jsvalue?)))))

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

;; Generic values and objects
(define _jstype
  (_enum '(undefined null boolean number string object)))

(define-cpointer-type _jsvalue)

(define-cpointer-type _jsobject _jsvalue)

(define/native jsvalue-unprotect!
  (_fun [context : _jscontext] [value : _jsvalue] -> _void)
  #:wrap (releaser)
  #:c-id JSValueUnprotect)

(define/native jsvalue-protect!
  (_fun [context : _jscontext] [value : _jsvalue] -> _void)
  #:wrap (retainer jsvalue-unprotect! cadr)
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
               (begin
                 (jsvalue-protect! context e)
                 (raise (exn:fail:js "jsvalue->number: failed" (current-continuation-marks) e)))
               v))
  #:c-id JSValueToBoolean)

(define/native jsvalue->string
  (_fun [context : _jscontext] [value : _jsvalue]
        [e : (_ptr io _jsvalue/null) = #f] -> [v : _jsstring/null]
        -> (if e
               (begin
                 (jsvalue-protect! context e)
                 (raise (exn:fail:js "jsvalue->string: failed" (current-continuation-marks) e)))
               (begin0
                 (jsstring->string v)
                 (jsstring-release! v))))
  #:c-id JSValueToStringCopy)

(define/native jsvalue->jsobject
  (_fun [context : _jscontext] [value : _jsvalue]
        [e : (_ptr io _jsvalue/null) = #f] -> [v : _jsobject/null]
        -> (if e
               (begin
                 (jsvalue-protect! context e)
                 (raise (exn:fail:js "jsvalue->jsobject: failed" (current-continuation-marks) e)))
               v))
  #:c-id JSValueToObject)

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
    (_fun [context : _jscontext]
          [object : _jsobject]
          -> _void)]
   [on-finalize
    _fpointer] ;Why
   [on-has-key
    (_fun [context : _jscontext]
          [object : _jsobject]
          [key : _jsstring]
          -> [? : _bool])]
   [on-ref
    (_fun [context : _jscontext]
          [object : _jsobject]
          [key : _jsstring]
          [e : _pointer]
          -> [v : _jsvalue])]
   [on-set
    (_fun [context : _jscontext]
          [object : _jsobject]
          [key : _jsstring]
          [v : _jsvalue]
          [e : _pointer]
          -> [ok? : _bool])]
   [on-remove
    (_fun [context : _jscontext]
          [object : _jsobject]
          [key : _jsstring]
          [e : _pointer]
          -> [ok? : _bool])]
   [on-keys
    (_fun [context : _jscontext]
          [object : _jsobject]
          [keys : _jskey-accumulator]
          -> _void)]
   [on-apply
    (_fun [context : _jscontext]
          [object : _jsobject]
          [this : _jsobject]
          [narguments : _uint]
          [arguments : _pointer]
          [e : _pointer]
          -> [v : _jsvalue])]
   [on-make
    (_fun [context : _jscontext]
          [object : _jsobject]
          [narguments : _uint]
          [arguments : _pointer]
          [e : _pointer]
          -> [v : _jsvalue])]
   [on-instanceof
    (_fun [context : _jscontext]
          [object : _jsobject]
          [instance : _jsobject]
          [e : _pointer]
          -> [? : _bool])]
   [on-convert-to-type
    (_fun [context : _jscontext]
          [object : _jsobject]
          [type : _jstype]
          [e : _pointer]
          -> [v : _jsvalue])]))

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
           [v : _jsvalue = (racket->js context v)]
           [attributes : (_bitmask '(read-only = 2
                                     dont-enumerate = 4
                                     dont-remove = 8))]
        [e : (_ptr io _jsvalue/null) = #f] -> _void
        -> (when e
             (jsvalue-protect! context e)
             (raise (exn:fail:js "jsobject-set!: failed" (current-continuation-marks) e))))
  #:c-id JSObjectSetProperty)

(define/native jsobject-ref
  (_fun (context object key)
        :: [context : _jscontext]
           [object : _jsobject]
           [key : _jsstring = (ensure-jskey 'jsobject-ref key)]
        [e : (_ptr io _jsvalue/null) = #f] -> [v : _jsvalue/null]
        -> (if e
               (begin
                 (jsvalue-protect! context e)
                 (raise (exn:fail:js "jsobject-ref: failed" (current-continuation-marks) e)))
               v))
  #:c-id JSObjectGetProperty)

(define/native jsobject-remove!
  (_fun (context object key)
        :: [context : _jscontext]
           [object : _jsobject]
           [key : _jsstring = (ensure-jskey 'jsobject-remove! key)]
        [e : (_ptr io _jsvalue/null) = #f] -> [ok? : _bool]
        -> (if e
               (begin
                 (jsvalue-protect! context e)
                 (raise (exn:fail:js "jsobject-remove: failed" (current-continuation-marks) e)))
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
                      = (map (cut racket->js context <>) arguments)]
        [e : (_ptr io _jsvalue/null) = #f] -> [v : _jsvalue/null]
        -> (if e
               (begin
                 (jsvalue-protect! context e)
                 (raise (exn:fail:js "jsobject-apply: failed" (current-continuation-marks) e)))
               v))
  #:c-id JSObjectCallAsFunction)

;Helper for simple conversions
(define (racket->js context v)
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
     (make-jsvalue-string context v)]))


