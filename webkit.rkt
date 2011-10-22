#lang racket/base
(require
  racket/gui/base
  ffi/unsafe/objc
  ffi/unsafe
  xml
  (for-syntax racket/base syntax/parse racket/syntax)
  racket/port
  racket/class
  mred/private/wx/cocoa/types
  mred/private/wx/cocoa/utils
  "jscore/proxy.rkt")

(provide
  frame-load-delegate
  js-resource
  xhtml-resource
  web-archive
  webkit-window%)

(define webkit-lib (ffi-lib (format "/System/Library/Frameworks/WebKit.framework/WebKit"))) ;ensure that bindings are available

(import-class
  WebArchive
  WebView
  WebResource
  NSURLRequest
  NSURL
  NSData
  NSArray)

(define (bytes->NSData bytes)
  (tell NSData dataWithBytes: #:type _pointer bytes length: #:type _NSUInteger (bytes-length bytes)))

(define (make-WebResource data url mime)
  (as-objc-allocation
    (tell (tell WebResource alloc)
      initWithData: data
      URL: (tell NSURL URLWithString: #:type _NSString url)
      MIMEType: #:type _NSString mime
      textEncodingName: #:type _NSString "utf-8"
      frameName: #f)))

(define (js-resource js url)
  (make-WebResource
    (bytes->NSData (string->bytes/utf-8 js))
    url
    "text/javascript"))

(define (xhtml-resource xhtml url)
  (define xml-bytes
    (cond
      ((bytes? xhtml) xhtml)
      ((string? xhtml) (string->bytes/utf-8 xhtml))
      ((xexpr? xhtml) (with-output-to-bytes (lambda () (write-xexpr xhtml))))
      (else (error 'xhtml-resource "Unknown data type: ~a" xhtml))))
  (make-WebResource (bytes->NSData xml-bytes) url "application/xhtml+xml"))

(define (web-archive main sub-resources)
  (as-objc-allocation
    (tell (tell WebArchive alloc)
      initWithMainResource: main
      subresources: (for/fold ((array (tell NSArray array)))
                              ((resource sub-resources))
                     (tell array arrayByAddingObject: resource))
      subframeArchives: #f)))

(define _WebView _id)
(define _WebScriptObject _id)
(define _WebDataSource _id)
(define _WebFrame _id)
(define _NSImage _id)
(define _NSError _id)
(define _NSURL _id)
(define _NSURLRequest _id)
(define _NSURLResponse _id)
(define _NSURLAuthenticationChallenge _id)
(define _NSDate _id)
(define _NSTimeInterval _double)


(define (maybe-call f . args)
  (when f (apply f args)))

(define-syntax (define-delegate stx)
 (define-syntax-class argument
  #:attributes ((signature 1) (values 1))
  (pattern (v:id) #:with (signature ...) #'(v) #:with (values ...) '())
  (pattern ((~seq v:id type:expr) ...)
           #:with (values ...) (generate-temporaries #'(v ...))
           #:with ((sig-part ...) ...) #'((v (type values)) ...)
           #:with (signature ...) #'(sig-part ... ...)))

 (syntax-parse stx
  ((_ delegate-name:id constructor
      (local-variable return-type:expr args:argument) ...)
   (define/with-syntax (keyword ...)
      (map (compose string->keyword symbol->string syntax-e)
           (syntax->list #'(local-variable ...))))
   (define/with-syntax ((constructor-args ...) ...)
      #'((keyword (local-variable #f)) ...))
   #'(begin
       (define (constructor constructor-args ... ...)
         (define instance (as-objc-allocation (tell (tell delegate-name alloc) init)))
         (set-ivar! instance local-variable local-variable) ...
         instance)
       (define-objc-class delegate-name NSObject
         [local-variable ...]
         (- return-type (args.signature ...)
            (maybe-call local-variable args.values ...)) ...)))))


(define-delegate RacketWebFrameLoadDelegate frame-load-delegate
 ;Loading Messages
 (on-load _void (webView: _WebView didFinishLoadForFrame: _WebFrame))
 (on-provisional-load _void (webView: _WebView didStartProvisionalLoadFor: _WebFrame))
 (on-commit-load _void (webView: _WebView didCommitLoadFor: _WebFrame))
 (on-close _void (webView: _WebView willCloseFrame: _WebFrame))
 (on-location-change _void (webView: _WebView didChangeLocationWithinPageForFrame: _WebFrame))
 ;Data Received Messages
 (on-receive-title _void (webView: _WebView didReceiveTitle: _NSString forFrame: _WebFrame))
 (on-receive-icon _void (webView: _WebView didReceiveIcon: _NSImage forFrame: _WebFrame))
 ;Error Messages
 (on-provisional-load-fail _void (webView: _WebView didFailProvisionalLoadWithError: _NSError forFrame: _WebFrame))
 (on-load-fail _void (webView: _WebView didFailLoadWithError: _NSError forFrame: _WebFrame))
 ;Client and Server Redirect Messages
 (on-cancel-redirect _void (webView: _WebView didCancelClientRedirectForFrame: _WebFrame))
 (on-client-redirect _void
          (webView: _WebView
           willPerformClientRedirectToURL: _NSURL
           delay: _NSTimeInterval
           fireDate: _NSDate
           forFrame: _WebFrame))
 (on-server-redirect _void (webView: _WebView didReceiveServerRedirectForProvisionalLoadForFrame: _WebFrame))
 ;WebScript Messages
 (on-clear-window-object _void (webView: _WebView didClearWindowObject: _WebScriptObject forFrame: _WebFrame)))

(define _resource _id)
(define-delegate RacketWebResourceLoadDelegate resource-load-delegate
 (get-identifier _resource (webView: _WebView identifierForInitialRequest: _NSURLRequest fromDataSource: _WebDataSource))
 (on-request _NSURLRequest (webView: _WebView resource: _resource
                            willSendRequest: _NSURLRequest redirectResponse: _NSURLResponse fromDataSource: _WebDataSource))
 (on-load _void (webView: _WebView resource: _resource didFinishLoadingFromDataSource: _WebDataSource))
 (on-response _void (webView: _WebView resource: _resource didReceiveResponse: _NSURLResponse fromDataSource: _WebDataSource))
 (on-data _void (webView: _WebView resource: _resource didReceiveContentLength: _NSUInteger fromDataSource: _WebDataSource))
 (on-load-fail _void (webView: _WebView resource: _resource didFailLoadingWithError: _NSError fromDataSource: _WebDataSource))
 (on-plugin-fail _void (webView: _WebView plugInFailedWithError: _NSError dataSource: _WebDataSource))
 (on-authentication-challenge _void (webView: _WebView resource: _resource
                                    didReceiveAuthenticationChallenge: _NSURLAuthenticationChallenge fromDataSource: _WebDataSource))
 (on-authentication-challenge-cancel _void (webView: _WebView resource: _resource
                                           didCancelAuthenticationChallenge: _NSURLAuthenticationChallenge fromDataSource: _WebDataSource)))


(define webkit-window%
  (class panel%
    (super-new)

    (define cocoa
      (as-objc-allocation (tell (tell WebView alloc) initWithFrame: #:type _NSRect (make-NSRect (make-NSPoint 0 0) (make-NSSize 300 300)) frameName: #f groupName: #f)))
    (tellv (send this get-client-handle) addSubview: cocoa) 

    (define/override (on-size w h)
      (tellv cocoa setFrame:
             #:type _NSRect (make-NSRect (make-NSPoint 0 0) 
                                         (make-NSSize w h))))

    (define/public (set-frame-load-delegate! load-delegate)
      (tellv cocoa setFrameLoadDelegate: load-delegate))

    (define/public (js-object)
      (let* ((main-frame (tell cocoa mainFrame))
             (context (tell #:type _jscontext main-frame globalContext))
             (window-object (tell #:type (_jsproxy context) (tell main-frame windowObject) JSObject)))
        window-object))

    (define/public (load-url s)
     (tellv (tell cocoa mainFrame) loadRequest: 
      (tell NSURLRequest requestWithURL: (tell NSURL URLWithString: #:type _NSString s))))
    
    (define/public (load-xhtml-xexpr expr)
      (define data (bytes->NSData (with-output-to-bytes (lambda () (write-xexpr expr)))))
      (tellv (tell cocoa mainFrame)
             loadData: data
             MIMEType: #:type _NSString "application/xhtml+xml"
             textEncodingName: #:type _NSString "utf-8"
             baseURL: #f))

    (define/public (load-archive archive)
      (tellv (tell cocoa mainFrame) loadArchive: archive))))
 
