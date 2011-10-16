#lang racket/gui
(require
  racket/gui/base
  ffi/unsafe/objc
  ffi/unsafe
  xml
  mred/private/wx/cocoa/types
  mred/private/wx/cocoa/utils)

(provide
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
 
