;;; Ypsilon Scheme System
;;; Copyright (c) 2004-2008 Y.FUJITA, LittleWing Company Limited.
;;; See license.txt for terms and conditions of use.

#|

  Scheme implementation of "Strictly Pretty, Christian Lindig, March 2000 (http://www.st.cs.uni-sb.de/~lindig)"

  * OCaml code from paper

      let (^^) x y    = DocCons(x,y)
      let empty       = DocNil
      let text s      = DocText(s)
      let nest i x    = DocNest(i,x)
      let break       = DocBreak(" ")
      let breakWith s = DocBreak(s)
      let group d     = DocGroup(d)

      let rec sdocToString = function
          | SNil -> ""
          | SText(s,d) -> s ^ sdocToString d
          | SLine(i,d) -> let prefix = String.make i ’ ’
                          in nl ^ prefix ^ sdocToString d

      let rec fits w = function
          | _ when w < 0               -> false
          | []                         -> true
          | (i,m,DocNil)          :: z -> fits w z
          | (i,m,DocCons(x,y))    :: z -> fits w ((i,m,x)::(i,m,y)::z)
          | (i,m,DocNest(j,x))    :: z -> fits w ((i+j,m,x)::z)
          | (i,m,DocText(s))      :: z -> fits (w - strlen s) z
          | (i,Flat, DocBreak(s)) :: z -> fits (w - strlen s) z
          | (i,Break,DocBreak(_)) :: z -> true (* impossible *)
          | (i,m,DocGroup(x))     :: z -> fits w ((i,Flat,x)::z)

      let rec format w k = function
          | [] -> SNil
          | (i,m,DocNil)          :: z -> format w k z
          | (i,m,DocCons(x,y))    :: z -> format w k ((i,m,x)::(i,m,y)::z)
          | (i,m,DocNest(j,x))    :: z -> format w k ((i+j,m,x)::z)
          | (i,m,DocText(s))      :: z -> SText(s,format w (k + strlen s) z)
          | (i,Flat, DocBreak(s)) :: z -> SText(s,format w (k + strlen s) z)
          | (i,Break,DocBreak(s)) :: z -> SLine(i,format w i z)
          | (i,m,DocGroup(x))     :: z -> if fits (w-k) ((i,Flat,x)::z)
                                          then format w k ((i,Flat ,x)::z)
                                          else format w k ((i,Break,x)::z)
  scheme implementation note:

    Flat           -->  .&FLAT
    Break          -->  .&BREAK
    DocCons(x,y)   -->  (x . y)
    DocNil         -->  ()
    DocText(s)     -->  "s"
    DocNest(i,x)   -->  (.&NEST i x ...)
    DocBreak(" ")  -->  #\;
    DocBreak(s)    -->  #\;
    DocGroup(x)    -->  (.&GROUP x ...)

|#

(define pretty-print-line-length (make-parameter 100))
(define pretty-print-initial-indent (make-parameter 0))
(define pretty-print-maximum-lines (make-parameter #f))
(define pretty-print-unwrap-syntax (make-parameter #f))

(define pretty-print
  (lambda (expr . port)
    (let ((port (if (pair? port) (car port) (current-output-port)))
          (n-more-lines (and (pretty-print-maximum-lines) (- (pretty-print-maximum-lines) 1))))

      (define indent-type1?
        (lambda (id)
          (memq id '(library define define-syntax define-macro define-inline define-constant
                      syntax-rules syntax-case
                      with-syntax lambda let-syntax letrec-syntax
                      let letrec let* letrec letrec* let-values let*-values
                      destructuring-match parameterize))))

      (define indent-type2?
        (lambda (id)
          (memq id '(if cond case and or set! import export cons map for-each exists for-all))))

      (define indent-type3?
        (lambda (id)
          (memq id '(do let-optionals))))

      (define fits?
        (lambda (w lst)
          (and (>= w 0)
               (or (null? lst)
                   (destructuring-match lst
                     (((_ _ ()) . z) (fits? w z))
                     (((_ '.&BREAK #\;) . z) #t)
                     (((_ '.&FLAT #\;) . z) (fits? (- w 1) z))
                     (((_ _ (? string? s)) . z) (fits? (- w (string-length s)) z))
                     (((i _ ('.&GROUP . x)) . z) (fits? w `((,i .&FLAT ,x) ,@z)))
                     (((i m ('.&NEST j . x)) . z) (fits? w `((,(+ i j) ,m ,x) ,@z)))
                     (((i m (x . y)) . z) (fits? w `((,i ,m ,x) (,i ,m ,y) ,@z))))))))

      (define print
        (lambda (w k lst)
          (or (null? lst)
              (destructuring-match lst
                (((_ _ ()) . z) (print w k z))
                (((i '.&BREAK #\;) . z)
                 (cond ((or (eq? n-more-lines #f) (> n-more-lines 0))
                        (and n-more-lines (set! n-more-lines (- n-more-lines 1)))
                        (put-char port #\newline)
                        (let loop ((i i)) (and (> i 0) (put-char port #\space)  (loop (- i 1))))
                        (print w i z))))
                (((_ '.&FLAT #\;) . z)
                 (begin
                   (put-char port #\space)
                   (print w (+ k 1) z)))
                (((_ _ (? string? s)) . z)
                 (begin
                   (put-string port s)
                   (print w (+ k (string-length s)) z)))
                (((i _ ('.&GROUP . x)) . z)
                 (let ((flat `((,i .&FLAT ,x) ,@z)))
                   (if (fits? (- w k) flat)
                       (print w k flat)
                       (print w k `((,i .&BREAK ,x) ,@z)))))
                (((i m ('.&NEST j . x)) . z)
                 (print w k `((,(+ i j) ,m ,x) ,@z)))
                (((i m (x . y)) . z)
                 (print w k `((,i ,m ,x) (,i ,m ,y) ,@z)))))))

      (define symbol->length
        (lambda (obj)
          (string-length (symbol->string obj))))

      (define parse-list
        (lambda (lst)
          (cond ((null? lst) '())
                ((null? (cdr lst))
                 (list (parse (car lst))))
                ((pair? (cdr lst))
                 (cons* (parse (car lst)) #\; (parse-list (cdr lst))))
                (else
                 (list (parse (car lst)) #\; "." #\; (parse (cdr lst)))))))

      (define parse
        (lambda (obj)
          (cond ((pair? obj)
                 (destructuring-match obj
                   (('quote e) `("'" (.&NEST 1 ,(parse e))))
                   (('unquote e) `("," (.&NEST 1 ,(parse e))))
                   (('quasiquote e) `("`" (.&NEST 1 ,(parse e))))
                   (('unquote-splicing e) `(",@" (.&NEST 2 ,(parse e))))
                   (('let (? symbol? e1) e2 . (? pair? e3))
                    `(.&GROUP ,(format "(let ~a " e1)
                              (.&NEST 2
                                      (.&NEST ,(+ (symbol->length e1) 4)
                                              ,(parse e2))
                                      #\;
                                      ,@(parse-list e3) ")")))
                   (((? indent-type1? e1) e2 . (? pair? e3))
                    `(.&GROUP ,(format "(~a " e1)
                              (.&NEST 2
                                      (.&NEST ,(symbol->length e1)
                                              ,(parse e2))
                                      #\;
                                      ,@(parse-list e3) ")")))
                   (((? indent-type2? e1) e2 . (? pair? e3))
                    `(.&GROUP ,(format "(~a " e1)
                              (.&NEST ,(+ (symbol->length e1) 2)
                                      ,(parse e2) #\; ,@(parse-list e3)) ")"))
                   (((? indent-type3? e1) e2 e3 . (? pair? e4))
                    `(.&GROUP ,(format "(~a " e1)
                              (.&NEST 2
                                      (.&NEST 2
                                              ,(parse e2) #\; ,(parse e3))
                                      #\;
                                      ,@(parse-list e4) ")")))
                   (((? symbol? _) . _)
                    `(.&GROUP "(" (.&NEST 2 ,@(parse-list obj)) ")"))
                   (_
                    `(.&GROUP "(" (.&NEST 1 ,@(parse-list obj)) ")"))))
                ((vector? obj)
                 (if (= (vector-length obj) 0)
                     "#()"
                     `(.&GROUP "#(" (.&NEST 2 ,@(parse-list (vector->list obj))) ")")))
                ((tuple? obj)
                 (format "~w" obj))
                ((pretty-print-unwrap-syntax)
                 (format "~u" obj))
                (else
                 (format "~s" obj)))))

      (if (cyclic-object? expr)
          (format port "~w" expr)
          (let ((width (pretty-print-line-length)))
            (parameterize ((collect-notify #f))
              (print width 0 `((,(pretty-print-initial-indent) .&FLAT ,(parse expr)))))))
      (cond ((and n-more-lines (<= n-more-lines 0))
             (put-char port #\newline)
             (let loop ((i (pretty-print-initial-indent))) (and (> i 0) (put-char port #\space) (loop (- i 1))))
             (put-string port "  ..."))
            (else
             (unspecified))))))
