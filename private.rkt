#lang racket/base

(require racket/function
         syntax/parse
         (for-template racket/base
                       racket/format
                       racket/string))

(provide (all-defined-out))

(define (single-declaration stx)
  (define-values (ts stx*) (fully-specified-type stx))
  (if ts
      (syntax-parse stx*
        [(head:id . tail)
         (define-values (as stx**) (array-specifier #'tail))
         (values (append ts (list #`(~a 'head #,@as))) stx**)]
        [_ (values ts stx*)])
      (values #f #f)))

(define (fully-specified-type stx)
  (define-values (qs stx*) (type-qualifier stx))
  (define-values (t stx**) (type-specifier stx*))
  (if t (values (append qs t) stx**) (values #f #f)))

(define (type-qualifier stx)
  (let loop ([qs  null]
             [stx* stx])
    (define-values (head tail) (single-type-qualifier stx*))
    (if head
        (loop (cons head qs) tail)
        (values (reverse qs) stx*))))

(define (single-type-qualifier stx)
  (define-values            (head tail)       (storage-qualifier stx))
  (unless head (set!-values (head tail)        (layout-qualifier stx)))
  (unless head (set!-values (head tail)     (precision-qualifier stx)))
  (unless head (set!-values (head tail) (interpolation-qualifier stx)))
  (unless head (set!-values (head tail)     (invariant-qualifier stx)))
  (unless head (set!-values (head tail)       (precise-qualifier stx)))
  (values head tail))

(define storage-qualifier
  (syntax-parser
    [((~datum const)      . tail) (values #'"const"      #'tail)]
    [((~datum in)         . tail) (values #'"in"         #'tail)]
    [((~datum out)        . tail) (values #'"out"        #'tail)]
    [((~datum inout)      . tail) (values #'"inout"      #'tail)]
    [((~datum centroid)   . tail) (values #'"centroid"   #'tail)]
    [((~datum patch)      . tail) (values #'"patch"      #'tail)]
    [((~datum sample)     . tail) (values #'"sample"     #'tail)]
    [((~datum uniform)    . tail) (values #'"uniform"    #'tail)]
    [((~datum buffer)     . tail) (values #'"buffer"     #'tail)]
    [((~datum shared)     . tail) (values #'"shared"     #'tail)]
    [((~datum coherent)   . tail) (values #'"coherent"   #'tail)]
    [((~datum volatile)   . tail) (values #'"volatile"   #'tail)]
    [((~datum restrict)   . tail) (values #'"restrict"   #'tail)]
    [((~datum readonly)   . tail) (values #'"readonly"   #'tail)]
    [((~datum writeonly)  . tail) (values #'"writeonly"  #'tail)]
    [((~datum subroutine) . tail) (values #'"subroutine" #'tail)]
    [_ (values #f #f)]))

(define layout-qualifier
  (syntax-parser
    [((~datum layout) ((~or a:id [a:id e:expr] (~datum shared)) ...+) . tail)
     (values
      #'(~a "layout ("
            (string-join (list (~a (~? (~@ 'a (~? (~@ " = " e))) 's)) ...) ", ")
            ")")
      #'tail)]
    [_ (values #f #f)]))

(define precision-qualifier
  (syntax-parser
    [((~datum highp)   . tail) (values #'"highp"   #'tail)]
    [((~datum mediump) . tail) (values #'"mediump" #'tail)]
    [((~datum lowp)    . tail) (values #'"lowp"    #'tail)]
    [_ (values #f #f)]))

(define interpolation-qualifier
  (syntax-parser
    [((~datum smooth)        . tail) (values #'"smooth"        #'tail)]
    [((~datum flat)          . tail) (values #'"flat"          #'tail)]
    [((~datum noperspective) . tail) (values #'"noperspective" #'tail)]
    [_ (values #f #f)]))

(define invariant-qualifier
  (syntax-parser
    [((~datum invariant) . tail) (values #'"invariant" #'tail)]
    [_ (values #f #f)]))

(define precise-qualifier
  (syntax-parser
    [((~datum precise) . tail) (values #'"precise" #'tail)]
    [_ (values #f #f)]))

(define (type-specifier stx)
  (define-values ( s stx* ) (type-specifier-nonarray stx))
  (define-values (as stx**) (if s (array-specifier stx*) (values #f #f)))
  (values (and as (list #`(~a #,s #,@as))) (and as stx**)))

(define (array-specifier stx)
  (let loop ([as  null]
             [stx* stx])
    (syntax-parse stx*
      [(() . tail) (loop (cons #'"[]" as) #'tail)] 
      [((n:exact-integer) . tail) (loop (cons #'(~a "[" n "]") as) #'tail)]
      [_ (values (reverse as) stx*)])))

(define type-specifier-nonarray
  (syntax-parser
    [((~datum void)    . tail) (values #'"void"    #'tail)]
    [((~datum float)   . tail) (values #'"float"   #'tail)]
    [((~datum double)  . tail) (values #'"double"  #'tail)]
    [((~datum int)     . tail) (values #'"int"     #'tail)]
    [((~datum uint)    . tail) (values #'"uint"    #'tail)]
    [((~datum bool)    . tail) (values #'"bool"    #'tail)]
    [((~datum vec2)    . tail) (values #'"vec2"    #'tail)]
    [((~datum vec3)    . tail) (values #'"vec3"    #'tail)]
    [((~datum vec4)    . tail) (values #'"vec4"    #'tail)]
    [((~datum dvec2)   . tail) (values #'"dvec2"   #'tail)]
    [((~datum dvec3)   . tail) (values #'"dvec3"   #'tail)]
    [((~datum dvec4)   . tail) (values #'"dvec4"   #'tail)]
    [((~datum bvec2)   . tail) (values #'"bvec2"   #'tail)]
    [((~datum bvec3)   . tail) (values #'"bvec3"   #'tail)]
    [((~datum bvec4)   . tail) (values #'"bvec4"   #'tail)]
    [((~datum ivec2)   . tail) (values #'"ivec2"   #'tail)]
    [((~datum ivec3)   . tail) (values #'"ivec3"   #'tail)]
    [((~datum ivec4)   . tail) (values #'"ivec4"   #'tail)]
    [((~datum uvec2)   . tail) (values #'"uvec2"   #'tail)]
    [((~datum uvec3)   . tail) (values #'"uvec3"   #'tail)]
    [((~datum uvec4)   . tail) (values #'"uvec4"   #'tail)]
    [((~datum mat2)    . tail) (values #'"mat2"    #'tail)]
    [((~datum mat3)    . tail) (values #'"mat3"    #'tail)]
    [((~datum mat4)    . tail) (values #'"mat4"    #'tail)]
    [((~datum mat2x2)  . tail) (values #'"mat2x2"  #'tail)]
    [((~datum mat2x3)  . tail) (values #'"mat2x3"  #'tail)]
    [((~datum mat2x4)  . tail) (values #'"mat2x4"  #'tail)]
    [((~datum mat3x2)  . tail) (values #'"mat3x2"  #'tail)]
    [((~datum mat3x3)  . tail) (values #'"mat3x3"  #'tail)]
    [((~datum mat3x4)  . tail) (values #'"mat3x4"  #'tail)]
    [((~datum mat4x2)  . tail) (values #'"mat4x2"  #'tail)]
    [((~datum mat4x3)  . tail) (values #'"mat4x3"  #'tail)]
    [((~datum mat4x4)  . tail) (values #'"mat4x4"  #'tail)]
    [((~datum dmat2)   . tail) (values #'"dmat2"   #'tail)]
    [((~datum dmat3)   . tail) (values #'"dmat3"   #'tail)]
    [((~datum dmat4)   . tail) (values #'"dmat4"   #'tail)]
    [((~datum dmat2x2) . tail) (values #'"dmat2x2" #'tail)]
    [((~datum dmat2x3) . tail) (values #'"dmat2x3" #'tail)]
    [((~datum dmat2x4) . tail) (values #'"dmat2x4" #'tail)]
    [((~datum dmat3x2) . tail) (values #'"dmat3x2" #'tail)]
    [((~datum dmat3x3) . tail) (values #'"dmat3x3" #'tail)]
    [((~datum dmat3x4) . tail) (values #'"dmat3x4" #'tail)]
    [((~datum dmat4x2) . tail) (values #'"dmat4x2" #'tail)]
    [((~datum dmat4x3) . tail) (values #'"dmat4x3" #'tail)]
    [((~datum dmat4x4) . tail) (values #'"dmat4x4" #'tail)]
    [((~datum atomic_uint) . tail) (values #'"atomic_uint" #'tail)]
    [((~datum sampler1d)              . tail) (values #'"sampler1d"              #'tail)]
    [((~datum sampler2d200)           . tail) (values #'"sampler2d200"           #'tail)]
    [((~datum sampler3d)              . tail) (values #'"sampler3d"              #'tail)]
    [((~datum samplercube)            . tail) (values #'"samplercube"            #'tail)]
    [((~datum sampler1dshadow)        . tail) (values #'"sampler1dshadow"        #'tail)]
    [((~datum sampler2dshadow)        . tail) (values #'"sampler2dshadow"        #'tail)]
    [((~datum samplercubeshadow)      . tail) (values #'"samplercubeshadow"      #'tail)]
    [((~datum sampler1darray)         . tail) (values #'"sampler1darray"         #'tail)]
    [((~datum sampler2darray)         . tail) (values #'"sampler2darray"         #'tail)]
    [((~datum sampler1darrayshadow)   . tail) (values #'"sampler1darrayshadow"   #'tail)]
    [((~datum sampler2darrayshadow)   . tail) (values #'"sampler2darrayshadow"   #'tail)]
    [((~datum samplercubearray)       . tail) (values #'"samplercubearray"       #'tail)]
    [((~datum samplercubearrayshadow) . tail) (values #'"samplercubearrayshadow" #'tail)]
    [((~datum isampler1d)        . tail) (values #'"isampler1d"        #'tail)]
    [((~datum isampler2d)        . tail) (values #'"isampler2d"        #'tail)]
    [((~datum isampler3d)        . tail) (values #'"isampler3d"        #'tail)]
    [((~datum isamplercube)      . tail) (values #'"isamplercube"      #'tail)]
    [((~datum isampler1darray)   . tail) (values #'"isampler1darray"   #'tail)]
    [((~datum isampler2darray)   . tail) (values #'"isampler2darray"   #'tail)]
    [((~datum isamplercubearray) . tail) (values #'"isamplercubearray" #'tail)]
    [((~datum usampler1d)        . tail) (values #'"usampler1d"        #'tail)]
    [((~datum usampler2d)        . tail) (values #'"usampler2d"        #'tail)]
    [((~datum usampler3d)        . tail) (values #'"usampler3d"        #'tail)]
    [((~datum usamplercube)      . tail) (values #'"usamplercube"      #'tail)]
    [((~datum usampler1darray)   . tail) (values #'"usampler1darray"   #'tail)]
    [((~datum usampler2darray)   . tail) (values #'"usampler2darray"   #'tail)]
    [((~datum usamplercubearray) . tail) (values #'"usamplercubearray" #'tail)]
    [((~datum sampler2drectshadow) . tail) (values #'"sampler2drectshadow" #'tail)]
    [((~datum sampler2drect)       . tail) (values #'"sampler2drect"       #'tail)]
    [((~datum isampler2drect)      . tail) (values #'"isampler2drect"      #'tail)]
    [((~datum usampler2drect)      . tail) (values #'"usampler2drect"      #'tail)]
    [((~datum samplerbuffer)  . tail) (values #'"samplerbuffer"  #'tail)]
    [((~datum isamplerbuffer) . tail) (values #'"isamplerbuffer" #'tail)]
    [((~datum usamplerbuffer) . tail) (values #'"usamplerbuffer" #'tail)]
    [((~datum sampler2dms)  . tail) (values #'"sampler2dms"  #'tail)]
    [((~datum isampler2dms) . tail) (values #'"isampler2dms" #'tail)]
    [((~datum usampler2dms) . tail) (values #'"usampler2dms" #'tail)]
    [((~datum sampler2dmsarray)     . tail) (values #'"sampler2dmsarray"     #'tail)]
    [((~datum isampler2dmsarray201) . tail) (values #'"isampler2dmsarray201" #'tail)]
    [((~datum usampler2dmsarray)    . tail) (values #'"usampler2dmsarray"    #'tail)]
    [((~datum image1d)  . tail) (values #'"image1d"  #'tail)]
    [((~datum iimage1d) . tail) (values #'"iimage1d" #'tail)]
    [((~datum uimage1d) . tail) (values #'"uimage1d" #'tail)]
    [((~datum image2d)  . tail) (values #'"image2d"  #'tail)]
    [((~datum iimage2d) . tail) (values #'"iimage2d" #'tail)]
    [((~datum uimage2d) . tail) (values #'"uimage2d" #'tail)]
    [((~datum image3d)  . tail) (values #'"image3d"  #'tail)]
    [((~datum iimage3d) . tail) (values #'"iimage3d" #'tail)]
    [((~datum uimage3d) . tail) (values #'"uimage3d" #'tail)]
    [((~datum image2drect)  . tail) (values #'"image2drect"  #'tail)]
    [((~datum iimage2drect) . tail) (values #'"iimage2drect" #'tail)]
    [((~datum uimage2drect) . tail) (values #'"uimage2drect" #'tail)]
    [((~datum imagecube)  . tail) (values #'"imagecube"  #'tail)]
    [((~datum iimagecube) . tail) (values #'"iimagecube" #'tail)]
    [((~datum uimagecube) . tail) (values #'"uimagecube" #'tail)]
    [((~datum imagebuffer)  . tail) (values #'"imagebuffer"  #'tail)]
    [((~datum iimagebuffer) . tail) (values #'"iimagebuffer" #'tail)]
    [((~datum uimagebuffer) . tail) (values #'"uimagebuffer" #'tail)]
    [((~datum image1darray)  . tail) (values #'"image1darray"  #'tail)]
    [((~datum iimage1darray) . tail) (values #'"iimage1darray" #'tail)]
    [((~datum uimage1darray) . tail) (values #'"uimage1darray" #'tail)]
    [((~datum image2darray)  . tail) (values #'"image2darray"  #'tail)]
    [((~datum iimage2darray) . tail) (values #'"iimage2darray" #'tail)]
    [((~datum uimage2darray) . tail) (values #'"uimage2darray" #'tail)]
    [((~datum imagecubearray)  . tail) (values #'"imagecubearray"  #'tail)]
    [((~datum iimagecubearray) . tail) (values #'"iimagecubearray" #'tail)]
    [((~datum uimagecubearray) . tail) (values #'"uimagecubearray" #'tail)]
    [((~datum image2dms)  . tail) (values #'"image2dms"  #'tail)]
    [((~datum iimage2dms) . tail) (values #'"iimage2dms" #'tail)]
    [((~datum uimage2dms) . tail) (values #'"uimage2dms" #'tail)]
    [((~datum image2dmsarray)  . tail) (values #'"image2dmsarray"  #'tail)]
    [((~datum iimage2dmsarray) . tail) (values #'"iimage2dmsarray" #'tail)]
    [((~datum uimage2dmsarray) . tail) (values #'"uimage2dmsarray" #'tail)]
    [_ (values #f #f)]))

(define primary-expression
  (syntax-parser
    [(~or :id :number) #`(~a '#,this-syntax)]
    [#t #'"true"]
    [#f #'"false"]
    [_ #f]))

(define postfix-expression
  (disjoin
   primary-expression
   (syntax-parser
     [(e (~datum ++)) (define ex (postfix-expression #'e)) (and ex #`(~a #,ex '++))]
     [(e (~datum --)) (define ex (postfix-expression #'e)) (and ex #`(~a #,ex '--))]
     [_ #f])))

(define-syntax-class reserved-word
  (pattern (~or (~datum and)
                (~datum or)
                (~datum xor))))

(define function-call-generic
  (syntax-parser
    [((~and f:id (~not :reserved-word)) arg ...)
     (define fun (function-identifier #'f))
     (define args (and fun (map assignment-expression (attribute arg))))
     (and args
          (andmap values args)
          #`(~a #,fun "(" (string-join (list #,@args) ", ") ")"))]
    [_ #f]))

(define function-call-or-method function-call-generic)
(define function-call function-call-or-method)

(define (function-identifier stx)
  (define-values (t _) (type-specifier-nonarray #`(#,stx)))
  (or t (postfix-expression stx)))

(define unary-expression
  (disjoin
   postfix-expression
   (syntax-parser
     [((~datum ++) e) (define ex (unary-expression #'e)) (and ex #`(~a '++ #,ex))]
     [((~datum --) e) (define ex (unary-expression #'e)) (and ex #`(~a '-- #,ex))]
     [(o e)
      (define op (unary-operator #'o))
      (define ex (and op (unary-expression #'e)))
      (and ex #`(~a #,op #,ex))]
     [_ #f])))

(define unary-operator
  (syntax-parser
    [(~datum +) #'"+"]
    [(~datum -) #'"-"]
    [(~datum !) #'"!"]
    [(~datum ~) #'"~"]
    [_ #f]))

(define multiplicative-expression
  (disjoin
   unary-expression
   (syntax-parser
     [(e [i:exact-integer])
      (define ex (postfix-expression #'e))
      (and ex #`(~a #,ex "[" i "]"))]
     [_ #f])
   (syntax-parser
     [((~datum *) e ...)
      (define es (map unary-expression (attribute e)))
      (and (andmap values es) #`(string-join (list #,@es) " * "))]
     [((~datum /) e ...)
      (define es (map unary-expression (attribute e)))
      (and (andmap values es) #`(string-join (list #,@es) " / "))]
     [((~datum %) e ...)
      (define es (map unary-expression (attribute e)))
      (and (andmap values es) #`(string-join (list #,@es) " % "))]
     [_ #f])
   function-call))

(define additive-expression
  (disjoin
   multiplicative-expression
   (syntax-parser
     [((~datum +) e ...)
      (define es (map multiplicative-expression (attribute e)))
      (and (andmap values es) #`(string-join (list #,@es) " + "))]
     [((~datum -) e ...)
      (define es (map multiplicative-expression (attribute e)))
      (and (andmap values es) #`(string-join (list #,@es) " - "))]
     [_ #f])))

(define shift-expression
  (disjoin
   additive-expression
   (syntax-parser
     [((~datum <<) e ...)
      (define es (map additive-expression (attribute e)))
      (and (andmap values es) #`(string-join (list #,@es) " << "))]
     [((~datum >>) e ...)
      (define es (map additive-expression (attribute e)))
      (and (andmap values es) #`(string-join (list #,@es) " >> "))]
     [_ #f])))

(define relational-expression
  (disjoin
   shift-expression
   (syntax-parser
     [((~datum <) e ...)
      (define es (map shift-expression (attribute e)))
      (and (andmap values es) #`(string-join (list #,@es) " < "))]
     [((~datum >) e ...)
      (define es (map shift-expression (attribute e)))
      (and (andmap values es) #`(string-join (list #,@es) " > "))]
     [((~datum <=) e ...)
      (define es (map shift-expression (attribute e)))
      (and (andmap values es) #`(string-join (list #,@es) " <= "))]
     [((~datum >=) e ...)
      (define es (map shift-expression (attribute e)))
      (and (andmap values es) #`(string-join (list #,@es) " >= "))]
     [_ #f])))

(define equality-expression
  (disjoin
   relational-expression
   (syntax-parser
     [((~datum ==) e ...)
      (define es (map relational-expression (attribute e)))
      (and (andmap values es) #`(string-join (list #,@es) " == "))]
     [((~datum !=) e ...)
      (define es (map relational-expression (attribute e)))
      (and (andmap values es) #`(string-join (list #,@es) " != "))]
     [_ #f])))

(define and-expression
  (disjoin
   equality-expression
   (syntax-parser
     [((~datum bitwise-and) e ...)
      (define es (map equality-expression (attribute e)))
      (and (andmap values es) #`(string-join (list #,@es) " & "))]
     [_ #f])))

(define exclusive-or-expression
  (disjoin
   and-expression
   (syntax-parser
     [((~datum bitwise-xor) e ...)
      (define es (map and-expression (attribute e)))
      (and (andmap values es) #`(string-join (list #,@es) " ^ "))]
     [_ #f])))

(define inclusive-or-expression
  (disjoin
   exclusive-or-expression
   (syntax-parser
     [((~datum bitwise-ior) e ...)
      (define es (map exclusive-or-expression (attribute e)))
      (and (andmap values es) #`(string-join (list #,@es) " | "))]
     [_ #f])))

(define logical-and-expression
  (disjoin
   inclusive-or-expression
   (syntax-parser
     [((~datum and) e ...)
      (define es (map inclusive-or-expression (attribute e)))
      (and (andmap values es) #`(string-join (list #,@es) " && "))]
     [_ #f])))

(define logical-xor-expression
  (disjoin
   logical-and-expression
   (syntax-parser
     [((~datum xor) e ...)
      (define es (map logical-and-expression (attribute e)))
      (and (andmap values es) #`(string-join (list #,@es) " ^^ "))]
     [_ #f])))

(define logical-or-expression
  (disjoin
   logical-xor-expression
   (syntax-parser
     [((~datum or) e ...)
      (define es (map logical-xor-expression (attribute e)))
      (and (andmap values es) #`(string-join (list #,@es) " || "))]
     [_ #f])))

(define conditional-expression
  (disjoin
   (syntax-parser
     [((~datum if) t e1 e2)
      (define test (logical-or-expression #'t))
      (define ex1 (and test (expression #'e1)))
      (define ex2 (and ex1 (assignment-expression #'e2)))
      (and ex2 #`(string-join #,test "?" #,ex1 ":" #,ex2))]
     [_ #f])
   logical-or-expression))

(define assignment-operator
  (syntax-parser
    [(~datum *=) #'"*="]
    [(~datum /=) #'"/="]
    [(~datum %=) #'"%="]
    [(~datum +=) #'"+="]
    [(~datum -=) #'"-="]
    [(~datum &=) #'"&="]
    [(~datum ^=) #'"^="]
    [(~datum or=) #'"|="]
    [_ #f]))

(define assignment-expression
  (disjoin
   (syntax-parser
     [((~datum set!) l r)
      (define lhs (unary-expression #'l))
      (define rhs (and lhs (assignment-expression #'r)))
      (and rhs #`(string-join (list #,lhs "=" #,rhs)))]
     [(o l r ...+)
      (define op (assignment-operator #'o))
      (define lhs (and op  (unary-expression #'l)))
      (define rhs (and lhs (map assignment-expression (attribute r))))
      (and rhs
           (andmap values rhs)
           #`(~a "("
                 (string-join (list #,@(cons lhs rhs)) (~a " " #,op " "))
                 ")"))]
     [_ #f])
   conditional-expression))

(define expression assignment-expression)

(define (expression-statement stx)
  (define e (expression stx))
  (and e #`(~a #,e ";")))

(define simple-statement expression-statement)
(define statement simple-statement)

(define function-definition
  (syntax-parser
    [((~datum define) (f:id [tx:id ...+ x:id] ...) (~datum :) (~or tf:id (tfs ...+))
                      b ...)
     (define body (map statement (attribute b)))
     (and
      (andmap values body)
      #`(~a (string-join (list (~? (~a 'tf) (~? (~@ (~a 'tfs) ...))) (~a 'f)))
            "("
            (string-join (list (string-join (list (~a 'tx) ... (~a 'x))) ...) ", ")
            ") {\n"
            #,@(for/list ([form (in-list body)]) #`(~a "  " #,form "\n"))
            "}"))]
    [_ #f]))

(define init-declarator-list single-declaration)

(define (declaration stx)
  (define-values (ds stx*) (init-declarator-list stx))
  (and ds
       (null? (syntax-e stx*))
       #`(~a (string-join (list #,@ds)) ";")))

(define (external-declaration stx)
  (or (function-definition stx)
      (declaration stx)
      (raise-syntax-error #f "invalid GLSL declaration" stx)))

(define-syntax-class version-num
  #:description "GLSL version"
  (pattern :nat))

(define-syntax-class profile-name
  #:description "GLSL profile name"
  (pattern (~or (~datum core) (~datum compatibility) (~datum es))))

(define version-directive
  (syntax-parser
    [((~datum #%version) val:version-num (~optional profile:profile-name))
     #'(~a "#version " val (~? (~@ " " 'profile)))]
    [_ #f]))

(define preprocessing-directive version-directive)

(define glsl-top
  (disjoin preprocessing-directive
           external-declaration))
