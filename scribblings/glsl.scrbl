#lang scribble/manual

@title{The OpenGL Shading Language 4.5 (GLSL)}
@author{@author+email["Eric Griffis" "dedbox@gmail.com"]}

@require{./glsl-includes.rkt}

@defmodulelang[glsl]

Hello, world!

@hyperlink["https://www.khronos.org/registry/OpenGL/specs/gl/GLSLangSpec.4.50.pdf"]{the spec}

@example[
  #:hidden
  (require glsl)
]

@example[
  (module vertex-shader-330 glsl
    (#%version 330 core)
    (layout ([location 0]) in vec2 vPosition)
    (define (main) : void
      (set! gl_Position (vec4 vPosition 0 1))))
]

@example[
  (require 'vertex-shader-330)
  (display the-string)
]
