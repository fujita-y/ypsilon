
(library (rnrs arithmetic flonums (6))

  (export flonum?
          real->flonum
          fl=?
          fl<?
          fl>?
          fl<=?
          fl>=?
          flinteger?
          flzero?
          flpositive?
          flnegative?
          flodd?
          fleven?
          flfinite?
          flinfinite?
          flnan?
          flmax
          flmin
          fl+
          fl*
          fl-
          fl/
          fldiv-and-mod
          fldiv
          flmod
          fldiv0-and-mod0
          fldiv0
          flmod0
          flnumerator
          fldenominator
          flfloor
          flceiling
          fltruncate
          flround
          flexp
          fllog
          flsin
          flcos
          fltan
          flasin
          flacos
          flatan
          flabs
          flsqrt
          fixnum->flonum)

  (import (core arithmetic)))