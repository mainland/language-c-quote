-- |
-- Module      :  Language.C.Quote.OpenCL
-- Copyright   :  (c) Harvard University 2006-2011
--                (c) Geoffrey Mainland 2011-2013
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu

module Language.C.Quote.OpenCL (
    ToExp(..),
    cexp,
    cedecl,
    cdecl,
    csdecl,
    cenum,
    cty,
    cparam,
    cinit,
    cstm,
    citem,
    cunit,
    cfun
  ) where

import qualified Language.C.Parser as P
import qualified Language.C.Syntax as C
import Language.C.Quote.Base (ToExp(..), quasiquote)
import Language.Haskell.TH.Quote (QuasiQuoter)

exts :: [C.Extensions]
exts = [C.OpenCL]

-- | OpenCL types and reserved types.
-- As of OpenCL 1.2 the types are: http://www.khronos.org/files/opencl-1-2-quick-reference-card.pdf
-- reproduced here: 
--   * Builtin Scalar types: bool, char, unsigned char, uchar, short, unsigned short, ushort,
--   int, unsigned int, uint, long, unsigned long, ulong, float, double, half, size_t, 
--   ptrdiff_t, intptr_t, uintptr_t, void
--   * Bultin Vector data types of sizes with n = 2, 3, 4, 8, or 16:
--   charn, ucharn, shortn, ushortn, intn, uintn, longn, ulongn, floatn, doublen
--   * Other builtin types:
--   image2d_t, image3d_t, image2d_array_t, image1d_t, image1d_buffer_t, image1d_array_t,
--   sampler_t, event_t 
--   * Reserved types with n,m = 2,3,4,8, or 16:
--   booln, halfn, quad, quadn, complex half, complex halfn, imaginary half, imaginary halfn,
--   complex float, complex floatn, imaginary float, imaginary floatn, complex double,
--   complex doublen, imaginary double, imaginary doublen, complex quad, complex quadn,
--   imaginary quad, imaginary quadn, floatnxm, doublenxm
typenames :: [String]
typenames =
    -- Builtin Scalar types
    ["bool","char","uchar","short","ushort","int"
    ,"uint","long","ulong","float","double","half"
    ,"size_t","ptrdiff_t","intptr_t","uintptr_t","void"] ++
    -- Bulitin vector types
    typeN ["char","uchar","short","ushort","int","uint"
          ,"long","ulong","float","double"] ++
    -- Other builtin types:
    ["image2d_t","image3d_t","image2d_array_t","image1d_t"
    ,"image1d_buffer_t","image1d_array_t","sampler_t","event_t"] ++
    -- Reserved types:
    typeN ["bool","half","quad"] ++
    ["quad"] ++ typeNxM ["float","double"]
    -- TODO: add complex/imaginary types

typeN :: [String] -> [String]
typeN ts = [t ++ show n | n <- [2,3,4,8,16::Int], t <- ts]

typeNxM :: [String] -> [String]
typeNxM ts = [t ++ show n ++ "x" ++ show m | n <- [2,3,4,8,16::Int]
                                           , m <- [2,3,4,8,16::Int]
                                           , t <- ts ]

cdecl, cedecl, cenum, cexp, cfun, cinit, cparam, csdecl, cstm :: QuasiQuoter
citem, cty, cunit :: QuasiQuoter
cdecl  = quasiquote exts typenames P.parseDecl
cedecl = quasiquote exts typenames P.parseEdecl
cenum  = quasiquote exts typenames P.parseEnum
cexp   = quasiquote exts typenames P.parseExp
cfun   = quasiquote exts typenames P.parseFunc
cinit  = quasiquote exts typenames P.parseInit
cparam = quasiquote exts typenames P.parseParam
csdecl = quasiquote exts typenames P.parseStructDecl
cstm   = quasiquote exts typenames P.parseStm
citem  = quasiquote exts typenames P.parseBlockItem
cty    = quasiquote exts typenames P.parseType
cunit  = quasiquote exts typenames P.parseUnit
