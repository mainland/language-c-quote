-- Copyright (c) 2010-2011
--         The President and Fellows of Harvard College.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
-- 3. Neither the name of the University nor the names of its contributors
--    may be used to endorse or promote products derived from this software
--    without specific prior written permission.

-- THIS SOFTWARE IS PROVIDED BY THE UNIVERSITY AND CONTRIBUTORS ``AS IS'' AND
-- ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- ARE DISCLAIMED.  IN NO EVENT SHALL THE UNIVERSITY OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
-- LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
-- OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
-- SUCH DAMAGE.

--------------------------------------------------------------------------------
-- |
-- Module      :  Language.C.Smart
-- Copyright   :  (c) Harvard University 2010-2011
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

{-# LANGUAGE QuasiQuotes #-}

module Language.C.Smart where

import qualified Data.Loc
import qualified Data.Symbol
import Language.C.Quote.C
import Language.C.Syntax as C
import qualified Language.C.Syntax

instance Enum Exp where
    toEnum n = [cexp|$int:n|]

    fromEnum [cexp|$int:n|]   = fromIntegral n
    fromEnum [cexp|$uint:n|]  = fromIntegral n
    fromEnum [cexp|$lint:n|]  = fromIntegral n
    fromEnum [cexp|$ulint:n|] = fromIntegral n

    fromEnum _ =
        error "fromEnum: non-integer constant C expressions"

instance Num C.Exp where
    e1 + e2       = [cexp|$exp:e1 + $exp:e2|]
    e1 * e2       = [cexp|$exp:e1 * $exp:e2|]
    e1 - e2       = [cexp|$exp:e1 - $exp:e2|]
    negate e      = [cexp|-$exp:e|]
    abs e         = [cexp|abs($exp:e)|]
    signum e      = [cexp|$exp:e > 0 ? 1 : ($exp:e < 0 ? -1 : 0)|]
    fromInteger n = [cexp|$int:n|]

instance Real C.Exp where
    toRational [cexp|$float:n|]   = n
    toRational [cexp|$double:n|]  = n
    toRational [cexp|$ldouble:n|] = n

    toRational _ =
        error "fromEnum: non-rational constant C expressions"

instance Integral C.Exp where
    e1 `quotRem` e2 = ([cexp|$exp:e1 / $exp:e2|], [cexp|$exp:e1 % $exp:e2|])

    toInteger [cexp|$int:n|]   = n
    toInteger [cexp|$uint:n|]  = n
    toInteger [cexp|$lint:n|]  = n
    toInteger [cexp|$ulint:n|] = n

    toInteger _ =
        error "fromInteger: non-integer constant C expressions"

instance Fractional C.Exp where
    e1 / e2 = [cexp|$exp:e1 / $exp:e2|]
    recip e = [cexp|1 / $exp:e|]

    fromRational n = [cexp|$double:n|]

instance Floating C.Exp where
    pi            = [cexp|3.141592653589793238|]
    exp e         = [cexp|exp($exp:e)|]
    sqrt e        = [cexp|sqrt($exp:e)|]
    log e         = [cexp|log($exp:e)|]
    e1 ** e2      = [cexp|pow($exp:e1, $exp:e2)|]
    logBase e1 e2 = [cexp|log($exp:e2)/log($exp:e1)|]
    sin e         = [cexp|sin($exp:e)|]
    tan e         = [cexp|tan($exp:e)|]
    cos e         = [cexp|cos($exp:e)|]
    asin e        = [cexp|asin($exp:e)|]
    atan e        = [cexp|atan($exp:e)|]
    acos e        = [cexp|acos($exp:e)|]
    sinh e        = [cexp|sinh($exp:e)|]
    tanh e        = [cexp|tanh($exp:e)|]
    cosh e        = [cexp|cosh($exp:e)|]
    asinh e       = [cexp|asinh($exp:e)|]
    atanh e       = [cexp|atanh($exp:e)|]
    acosh e       = [cexp|acosh($exp:e)|]

infix 4 ===
(===) :: C.Exp -> C.Exp -> C.Stm
e1 === e2 = [cstm|$exp:e1 = $exp:e2;|]

infix 4 +=
(+=) :: C.Exp -> C.Exp -> C.Stm
e1 += e2 = [cstm|$exp:e1 += $exp:e2;|]
