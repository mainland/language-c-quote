-- |
-- Module      :  Language.C.Smart
-- Copyright   :  (c) 2010-2011 Harvard University
--                (c) 2011-2013 Geoffrey Mainland
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.C.Smart where

import Language.C.Quote.C
import Language.C.Syntax as C

#if !MIN_VERSION_template_haskell(2,7,0)
import qualified Data.Loc
import qualified Language.C.Syntax
#endif /* !MIN_VERSION_template_haskell(2,7,0) */

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
    toRational [cexp|$float:n|]   = toRational n
    toRational [cexp|$double:n|]  = toRational n
    toRational [cexp|$ldouble:n|] = toRational n

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

    fromRational n = [cexp|$double:(fromRational n)|]

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
