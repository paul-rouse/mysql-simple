{-# LANGUAGE BangPatterns, CPP, MagicHash, OverloadedStrings, UnboxedTuples #-}

-- | This module is designed to provide a shim for @blaze-textual@.
-- @blaze-textual@ does not support GHC 9. A PR has been opened to add that
-- support for GHC 9 here: https://github.com/bos/blaze-textual/pull/14
--
-- When GHC 9 support is merged in, we can delete the CPP in this and
-- re-export the blaze functions directly, which is what we do for older
-- versions of base.
module Database.MySQL.Internal.Blaze
    ( integral
    , double
    , float
    ) where

#if MIN_VERSION_base(4,15,0)

#define PAIR(a,b) (# a,b #)

import Blaze.ByteString.Builder (Builder, fromByteString)
import Blaze.ByteString.Builder.Char8 (fromChar)
import Data.ByteString.Char8 ()
import Data.Monoid (mappend, mconcat, mempty)
import qualified Data.Vector as V

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char8
import Data.ByteString.Char8 ()
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Monoid (mappend, mempty)
import Data.Word (Word, Word8, Word16, Word32, Word64)
import GHC.Base (quotInt, remInt)
import GHC.Num (quotRemInteger)
-- import GHC.Types (Int(..))

#if defined(INTEGER_GMP)
import GHC.Integer.GMP.Internals
#elif defined(INTEGER_SIMPLE)
import GHC.Integer.Simple.Internals
#endif

minus :: Builder
minus = fromWord8 45
data TInt = TInt !Integer !Int
putH :: [Integer] -> Builder
putH (n:ns) = case n `quotRemInteger` maxInt of
                PAIR(x,y)
                    | q > 0     -> int q `mappend` pblock r `mappend` putB ns
                    | otherwise -> int r `mappend` putB ns
                    where q = fromInteger x
                          r = fromInteger y
putH _ = error "putH: the impossible happened"
int :: Int -> Builder
int = integral
{-# INLINE int #-}
fstT :: TInt -> Integer
fstT (TInt a _) = a
maxInt :: Integer
maxDigits :: Int
TInt maxInt maxDigits =
    until ((>mi) . (*10) . fstT) (\(TInt n d) -> TInt (n*10) (d+1)) (TInt 10 1)
  where mi = fromIntegral (maxBound :: Int)
integral :: (Integral a, Show a) => a -> Builder
{-# RULES "integral/Int" integral = bounded :: Int -> Builder #-}
{-# RULES "integral/Int8" integral = bounded :: Int8 -> Builder #-}
{-# RULES "integral/Int16" integral = bounded :: Int16 -> Builder #-}
{-# RULES "integral/Int32" integral = bounded :: Int32 -> Builder #-}
{-# RULES "integral/Int64" integral = bounded :: Int64 -> Builder #-}
{-# RULES "integral/Word" integral = nonNegative :: Word -> Builder #-}
{-# RULES "integral/Word8" integral = nonNegative :: Word8 -> Builder #-}
{-# RULES "integral/Word16" integral = nonNegative :: Word16 -> Builder #-}
{-# RULES "integral/Word32" integral = nonNegative :: Word32 -> Builder #-}
{-# RULES "integral/Word64" integral = nonNegative :: Word64 -> Builder #-}
{-# RULES "integral/Integer" integral = integer :: Integer -> Builder #-}

-- This definition of the function is here PURELY to be used by ghci
-- and those rare cases where GHC is being invoked without
-- optimization, as otherwise the rewrite rules above should fire. The
-- test for "-0" catches an overflow if we render minBound.
integral i
    | i >= 0                 = nonNegative i
    | toByteString b == "-0" = fromString (show i)
    | otherwise              = b
  where b = minus `mappend` nonNegative (-i)

{-# NOINLINE integral #-}

pblock :: Int -> Builder
pblock = go maxDigits
  where
    go !d !n
        | d == 1    = digit n
        | otherwise = go (d-1) q `mappend` digit r
        where q = n `quotInt` 10
              r = n `remInt` 10

putB :: [Integer] -> Builder
putB (n:ns) = case n `quotRemInteger` maxInt of
                PAIR(x,y) -> pblock q `mappend` pblock r `mappend` putB ns
                    where q = fromInteger x
                          r = fromInteger y
putB _ = mempty

bounded :: (Bounded a, Integral a) => a -> Builder
{-# SPECIALIZE bounded :: Int -> Builder #-}
{-# SPECIALIZE bounded :: Int8 -> Builder #-}
{-# SPECIALIZE bounded :: Int16 -> Builder #-}
{-# SPECIALIZE bounded :: Int32 -> Builder #-}
{-# SPECIALIZE bounded :: Int64 -> Builder #-}
bounded i
    | i >= 0        = nonNegative i
    | i > minBound  = minus `mappend` nonNegative (-i)
    | otherwise     = minus `mappend`
                      nonNegative (negate (k `quot` 10)) `mappend`
                      digit (negate (k `rem` 10))
  where k = minBound `asTypeOf` i

nonNegative :: Integral a => a -> Builder
{-# SPECIALIZE nonNegative :: Int -> Builder #-}
{-# SPECIALIZE nonNegative :: Int8 -> Builder #-}
{-# SPECIALIZE nonNegative :: Int16 -> Builder #-}
{-# SPECIALIZE nonNegative :: Int32 -> Builder #-}
{-# SPECIALIZE nonNegative :: Int64 -> Builder #-}
{-# SPECIALIZE nonNegative :: Word -> Builder #-}
{-# SPECIALIZE nonNegative :: Word8 -> Builder #-}
{-# SPECIALIZE nonNegative :: Word16 -> Builder #-}
{-# SPECIALIZE nonNegative :: Word32 -> Builder #-}
{-# SPECIALIZE nonNegative :: Word64 -> Builder #-}
nonNegative = go
  where
    go n | n < 10    = digit n
         | otherwise = go (n `quot` 10) `mappend` digit (n `rem` 10)

digit :: Integral a => a -> Builder
digit n = fromWord8 $! fromIntegral n + 48
{-# INLINE digit #-}

integer :: Integer -> Builder
#if defined(INTEGER_GMP)
integer (S# i#) = int (I# i#)
#endif
integer i
    | i < 0     = minus `mappend` go (-i)
    | otherwise = go i
  where
    go n | n < maxInt = int (fromInteger n)
         | otherwise  = putH (splitf (maxInt * maxInt) n)

    splitf p n
      | p > n       = [n]
      | otherwise   = splith p (splitf (p*p) n)

    splith p (n:ns) = case n `quotRemInteger` p of
                        PAIR(q,r) | q > 0     -> q : r : splitb p ns
                                  | otherwise -> r : splitb p ns
    splith _ _      = error "splith: the impossible happened."

    splitb p (n:ns) = case n `quotRemInteger` p of
                        PAIR(q,r) -> q : r : splitb p ns
    splitb _ _      = []


-- The code below is originally from GHC.Float, but has been optimised
-- in quite a few ways.

data T = T [Int] {-# UNPACK #-} !Int

float :: Float -> Builder
float = double . realToFrac

double :: Double -> Builder
double f
    | isInfinite f              = fromByteString $
                                  if f > 0 then "Infinity" else "-Infinity"
    | f < 0 || isNegativeZero f = minus `mappend` goGeneric (floatToDigits (-f))
    | f >= 0                    = goGeneric (floatToDigits f)
    | otherwise                 = fromByteString "NaN"
  where
   goGeneric p@(T _ e)
     | e < 0 || e > 7 = goExponent p
     | otherwise      = goFixed    p
   goExponent (T is e) =
       case is of
         []     -> error "putFormattedFloat"
         [0]    -> fromByteString "0.0e0"
         [d]    -> digit d `mappend` fromByteString ".0e" `mappend` integral (e-1)
         (d:ds) -> digit d `mappend` fromChar '.' `mappend` digits ds `mappend`
                   fromChar 'e' `mappend` integral (e-1)
   goFixed (T is e)
       | e <= 0    = fromChar '0' `mappend` fromChar '.' `mappend`
                     mconcat (replicate (-e) (fromChar '0')) `mappend`
                     digits is
       | otherwise = let g 0 rs     = fromChar '.' `mappend` mk0 rs
                         g n []     = fromChar '0' `mappend` g (n-1) []
                         g n (r:rs) = digit r `mappend` g (n-1) rs
                     in g e is
   mk0 [] = fromChar '0'
   mk0 rs = digits rs

digits :: [Int] -> Builder
digits (d:ds) = digit d `mappend` digits ds
digits _      = mempty
{-# INLINE digits #-}

floatToDigits :: Double -> T
floatToDigits 0 = T [0] 0
floatToDigits x = T (reverse rds) k
 where
  (f0, e0)     = decodeFloat x
  (minExp0, _) = floatRange (undefined::Double)
  p = floatDigits x
  b = floatRadix x
  minExp = minExp0 - p -- the real minimum exponent
  -- Haskell requires that f be adjusted so denormalized numbers
  -- will have an impossibly low exponent.  Adjust for this.
  (# f, e #) =
   let n = minExp - e0 in
   if n > 0 then (# f0 `div` (b^n), e0+n #) else (# f0, e0 #)
  (# r, s, mUp, mDn #) =
   if e >= 0
   then let be = b^ e
        in if f == b^(p-1)
           then (# f*be*b*2, 2*b, be*b, b #)
           else (# f*be*2, 2, be, be #)
   else if e > minExp && f == b^(p-1)
        then (# f*b*2, b^(-e+1)*2, b, 1 #)
        else (# f*2, b^(-e)*2, 1, 1 #)
  k = fixup k0
   where
    k0 | b == 2 = (p - 1 + e0) * 3 `div` 10
        -- logBase 10 2 is slightly bigger than 3/10 so the following
        -- will err on the low side.  Ignoring the fraction will make
        -- it err even more.  Haskell promises that p-1 <= logBase b f
        -- < p.
       | otherwise = ceiling ((log (fromInteger (f+1) :: Double) +
                               fromIntegral e * log (fromInteger b)) / log 10)
    fixup n
      | n >= 0    = if r + mUp <= exp10 n * s then n else fixup (n+1)
      | otherwise = if exp10 (-n) * (r + mUp) <= s then n else fixup (n+1)

  gen ds !rn !sN !mUpN !mDnN =
   let (dn0, rn') = (rn * 10) `divMod` sN
       mUpN' = mUpN * 10
       mDnN' = mDnN * 10
       !dn   = fromInteger dn0
       !dn'  = dn + 1
   in case (# rn' < mDnN', rn' + mUpN' > sN #) of
        (# True,  False #) -> dn : ds
        (# False, True #)  -> dn' : ds
        (# True,  True #)  -> if rn' * 2 < sN then dn : ds else dn' : ds
        (# False, False #) -> gen (dn:ds) rn' sN mUpN' mDnN'

  rds | k >= 0    = gen [] r (s * exp10 k) mUp mDn
      | otherwise = gen [] (r * bk) s (mUp * bk) (mDn * bk)
      where bk = exp10 (-k)

exp10 :: Int -> Integer
exp10 n
    | n >= 0 && n < maxExpt = V.unsafeIndex expts n
    | otherwise             = 10 ^ n
  where expts = V.generate maxExpt (10^)
        {-# NOINLINE expts #-}
        maxExpt = 17
{-# INLINE exp10 #-}




#else

import Blaze.Text (integral, double, float)

#endif

