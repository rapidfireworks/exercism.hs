module ComplexNumbers (Complex, conjugate, abs, exp, real, imaginary, mul, add, sub, div, complex) where

import Prelude hiding (abs, div, exp, (^))
import qualified Prelude (exp, (^))

type Complex a = (a, a)

complex :: (a, a) -> Complex a
complex = id

conjugate :: Num a => Complex a -> Complex a
conjugate (a, b) = (a, - b)

abs :: Floating a => Complex a -> a
abs (a, b) = sqrt (a ^ 2 + b ^ 2)

real :: Num a => Complex a -> a
real = fst

imaginary :: Num a => Complex a -> a
imaginary = snd

exp :: Floating a => Complex a -> Complex a
exp (r, i) = (e * cos i, e * sin i)
  where
    e = Prelude.exp r

mul :: Num a => Complex a -> Complex a -> Complex a
mul (a, b) (c, d) = (a * c - b * d, b * c + a * d)

add :: Num a => Complex a -> Complex a -> Complex a
add (a, b) (c, d) = (a + c, b + d)

sub :: Num a => Complex a -> Complex a -> Complex a
sub (a, b) (c, d) = (a - c, b - d)

div :: Fractional a => Complex a -> Complex a -> Complex a
div (a, b) (c, d) = ((a * c + b * d) / (c ^ 2 + d ^ 2), (b * c - a * d) / (c ^ 2 + d ^ 2))

(^) :: (Num a) => a -> Integer -> a
(^) = (Prelude.^)
