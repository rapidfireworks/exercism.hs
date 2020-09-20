module ComplexNumbers (Complex, conjugate, abs, exp, real, imaginary, mul, add, sub, div, complex) where

import Prelude hiding (abs, div, exp, (^))
import qualified Prelude (exp, (^))

data Complex a = Complex a a deriving (Eq, Show)

complex :: (a, a) -> Complex a
complex (r, i) = Complex r i

conjugate :: Num a => Complex a -> Complex a
conjugate (Complex r i) = Complex r (- i)

abs :: Floating a => Complex a -> a
abs (Complex r i) = sqrt (r ^ 2 + i ^ 2)

real :: Num a => Complex a -> a
real (Complex r _) = r

imaginary :: Num a => Complex a -> a
imaginary (Complex _ i) = i

exp :: Floating a => Complex a -> Complex a
exp (Complex r i) = Complex (e * c) (e * s)
  where
    e = Prelude.exp r
    c = cos i
    s = sin i

mul :: Num a => Complex a -> Complex a -> Complex a
mul lhs rhs = Complex (dot conj rhs) (det conj rhs)
  where
    conj = conjugate lhs

add :: Num a => Complex a -> Complex a -> Complex a
add (Complex r1 i1) (Complex r2 i2) = Complex (r1 + r2) (i1 + i2)

sub :: Num a => Complex a -> Complex a -> Complex a
sub (Complex r1 i1) (Complex r2 i2) = Complex (r1 - r2) (i1 - i2)

div :: Floating a => Complex a -> Complex a -> Complex a
div lhs rhs = Complex (r1 / r2) (i1 / r2)
  where
    conj = conjugate rhs
    Complex r1 i1 = mul lhs conj
    Complex r2 _ = mul rhs conj

dot :: Num a => Complex a -> Complex a -> a
dot (Complex a b) (Complex c d) = a * c + b * d

det :: Num a => Complex a -> Complex a -> a
det (Complex a b) (Complex c d) = a * d - b * c

(^) :: (Num a) => a -> Integer -> a
(^) = (Prelude.^)
