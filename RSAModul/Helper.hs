{-

RSA Modul - training modul for pupils
Copyright (C) 2012 Benjamin Weißenfels <b.pixeldrama@gmail.com>

This program is free software: you can redistribute it and/or
modify it under the terms of the GNU General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

-}

-- | Helper Stuff comments will have to go here
module RSAModul.Helper where

import System.Random

-- | defines the max size of the primes set
maxPrim :: Integer
maxPrim = 10

ggT a 0 = a
ggT 0 b = b
ggT a b
  | a > b = ggT b (mod a b)
  | otherwise = ggT a (mod b a)
                
extendEuklid a b
  | b == 0 = return (1, 0)
  | otherwise = do
    let (q, r) = divMod a b
    (s, t) <- extendEuklid b r
    return (t, s - q * t)

-- | Produces an infinit List of primes. Should only be used taking
-- lazy evalution into accout. Seems to be a slow way to generate
-- primes.
primes :: [Integer]
primes = sieve [2..]
  where
    sieve (l:ls) = l: sieve[x | x <-ls, mod x 2 /= 0]

-- | Get a tuple of different random numbers
getRandomNumbers :: IO (Integer, Integer)
getRandomNumbers = do
  r1 <- randomRIO(div maxPrim 2, maxPrim -1)
  r2 <- randomRIO(div maxPrim 2, maxPrim -1)
  if r1 == r2 then
    getRandomNumbers
    else return (r1, r2)
