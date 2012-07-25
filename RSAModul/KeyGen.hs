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

-- | KeyGen generates a public and a private key. This module contains
-- some functions with O(n), so it's very slow for big keys.
module RSAModul.KeyGen(getKeys) where

import System.Random

import RSAModul.Key
import RSAModul.Helper


-- | Generates a tuple of distinct random primes
getPrimes :: Integer -> Integer -> (Integer, Integer)
getPrimes r1 r2 =  (primes!!(fromIntegral r1), primes!!(fromIntegral r2))

-- | calculate the final keys
getKeys :: IO (Key, Key)
getKeys = do
  (r1, r2) <- getRandomNumbers
  let (p, q) = getPrimes r1 r2
  let (n, phi) = ((p * q), (p -1) * (q -1))

  -- this seems to be extremly slow because (!!) has only O(n), the
  -- potential keys should be save in a BTree or HashMap
  e <- coprimes phi

  -- get the second key
  (a, b) <- extendEuklid e phi
  let d = getFactor a b e phi

  let publicKey = Key e n
  return (publicKey, Key d n)
  where
    coprimes phi = do
      e <- randomRIO(div phi 2, phi - 1)
      case (ggT e phi == 1) of
        True -> return e
        _ -> coprimes phi

    getFactor a b f phi
      | a*f `mod` phi == 1 = pos a
      | otherwise = pos b
        where
          pos x
            | x < 0 = phi + x
            | otherwise = x