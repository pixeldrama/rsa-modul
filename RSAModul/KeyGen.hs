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

-- set max for primes
maxPrim :: Integer
maxPrim = 500

-- | Generates a tuple of distinct random primes
getPrimes :: Integer -> Integer -> (Integer, Integer)
getPrimes r1 r2 =  (primes!!(fromIntegral r1), primes!!(fromIntegral r2))
  where
    -- very slow way to generate primes
    primes :: [Integer]
    primes = sieve [2..]
      where
        sieve (l:ls) = l: sieve[x | x <-ls, mod x 2 /= 0]

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
  let keys = (publicKey, Key d n)
  -- work around for damaged keys, which means generated keys so long
  -- until they work. The checking function is O(n), so we have to
  -- wait a long time
  putStr "testing keys: "
  testingKeys keys >>= (\x -> case x of
                           False -> do
                             putStrLn "keys are not valid"
                             getKeys
                           _ -> do
                             putStrLn "keys are valid"
                             return keys
                       )
  where
    -- calculate all the coprimes until phi
    coprimes phi = do
      e <- randomRIO(div phi 2, phi - 1)
      case (ggT e phi == 1) of
        True -> return e
        _ -> coprimes phi

    ggT a 0 = a
    ggT 0 b = b
    ggT a b
      | a > b = ggT b (mod a b)
      | otherwise = ggT a (mod b a)

    getFactor a b f phi
      | a*f `mod` phi == 1 = pos a
      | otherwise = pos b
        where
          pos x
            | x < 0 = phi + x
            | otherwise = x

    extendEuklid a b
      | b == 0 = return (1, 0)
      | otherwise = do
        let (q, r) = divMod a b
        (s, t) <- extendEuklid b r
        return (t, s - q * t)

    getRandomNumbers :: IO (Integer, Integer)
    getRandomNumbers = do
      r1 <- randomRIO(div maxPrim 2, maxPrim -1)
      r2 <- randomRIO(div maxPrim 2, maxPrim -1)
      if r1 == r2 then
        getRandomNumbers
        else return (r1, r2)

    -- ugly testing
    testingKeys keys = do
      let l = map (\x -> x == (equalTest keys x)) [1 .. 20]
      check l 1
        where
          equalTest ((Key v1 n1), (Key v2 n2)) x = (((x^v1) `mod` n1)^v2) `mod` n2
          check [] _ = return (True)
          check (l:ls) n
            | l = check ls $ n + 1
              | otherwise = return (False)
