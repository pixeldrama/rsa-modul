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

-- | KeyGen generates a public and a private key
module RSAModul.KeyGen(getKeys) where

import System.Random
import RSAModul.Key

-- set max for primes
maxPrim :: Int
maxPrim = 100

-- | Generates a tuple of distinct random primes
createRandomPrimes :: IO (Int, Int)
createRandomPrimes =  do
  let l = length primes
  p <- randomRIO (div l 2, l - 1)
  q <- randomRIO (div l 2, l - 1)
  if (p == q) then 
    createRandomPrimes
    else   
    return (primes!!p, primes!!q)
  where
    -- very slow way to generate primes
    primes :: [Int]
    primes = sieve [2..maxPrim]
      where 
        sieve [] = []
        sieve (l:ls) = l: sieve[x | x <-ls, mod x 2 /= 0]
        
-- | Calculate the modulus n and phi(n). The modulus is used for the
-- public and private key and must be stored. Phi is the result of the
-- Euler's phi function of n.
n_phi :: IO (Int, Int) -- ^ the first value is n, the second phi(n)
n_phi = do 
  (p,q) <- createRandomPrimes
  return $ (p * q, (p - 1) * (q -1)) 
  
-- | calculate the final keys
getKeys :: IO (Key, Key)
getKeys = do
  (n, phi) <- n_phi
  let potentialKeys = coprimes phi
      
  -- select a random value from the potential keys
  index <- randomRIO (0, (length potentialKeys) - 1)
  let privateKey = Key (fromIntegral $ potentialKeys!!index) $ fromIntegral n
      
  -- get the second key
  res <- extendEuklid (value privateKey) $ fromIntegral phi
  let publicKey = Key ((fst res) `mod` (fromIntegral phi)) (fromIntegral n)
  return (privateKey, publicKey)
  where
    -- calculate all the coprimes until phi
    coprimes phi = [y | y <-[(div phi 2)..phi],  ggT y phi == 1]
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

