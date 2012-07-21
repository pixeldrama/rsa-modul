import RSAModul.KeyGen
import RSAModul.Key
import RSAModul.Crypto
import System.Random

import Test.HUnit

testKeyGen = do
  keys <- getKeys
  let staticKeys = (Key 23 143, Key 47 143)
  randomInt <- randomRIO(0, (modulus $ fst keys) - 1)
  
  putStrLn $ show keys
  
  let tCase1 = TestCase (assertEqual "static value:" 2 (equalTest keys 2))
  let tCase2 = TestCase (assertEqual "static value:" 8 (equalTest keys 8))
  let tCase3 = TestCase (assertEqual "random value:" randomInt (equalTest keys randomInt))  
  let tCase4 = TestCase (assertEqual "standard case:" 2 (equalTest staticKeys 2))
      
  let str = "Hello World!"      
  let tCase5 = TestCase $ assertEqual "test encrypt/decrypt" str $ enDeCrypt str staticKeys  
  runTestTT $ TestList [tCase1, tCase2, tCase3, tCase4, tCase5]
  return ()
    where
      equalTest ((Key v1 n1), (Key v2 n2)) x = ((x^v1) `mod` n1)^v2 `mod` n2
      enDeCrypt str (pub, priv) = encrypt (encrypt str pub) priv
