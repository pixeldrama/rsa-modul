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

module Main where

import System.Environment

import RSAModul.WriteKeys
import RSAModul.Key
import RSAModul.Crypto


usageMessage = do putStrLn "usage: keygen | encrypt <path/to/pubkey> <file> | decrypt <path/to/privkey> <file"

main = do
  args <- getArgs
  let sizeArgs = length args
  parseArgs args sizeArgs    
  
parseArgs args sizeArgs 
  | sizeArgs == 0 = usageMessage
  | otherwise = case (args!!0) of 
    "keygen" -> case sizeArgs of
      1 -> writeKeys
      2 -> writeNamedKeys (args!!1)
    "encrypt" -> case sizeArgs of
      3 -> do
        message <- readFile (args!!2)
        key <- (readFile (args!!1) >>= return.read)
        encrMes <- return $ encrypt message key
        writeFile ((args!!2)++ ".encr") encrMes
      _ -> usageMessage
    "decrypt" -> case sizeArgs of
      3 -> do
        message <- readFile (args!!2)
        key <- (readFile (args!!1) >>= return.read)
        putStrLn $ decrypt message key
      _ -> usageMessage
    _ -> usageMessage