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

-- | The crypto modul. There can be done some optimizations, because
-- the modulo operation could be calcalute faster than the haskell
-- function mod do.
module RSAModul.Crypto where
  
import RSAModul.Key
import Codec.Binary.Base64.String
import Data.List.Split
import Data.Char
  
encrypt :: String -> Key -> String
encrypt str (Key v n) = encode $ concat $ map (\c -> (show $ fromIntegral ((charToInt c)^v `mod` n)) ++ ",") str
  where
    --convert Char->Int->Integer
    charToInt c = fromIntegral (fromEnum c)    

decrypt :: String -> Key -> String
decrypt str (Key v n) = conv $ filtList str
  where
    conv l = map (\x -> chr $ fromIntegral (((read x) :: Integer)^v `mod` n)) l
    filtList str = filter (\x -> x /= "=" && x /= "") $ splitOn "," $ decode str
