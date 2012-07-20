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

-- | The crypto modul.
module RSAModul.Crypto where
  
import RSAModul.Key
import Char
import Data.Text as T
import Data.Text.Encoding as E
import Data.ByteString (ByteString)
  
encrypt :: String -> Key -> String
encrypt str (Key v n) = unpack $ T.map (\c -> chr (fromIntegral ((charToInt c)^v `mod` n)::Int)) text
  where
    text = pack str
    --convert Char->Int->Integer
    charToInt c = fromIntegral (fromEnum c)    
