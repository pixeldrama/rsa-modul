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

module RSAModul.Crack where

import RSAModul.Helper
import RSAModul.Key

getPrivateKey :: Key -> Key
getPrivateKey (Key v m)  =  getPrivateKey' [1.. ((phi $ primeFactorization m) - 1)] v m 
  where 
    getPrivateKey' [] _ _ = error "key not found"
    getPrivateKey' (l:ls) v m  
      | l*v `mod` m == 1 = Key l m
      | otherwise = getPrivateKey' ls v m