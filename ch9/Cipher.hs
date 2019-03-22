module Cipher where

-- Chapters 9 and 11 Chapter exercises
-- pg 340
-- pg 453

import Data.Char
import Data.List

uppers :: String
uppers = "ABCDEFGHIJKLMNOPQRSTUVWXYZ "

offset :: Int
offset = ord 'A'

-- caesar cipher
caesar :: Int -> Message -> Message
caesar n (Message plain) = Message $ map (rotateChar n . toUpper) plain

unCaesar :: Int -> Message -> Message
unCaesar n cipher = caesar (-n) cipher

rotateChar :: Int -> Char -> Char
rotateChar n plain =
    let pVal      = ord plain
        zeroBased = pVal - offset
    in if (ord 'A') <= pVal && pVal <= (ord 'Z')
       then chr $ ((zeroBased + n) `mod` 26) + offset
       else plain


newtype CipherKey = CipherKey String deriving Show
newtype Message   = Message String deriving Show

-- Vigenere cipher
vigenRaw :: Int -> CipherKey -> Message -> Message
vigenRaw sign (CipherKey key) (Message plain) =
    let uKey    = map toUpper key
        uPlain  = map toUpper plain
        fullKey = expandKey (CipherKey uKey) (Message uPlain)
        ordKey  = map ((+ (-offset)) . ord) fullKey
    in  Message $ zipWith rotateChar (map (* sign) ordKey) uPlain

vigen :: CipherKey -> Message -> Message
vigen k m = vigenRaw 1 k m

unVigen :: CipherKey -> Message -> Message
unVigen k m = vigenRaw (-1) k m

expandKey :: CipherKey -> Message -> String
expandKey (CipherKey key) (Message plain) =
    take (length plain) $ concat $ iterate id key
