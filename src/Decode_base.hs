module Decode where
import Utility
import Data.Bits
import Data.Maybe
import Data.List
--import Data.Function.Memoize
type Register = Int

signExtend :: Int -> Integer -> Integer
signExtend l n = if testBit n (l-1)
                 then n-2^l
                 else n

getRd inst = fromIntegral $ bitSlice inst 7 12
getRs1 inst = fromIntegral $ bitSlice inst 15 20
getRs2 inst = fromIntegral $ bitSlice inst 20 25
getPred inst = bitSlice inst 24 28
getSucc inst = bitSlice inst 20 24
getImm20 inst = shift (bitSlice inst 12 32) 12
getOimm20 inst = shift (bitSlice inst 12 32) 12
getJimm20 inst = signExtend 21 $ shift (bitSlice inst 31 32) 20 .|. shift (bitSlice inst 21 31) 1 .|.
                 shift (bitSlice inst 20 21) 11 .|. shift (bitSlice inst 12 20) 12
getImm12 inst = signExtend 12 $ bitSlice inst 20 32
getOimm12 inst = signExtend 12 $ bitSlice inst 20 32
getCsr12 inst = fromIntegral $ bitSlice inst 20 32
getSimm12 inst = signExtend 12 $ shift (bitSlice inst 25 32) 5 .|. bitSlice inst 7 12
getSbimm12 inst = signExtend 13 $ shift (bitSlice inst 31 32) 12 .|. shift (bitSlice inst 25 31) 5 .|.
                  shift (bitSlice inst 8 12) 1 .|. shift (bitSlice inst 7 8) 11
getShamt5 inst = bitSlice inst 20 25
getShamt6 inst = bitSlice inst 20 26
getZimm inst = bitSlice inst 15 20

decode :: Integer -> Instruction
decode = decode_notmemo
decode_notmemo :: Integer -> Instruction
decode_notmemo inst = (fst $ fromJust $ find (\e -> all match (snd e)) opcodeTable) inst
              where match (start, end, val) = bitSlice inst start end == val

-- Auto-generated code begins here. --
