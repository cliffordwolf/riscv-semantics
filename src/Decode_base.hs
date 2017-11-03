module Decode where
import Utility
import Data.Bits
import Data.Int
import Data.Maybe
import Data.List

type Register = MachineInt

signExtend :: Int -> MachineInt -> MachineInt
signExtend l n = if testBit n (l-1)
                 then n-2^l
                 else n

getRd :: MachineInt -> MachineInt
getRd inst = fromIntegral $ bitSlice inst 7 12
getRs1 :: MachineInt -> MachineInt
getRs1 inst = fromIntegral $ bitSlice inst 15 20
getRs2 :: MachineInt -> MachineInt
getRs2 inst = fromIntegral $ bitSlice inst 20 25
getPred :: MachineInt -> MachineInt
getPred inst = bitSlice inst 24 28
getSucc :: MachineInt -> MachineInt
getSucc inst = bitSlice inst 20 24
getImm20 :: MachineInt -> MachineInt
getImm20 inst = shift (bitSlice inst 12 32) 12
getOimm20 :: MachineInt -> MachineInt
getOimm20 inst = shift (bitSlice inst 12 32) 12
getJimm20 :: MachineInt -> MachineInt
getJimm20 inst = signExtend 21 $ shift (bitSlice inst 31 32) 20 .|. shift (bitSlice inst 21 31) 1 .|.
                 shift (bitSlice inst 20 21) 11 .|. shift (bitSlice inst 12 20) 12
getImm12 :: MachineInt -> MachineInt
getImm12 inst = signExtend 12 $ bitSlice inst 20 32
getOimm12 :: MachineInt -> MachineInt
getOimm12 inst = signExtend 12 $ bitSlice inst 20 32
getCsr12 :: MachineInt -> MachineInt
getCsr12 inst = fromIntegral $ bitSlice inst 20 32
getSimm12 :: MachineInt -> MachineInt
getSimm12 inst = signExtend 12 $ shift (bitSlice inst 25 32) 5 .|. bitSlice inst 7 12
getSbimm12 :: MachineInt -> MachineInt
getSbimm12 inst = signExtend 13 $ shift (bitSlice inst 31 32) 12 .|. shift (bitSlice inst 25 31) 5 .|.
                  shift (bitSlice inst 8 12) 1 .|. shift (bitSlice inst 7 8) 11
getShamt5 :: MachineInt -> MachineInt
getShamt5 inst = bitSlice inst 20 25
getShamt6 :: MachineInt -> MachineInt
getShamt6 inst = bitSlice inst 20 26
getZimm :: MachineInt -> MachineInt
getZimm inst = bitSlice inst 15 20

decode :: MachineInt -> Instruction
decode inst = (fst $ fromMaybe (\x -> InvalidInstruction ,undefined) $ find (\e -> all match (snd e)) opcodeTable) inst
  where match (start, end, val) = bitSlice inst start end == val

-- Auto-generated code begins here. --
