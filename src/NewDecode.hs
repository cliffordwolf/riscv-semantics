import Decode
import Utility
import Data.Word
import Data.Maybe


-----------------------------------------
-- Mockup for new decode

data NewInstruction =
  NewAddi { len :: Integer, rd :: Register, rs1 :: Register, imm12 :: Integer } |
  NewSlti { len :: Integer, rd :: Register, rs1 :: Register, imm12 :: Integer } |
  NewSltiu { len :: Integer, rd :: Register, rs1 :: Register, imm12 :: Integer }
  deriving (Eq, Read, Show)

getOpcode inst = fromIntegral $ bitSlice inst 0 7
getFunct3 inst = fromIntegral $ bitSlice inst 12 15

newDecode :: [Word8] -> Maybe NewInstruction

newDecode bytes@(b0:b1:b2:b3:_)
    | (getOpcode inst) == 0x13 && (getFunct3 inst) == 0 = Just $ NewAddi 4 (getRd inst) (getRs1 inst) (getImm12 inst)
    | (getOpcode inst) == 0x13 && (getFunct3 inst) == 2 = Just $ NewSlti 4 (getRd inst) (getRs1 inst) (getImm12 inst)
    | (getOpcode inst) == 0x13 && (getFunct3 inst) == 3 = Just $ NewSltiu 4 (getRd inst) (getRs1 inst) (getImm12 inst)
  where
    inst = (combineBytes $ take 4 bytes)

newDecode _ = Nothing

decode32 inst = fromJust $ newDecode (splitWord inst)


-----------------------------------------
-- Test

inst_integer = 0x00150513
inst_bytes = [0x13, 0x05, 0x15, 0x00, 0xff, 0xff]

main = do
  putStrLn $ show $ decode inst_integer
  putStrLn $ show $ decode32 inst_integer
  putStrLn $ show $ newDecode inst_bytes
  putStrLn $ show $ newDecode [0x13, 0x05, 0x15]

