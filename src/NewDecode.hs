import Decode
import Utility
import Data.Word
import Data.Maybe


-----------------------------------------
-- Mockup for new decode

getOpcode inst = fromIntegral $ bitSlice inst 0 7
getFunct3 inst = fromIntegral $ bitSlice inst 12 15

newDecode :: [Word8] -> (Maybe Instruction, [Word8])

newDecode (b0:b1:b2:b3:rest)
    | (getOpcode inst) == 0x13 && (getFunct3 inst) == 0 = (Just $ Addi (getRd inst) (getRs1 inst) (getImm12 inst), rest)
    | (getOpcode inst) == 0x13 && (getFunct3 inst) == 2 = (Just $ Slti (getRd inst) (getRs1 inst) (getImm12 inst), rest)
    | (getOpcode inst) == 0x13 && (getFunct3 inst) == 3 = (Just $ Sltiu (getRd inst) (getRs1 inst) (getImm12 inst), rest)
  where
    inst = (combineBytes [b0, b1, b2, b3])

newDecode rest = (Nothing, rest)

decode32 inst = fromJust $ fst $ newDecode (splitWord inst)


-----------------------------------------
-- Test

inst_integer = 0x00150513
inst_bytes = [0x13, 0x05, 0x15, 0x00, 0xff, 0xff]

main = do
  putStrLn $ show $ decode inst_integer
  putStrLn $ show $ decode32 inst_integer
  putStrLn $ show $ newDecode inst_bytes

