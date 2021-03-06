{-# LANGUAGE ScopedTypeVariables #-}
module ExecuteM64 where
import Decode
import Program
import Utility
import Control.Monad

execute :: forall p t u. (RiscvProgram p t u) => Instruction -> p ()
execute (Mulw rd rs1 rs2) = do
  x <- getRegister rs1
  y <- getRegister rs2
  setRegister rd (s32 $ x * y)
execute (Divw rd rs1 rs2) = do
  x <- getRegister rs1
  y <- getRegister rs2
  let q | x == minBound && y == -1 = x
        | y == 0 = -1
        | otherwise = x `div` y
    in setRegister rd (s32 q)
execute (Divu rd rs1 rs2) = do
  x <- getRegister rs1
  y <- getRegister rs2
  let q | y == 0 = maxBound::u
        | otherwise = (unsigned x) `div` (unsigned y)
    in setRegister rd (s32 q)
execute (Remw rd rs1 rs2) = do
  x <- getRegister rs1
  y <- getRegister rs2
  let r | x == minBound && y == -1 = 0
        | y == 0 = x
        | otherwise = x `rem` y
    in setRegister rd (s32 r)
execute (Remuw rd rs1 rs2) = do
  x <- getRegister rs1
  y <- getRegister rs2
  let r | y == 0 = x
        | otherwise = fromIntegral $ (unsigned x) `rem` (unsigned y)
    in setRegister rd (s32 r)
execute _ = mzero
