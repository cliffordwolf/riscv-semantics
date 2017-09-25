{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
import Program
import Utility
import Data.Maybe
import Data.Int
import Data.Word
import Data.Bits
import Execute
import Decode
import Control.Applicative
import Control.Monad


-----------------------------------------
-- Machine Model

newtype MState s a = MState { runState :: s -> Maybe (a, s) }

instance Functor (MState s) where
  fmap f a = MState $ \state -> fmap (\(b,s) -> (f b, s)) (runState a state)

instance Applicative (MState s) where
  pure x = MState $ \state -> Just (x, state)
  (<*>) f a = MState $ \state -> do
    (g, s1) <- runState f state
    (b, s2) <- runState a s1
    return (g b, s2)

instance Monad (MState s) where
  (>>=) a f = MState $ \state -> runState a state >>= (\(b, s) -> runState (f b) s)

instance Alternative (MState s) where
  empty = MState $ \state -> Nothing
  (<|>) a b = MState $ \state -> runState a state <|> runState b state

instance MonadPlus (MState s)

data MyComputer = MyComputer { registers :: [Int32], pc :: Int32, nextPC :: Int32} deriving (Show)

instance RiscvProgram (MState MyComputer) Int32 Word32 where
  getRegister reg = MState $ \comp -> Just (if reg == 0 then 0 else (registers comp) !! (fromIntegral reg-1), comp)
  setRegister reg val = MState $ \comp -> Just ((), if reg == 0 then comp else comp { registers = setIndex (fromIntegral reg-1) (fromIntegral val) (registers comp) })
  loadByte addr = MState $ \comp -> Just (0, comp) -- FIXME: Make the values read from mem unconstrained
  loadHalf addr = MState $ \comp -> Just (0, comp) -- ^^^
  loadWord addr = MState $ \comp -> Just (0, comp) -- ^^^
  storeByte addr val = MState $ \comp -> Just ((), comp)
  storeHalf addr val = MState $ \comp -> Just ((), comp)
  storeWord addr val = MState $ \comp -> Just ((), comp)
  loadCSR addr = MState $ \comp -> Just (0, comp) -- FIXME: Make the values read from CSRs unconstrained
  storeCSR addr val = MState $ \comp -> return ((), comp)
  getPC = MState $ \comp -> Just (pc comp, comp)
  setPC val = MState $ \comp -> Just ((), comp { nextPC = fromIntegral val })
  step = MState $ \comp -> Just ((), comp { pc = nextPC comp })


-----------------------------------------
-- Simple Demo

pre = MyComputer { registers = (take 31 $ repeat 0), pc = 5, nextPC = 0 }
post = snd $ fromJust $ runState (execute (Lui 1 19)) pre

main = do
  putStrLn $ show pre
  putStrLn $ show post

