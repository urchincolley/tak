module TakTests where

import Tak
import TakTypes

b6 = newBoard 6
top = Top
fw = Flat White Top
cw = Cap White
sw = Stand White

fwfw = stackStack fw fw
fwfwfw = stackMaybeStack fwfw fw
fwfwcw = stackMaybeStack fwfw cw

stackMaybeStack :: Maybe Stack -> Stack -> Maybe Stack
stackMaybeStack ms ts = do
  bs <- ms
  stackStack bs ts

type TestCase a b = (a, b)

testCases :: Eq b => [(a, b)] -> (a -> b) -> [Bool]
testCases cs tf = map (\c -> (tf . fst) c == snd c) cs

testEmptyBoard :: Int -> [Bool]
testEmptyBoard n = let
  b  = emptyBoard n
  cs = [ ((1,1),   Just Top)
       , ((1,n),   Just Top)
       , ((n,1),   Just Top)
       , ((n,n),   Just Top)
       , ((0,1),   Nothing)
       , ((n+1,1), Nothing)
       , ((1,0),   Nothing)
       , ((1,n+1), Nothing) ] in
  testCases cs b


