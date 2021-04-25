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

testStackStack :: [Bool]
testStackStack = let
  cs = [ ((Top, Flat White Top), Just $ Flat White Top)
       , ((Stand Black, Cap White), Just $ Flat Black (Cap White))
       , ((Flat White (Flat Black Top), Cap White), Just $ Flat White (Flat Black (Cap White)))
       , ((Flat White (Cap White), Flat Black Top), Nothing)
       , ((Stand Black, Stand White), Nothing)
       , ((Stand Black, Top), Nothing) ]
  tf c = stackStack (fst c) (snd c) in
  testCases cs tf

testTakeStack :: [Bool]
testTakeStack = let
  cs = [ ((1, Cap White), Just $ Cap White)
       , ((1, Flat Black (Cap White)), Just $ Cap White)
       , ((1, Flat Black Top), Just $ Flat Black Top)
       , ((0, Cap White), Nothing)
       , ((2, Flat Black Top), Nothing)
       , ((1, Top), Nothing)
       , ((0, Top), Nothing)
       , ((-1, Top), Nothing) ]
  tf c = takeStack (fst c) (snd c) in
  testCases cs tf

testDropStack :: [Bool]
testDropStack = let
  cs = [ ((0, Cap White), Just $ Cap White)
       , ((0, Top), Just Top)
       , ((1, Cap White), Just Top)
       , ((1, Stand White), Just Top)
       , ((1, Flat White Top), Just Top)
       , ((1, Flat White (Cap Black)), Just $ Flat White Top)
       , ((2, Flat White (Flat Black (Cap Black))), Just $ Flat White Top)
       , ((1, Flat White (Flat Black (Cap Black))), Just $ Flat White (Flat Black Top))
       , ((2, Cap White), Nothing)
       , ((-1, Cap White), Nothing)
       , ((1, Top), Nothing) ]
  tf c = dropStack (fst c) (snd c) in
  testCases cs tf
