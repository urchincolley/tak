module Tak where

import TakTypes

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe xs = Just $ head xs

find :: (a -> Bool) -> [a] -> Maybe a
find p xs = headMaybe $ take 1 $ filter p xs

allTrue :: [a -> b -> Bool] -> a -> b -> Bool
allTrue fs x y = foldr (&&) True $ map ($ y) $ map ($ x) fs

-- rows and cols are indexed by 1 because people think that way (?)
posOk :: Int -> Pos -> Bool
posOk n p = fst p > 0 && fst p <= n && snd p > 0 && snd p <= n

emptyBoard :: Int -> Board
emptyBoard n p = if posOk n p then Just Top else Nothing

newBoard :: Int -> Maybe Board
newBoard n
  | n < 3 || n > 8 = Nothing
  | otherwise      = Just $ emptyBoard n

newStack :: Player -> Stone -> Stack
newStack p s
  | s == CapStone      = Cap p
  | s == StandingStone = Stand p
  | s == FlatStone     = Flat p Top

stackHeight :: Stack -> Int
stackHeight Top        = 0
stackHeight (Cap _)    = 1
stackHeight (Stand _)  = 1
stackHeight (Flat _ t) = 1 + stackHeight t

stackCtrler :: Stack -> Maybe Player
stackCtrler Top          = Nothing
stackCtrler (Cap p)      = Just p
stackCtrler (Stand p)    = Just p
stackCtrler (Flat p Top) = Just p
stackCtrler (Flat _ ss)  = stackCtrler ss

compareToMaybe :: Eq a => Maybe a -> a -> Bool
compareToMaybe Nothing  _ = False
compareToMaybe (Just x) y = x == y 

-- Determine if the board position at po is controlled by player pl
srcOk :: Board -> Player -> Pos -> Bool
srcOk b pl po = compareToMaybe (b po >>= stackCtrler) pl

-- Combine two stacks if the combination is legal (bottom stack -> top stack -> result)
stackStack :: Stack -> Stack -> Maybe Stack
stackStack Top t               = Just t 
stackStack (Stand p) t@(Cap _) = Just $ Flat p t
stackStack (Flat p s) t        = Flat p <$> stackStack s t
stackStack _ _                 = Nothing

-- Returns the top n stones of the stack (if stack contains at least n stones)
takeStack :: Int -> Stack -> Maybe Stack
takeStack n s = go (stackHeight s - n) $ Just s
  where go :: Int -> Maybe Stack -> Maybe Stack
        go 0 s                 = s
        go n (Just (Flat _ s)) = go (n - 1) $ Just s
        go _ _                 = Nothing

-- Returns the stack with the top n stones removed (if stack contains at least n stones)
dropStack :: Int -> Stack -> Maybe Stack
dropStack n s = go (stackHeight s - n) $ Just s
  where go :: Int -> Maybe Stack -> Maybe Stack
        go 0 s                  = Just Top
        go 1 (Just (Cap _))     = Just Top
        go 1 (Just (Stand _))   = Just Top
        go n (Just (Flat p ss)) = Just . (Flat p) =<< go (n-1) (Just ss)
        go _ _                  = Nothing

-- Add a new stack to the board (if the add is legal)
addNewStack :: Maybe Board -> Player -> Stone -> Pos -> Maybe Board
addNewStack mb pl s po = do
  b <- mb
  if compareToMaybe (b po) Top then
    Just (\p -> if (p == po) then Just (newStack pl s) else b p)
  else
    Nothing

-- Move a single substack to another position
-- TODO: check position adjacency 
simpleMove :: Maybe Board -> Player -> Pos -> Int -> Pos -> Maybe Board
simpleMove mb pl src n dst = do
  b <- mb
  bsrc <- b src
  bdst <- b dst
  mov <- takeStack n bsrc
  let
    sok = srcOk b pl src
    bsrc' = dropStack n bsrc
    bdst' = stackStack bdst mov in
    if not sok || bsrc' == Nothing || bdst' == Nothing then
      Nothing
    else
      Just nb where nb src = bsrc'
                    nb dst = bdst'
                    nb po  = b po
