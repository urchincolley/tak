module TakTypes where

data Player = White | Black
  deriving (Eq, Ord, Show)

data Stack 
  = Top
  | Cap   Player
  | Stand Player
  | Flat  Player Stack
    deriving (Eq, Ord, Show)

data Stone
  = CapStone
  | StandingStone
  | FlatStone
    deriving (Eq, Ord, Show)

type Row    = Int
type Column = Int
type Pos = (Row, Column)

type Board = Pos -> Maybe Stack
