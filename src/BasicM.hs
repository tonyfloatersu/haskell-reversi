module BasicM where

data Player = Computer | Human
            deriving ( Eq, Enum )

instance Show Player where
    show p    = [ "AI", "Human" ] !! fromEnum p

data Color = Black | White | NULL
           deriving ( Eq, Enum, Show )

negnate :: Color -> Color
negnate c | c == White    = Black
          | c == NULL     = NULL
          | otherwise     = White

type Location = ( Int, Int )

data Hand = Hand { loc :: Location
                 , color :: Color }

instance Eq Hand where
    Hand { loc = l1, color = c1 } == Hand { loc = l2, color = c2 }    = l1 == l2 && c1 == c2

instance Show Hand where
    show Hand { loc = l, color = c }    = "(" ++ concat ([ show . fst
                                                         , const ", "
                                                         , show . snd ] <*> [l])
                                          ++ ", " ++ show c ++ ")"
