module BasicM where

size :: Int
size    = 8

data PVEPlayer = Computer | Human
               deriving ( Eq, Enum )

instance Show PVEPlayer where
    show p    = [ "AI", "Human" ] !! fromEnum p

data PVPPlayer = Human1 | Human2
               deriving ( Eq, Enum, Show )

data Color = White | Black | NULL
           deriving ( Eq, Enum )

instance Show Color where
    show c    = [ "O", "X", "#" ] !! fromEnum c

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

prompt :: String -> (String -> Bool) -> IO String
prompt command judgef    = putStrLn command >> getLine >>=
                           \x -> if judgef x
                                 then return x
                                 else prompt command judgef

modeChoice :: String
modeChoice    = "\nFirst please choose game mode: PVE (1) or PVP (2)"

roleChoice :: String
roleChoice    = "\nWould you like to be offensive (O) or defensive (X)?"

noChooseHint :: String
noChooseHint    = "\nIn PVP mode you need to decide who goes first; \
                   \the first player becomes the white (O) one."

{- we insert new hands into the front -}

type Hands = [ Hand ]

type HumanHands = Hands

type ComputerHands = Hands

type Human1Hands = Hands

type Human2Hands = Hands

data Operations = Oper Hands Hands

{- Let the empty hand for no place to go as ((-1, -1), ) -}

showOp :: Operations -> String
showOp (Oper h1 h2) | null h1 && null h2    = []
                    | null h1               = "\t\t" ++ (show . head) h2 ++ "\n"
                                              ++ showOp (Oper h1 (tail h2))
                    | null h2               = (show . head) h1 ++ "\t\t\n"
                                              ++ showOp (Oper (tail h1) h2)
                    | otherwise             = (show . head) h1 ++ "\t\t" ++ (show . head) h2
                                              ++ "\n" ++ showOp (Oper (tail h1) (tail h2))

instance Show Operations where
    show (Oper h1 h2)    = "P1\t\t\t\tP2\n" ++ showOp (Oper (reverse h1) (reverse h2))

backWardRegret :: Operations -> Operations
backWardRegret (Oper h1 h2) | null h1 && null h2    = Oper [] []
                            | null h1               = Oper [] (tail h2)
                            | null h2               = Oper (tail h1) []
                            | otherwise             = Oper (tail h1) (tail h2)
