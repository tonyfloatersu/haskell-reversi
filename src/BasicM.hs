module BasicM where

size :: Int
size    = 8

data PVEPlayer = Computer | Human
               deriving ( Eq, Enum )

instance Show PVEPlayer where
    show p    = [ "AI", "Human" ] !! fromEnum p

data PVPPlayer = Human1 | Human2
               deriving ( Eq, Enum, Show )

data Color = White | Black | NULL | Next
             deriving ( Eq, Enum )

instance Show Color where
    show c    = [ "O", "X", " ", "*" ] !! fromEnum c

negnate :: Color -> Color
negnate c | c == White    = Black
          | c == NULL     = NULL
          | c == Next     = Next
          | otherwise     = White

type Location = ( Int, Int )

data Hand = Hand { loc :: Location
                 , clr :: Color }

instance Eq Hand where
    Hand { loc = l1, clr = c1 } == Hand { loc = l2, clr = c2 }    = l1 == l2 && c1 == c2

instance Show Hand where
    show Hand { loc = l, clr = c }    = "(" ++ concat ([ show . fst
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

type Hands = [Hand]

type HumanHands = Hands

type ComputerHands = Hands

type Human1Hands = Hands

type Human2Hands = Hands

data Operations = Oper Hands Hands

{- Let the empty hand for no place to go as ((-1, -1), #) -}

showOp :: Operations -> String
showOp (Oper h a) | null h && null a    = []
                  | null h              = "\t\t" ++ (show . head) a ++ "\n"
                                          ++ showOp (Oper h (tail a))
                  | null a              = (show . head) h ++ "\t\t\n"
                                          ++ showOp (Oper (tail h) a)
                  | otherwise           = (show . head) h ++ "\t\t" ++ (show . head) a
                                          ++ "\n" ++ showOp (Oper (tail h) (tail a))

instance Show Operations where
    show (Oper h a)    = "P1\t\t\t\tP2\n" ++ showOp (Oper (reverse h) (reverse a))

backWardRegret :: Operations -> Operations
backWardRegret (Oper h a) | null h && null a    = Oper [] []
                          | null h              = Oper [] (tail a)
                          | null a              = Oper (tail h) []
                          | otherwise           = Oper (tail h) (tail a)
