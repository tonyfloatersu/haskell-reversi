module BasicM where

size :: Int
size    = 8

data PVEPlayer = Computer | Human
               deriving ( Eq, Enum )

instance Show PVEPlayer where
    show p    = [ "AI", "Human" ] !! fromEnum p

data PVPPlayer = Human1 | Human2
               deriving ( Eq, Enum, Show )

data Color = White | Black | Next
             deriving ( Eq, Enum )

instance Show Color where
    show c    = [ "O", "X", "*" ] !! fromEnum c

retColor :: Maybe Hand -> Maybe Color
retColor h    = h >>= \Hand { loc = _, clr = c } -> Just c

negnate :: Maybe Color -> Maybe Color
negnate (Just Next)    = Nothing
negnate color          = color >>= \x -> if x == White then Just Black else Just White

type Location = ( Int, Int )

data Hand = Hand { loc :: Location
                 , clr :: Color }

instance Eq Hand where
    Hand { loc = l1, clr = c1 } == Hand { loc = l2, clr = c2 }    = l1 == l2 && c1 == c2

instance Show Hand where
    show Hand { loc = l, clr = c }    = "(" ++ ([ show . fst
                                                , const ", "
                                                , show . snd ] >>= ($ l))
                                        ++ ", " ++ show c ++ ")"

showH :: Maybe Hand -> String
showH (Just h)    = show h
showH _           = " "

prompt :: String -> (String -> Bool) -> IO String
prompt command judgef    = putStrLn command >> getLine >>=
                           \x -> if judgef x then return x else prompt command judgef

modeChoice :: String
modeChoice    = "\nFirst please choose game mode: PVE (1) or PVP (2)"

roleChoice :: String
roleChoice    = "\nWould you like to be offensive (O) or defensive (X)?"

noChooseHint :: String
noChooseHint    = "\nIn PVP mode you need to decide who goes first; \
                   \the first player becomes the white (O) one."

{- we insert new hands into the front -}

type Hands = [Maybe Hand]

type HumanHands = Hands

type ComputerHands = Hands

type Human1Hands = Hands

type Human2Hands = Hands

data Operations = Oper Hands Hands

{- we add new steps at front and remove new steps at front -}

showOpS :: Int -> Operations -> String
showOpS n (Oper h a) | null h && null a    = []
                     | null h              = "step " ++ show n ++ "\t\t"
                                             ++ "\t\t" ++ (showH . last) a ++ "\n"
                                             ++ showOpS (n + 1) (Oper h (init a))
                     | null a              = "step " ++ show n ++ "\t\t"
                                             ++ (showH . last) h ++ "\t\t\n"
                                             ++ showOpS (n + 1) (Oper (init h) a)
                     | otherwise           = "step " ++ show n ++ "\t\t"
                                             ++ (showH . last) h ++ "\t\t"
                                             ++ (showH . last) a ++ "\n"
                                             ++ showOpS (n + 1) (Oper (init h) (init a))

instance Show Operations where
    show (Oper h a)    = "\t\t\tP1\t\t\t\tP2\n" ++ showOpS 0 (Oper (reverse h) (reverse a))

backWardRegret :: Operations -> Operations
backWardRegret (Oper h a) | null h && null a    = Oper [] []
                          | null h              = Oper [] (tail a)
                          | null a              = Oper (tail h) []
                          | otherwise           = Oper (tail h) (tail a)
