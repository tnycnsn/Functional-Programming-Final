import System.IO
import Data.Char


data Color = Red | Black
	deriving (Show, Eq)

data Suit = Clubs | Diamonds | Hearts | Spades
	deriving (Show, Eq)

data Rank = Num Int | Jack | Queen | King | Ace
	deriving (Show, Eq)

data Card = Card { suit :: Suit, rank :: Rank }
	deriving (Show, Eq)

data Move = Draw | Discard Card
	deriving (Show, Eq)


cardColor :: Card -> Color
cardColor card = case suit card of
	Clubs  -> Black
	Spades -> Black
	_      -> Red
	

cardValue :: Card -> Integer
cardValue card
	| r == Ace                             = 11
	| r == Jack || r == Queen || r == King = 10
	| otherwise                            = read (show r) :: Integer
	where
		r = rank card


removeCard :: Card -> [Card] -> [Card]
removeCard _ [] = error "card is not in list"
removeCard c (c':cs) = if c == c' then cs else c':removeCard c cs


allSameColor :: [Card] -> Bool
allSameColor cs = case cs of
	[]           -> True  -- can be changed
	[_]          -> True
	c1:cs@(c2:_) -> if cardColor c1 /= cardColor c2 then False else allSameColor cs


sumCards:: [Card] -> Integer
sumCards cs = sum' 0 cs
	where
		sum' :: Integer -> [Card] -> Integer
		sum' acc cards
			| null cards = acc
			| otherwise  = sum' (acc + cardValue (head cards)) (tail cards)


score :: [Card] -> Integer -> Integer
score hand goal
	| allSameColor hand = floor ((fromIntegral $ preliminary hand  goal) / 2.0)
	| otherwise         = preliminary hand goal
	where
		preliminary :: [Card] -> Integer -> Integer
		preliminary hand goal = if sum > goal then 3*(sum-goal) else goal-sum
			where
				sum = sumCards hand


data State = Start | End | Continue
	deriving (Show, Eq)


runGame :: [Card] -> [Move] -> Int -> Int
runGame cs ms goal = helper Start [] cs ms --start with empty held cards
	where
		helper :: State -> [Card] -> [Card] -> [Move] -> Int
		helper st held cs' ms'
			| st == End                                                                = fromIntegral (score held (fromIntegral goal ::Integer)) :: Int
			| (null ms') || goal < (fromIntegral (sumCards held) :: Int) || (null cs') = helper End held cs' ms'
			| otherwise                                                                = if m1 == Draw then helper Continue (c1:held) ct mt else helper Continue held (discarding m1 cs') mt
			where
				c1:ct = cs'
				m1:mt = ms'
				extractCard :: Move -> Card
				extractCard (Discard c) = c
					
				discarding :: Move -> [Card] -> [Card]
				discarding _ []                    = error "card not in card-list"	--If reached very end, card is not in list
				discarding move cardList@(cL1:cLt) = if cL1 == (extractCard move) then cLt else cL1:(discarding move cLt) --Iterate till find the card whenever find it concat rest without it


convertSuit :: Char -> Suit
convertSuit s
	| s == 'c' || s == 'C' = Clubs
	| s == 'd' || s == 'D' = Diamonds
	| s == 'h' || s == 'H' = Hearts
	| s == 's' || s == 'S' = Spades
	| otherwise   	         = error "Unknown Suit"


convertRank :: Char -> Rank
convertRank r
	| isDigit r            = if r == '1' then Ace else Num (digitToInt r)
	| r == 't' || r == 'T' = Num 10
	| r == 'j' || r == 'J' = Jack
	| r == 'q' || r == 'Q' = Queen
	| r == 'k' || r == 'K' = King
	| otherwise            = error "Unknown Rank"


convertCard :: Char -> Char -> Card
convertCard s r = Card (convertSuit s) (convertRank r)


readCards :: IO [Card]
readCards = readCards' []
	where
		readCards' :: [Card] -> IO [Card]
		readCards' cs = do
			ch1 <- getLine
			if (ch1 == ".")
				then return cs
				else do let s:r:_ = ch1
					let card = convertCard s r
					readCards' (card:cs)


convertMove :: Char -> Char -> Char -> Move
convertMove m s r
	| m == 'd' || m == 'D' = Draw
	| m == 'r' || m == 'R' = Discard (convertCard s r)


readMoves :: IO [Move]
readMoves = readMoves' []
	where
		readMoves' :: [Move] -> IO [Move]
		readMoves' ms = do
			ch1 <- getLine
			if (ch1 == ".")
				then return ms
				else do let m:rest = ch1
					if m == 'd'
						then do
							let move = convertMove m '.' '.'
							readMoves' (move:ms)
						else do
							let s:r:_ = rest
							let move = convertMove m s r
							readMoves' (move:ms)


main :: IO ()
main = do
	putStrLn "Enter cards:"
	cards <- readCards
	putStrLn (show cards)
	putStrLn "Enter moves:"
	moves <- readMoves
	putStrLn (show moves)
	putStrLn "Enter goal:"
	line <- getLine
	let goal = read line :: Int
	let score = runGame cards moves goal
	putStrLn ("Score: " ++ show score)

