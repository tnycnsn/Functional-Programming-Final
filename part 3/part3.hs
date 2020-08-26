import System.IO
import Data.Char
import Data.List


wordCharCounts :: [Char] -> [(Char, Int)]
wordCharCounts word = wordCharCount' cs' word
	where
		cs' = ['a' .. 'z']
		wordCharCount' :: [Char] -> [Char] -> [(Char, Int)]
		wordCharCount' _ [] = [] -- If word is empty no need to search
		wordCharCount' [] _ = [] -- Base cond. if end of the search list is reached, terminate
		wordCharCount' (c1:ct) word  = if count == 0 then wordCharCount' ct word else (c1, count):wordCharCount' ct word
			where
				count = length (filter (\c -> toLower(c) == c1) word)


sentenceCharCounts :: [[Char]] -> [(Char, Int)]
sentenceCharCounts wordList = (wordCharCounts . foldr (++) "") wordList	-- concat all words then count chars


dictCharCounts :: [[Char]] -> [[(Char, Int)]]
dictCharCounts []      = []
dictCharCounts (w1:wt) = (wordCharCounts w1):dictCharCounts wt


--
isEq :: [(Char, Int)] -> [(Char, Int)] -> Bool		-- checks whether CharCounts are same or Not
isEq [] []               = True
isEq leftCount []        = False
isEq [] rightCount       = False
isEq (lC1:lCt) (rC1:rCt) = if (fst lC1) == (fst rC1) && (snd lC1) == (snd rC1) then isEq lCt rCt else False
--


dictWordsByCharCounts :: [[(Char, Int)]] -> [[Char]] -> [[[Char]]]
dictWordsByCharCounts [] wordList        = []
dictWordsByCharCounts (cC1:cCt) wordList = nub $ (filter (\w -> isWord w cC1) wordList):dictWordsByCharCounts cCt wordList --iterate over charCount list and filter with that charCount; later remove dublicated items
	where
		isWord :: [Char] -> [(Char, Int)] -> Bool
		isWord word cC = isEq (wordCharCounts word) cC --cC: charCount
				

wordAnagrams :: [Char] -> [[[Char]]] -> [[Char]]
wordAnagrams word []               = []
wordAnagrams word anagrams@(a1:at) = if isEq wordCC (wordCharCounts (head a1)) then a1 else wordAnagrams word at -- tail of the anagrams
	where
		wordCC :: [(Char, Int)]
		wordCC = wordCharCounts word


charCountsSubsets :: [(Char, Int)] -> [[(Char, Int)]]
charCountsSubsets [] = [[]]
charCountsSubsets charCount@(cC1:cCt) = crossConcat (makeVary cC1) (charCountsSubsets cCt)
	where
		makeVary :: (Char, Int) -> [[(Char, Int)]]
		makeVary cC@(ch, count) = if count == 0 then [[]] else [(ch, count)]:(makeVary (ch, count-1))
		
		crossConcat :: [[(Char, Int)]] -> [[(Char, Int)]] -> [[(Char, Int)]]
		crossConcat xs ys = [x ++ y | x <- xs, y <- ys]


subtractCounts :: [(Char, Int)] -> [(Char, Int)] -> [(Char, Int)]
subtractCounts [] sndMap                 = [] 
subtractCounts fstMap@(fst1:fstT) sndMap
	| (snd fst1) - subtractAs (fst fst1) sndMap == 0 = subtractCounts fstT sndMap --if subtraction will result as extinction of the "key" delete don't concat it 
	| otherwise                                      = ((fst fst1), (snd fst1) - subtractAs (fst fst1) sndMap):(subtractCounts fstT sndMap)
	where
		subtractAs :: Char -> [(Char, Int)] -> Int
		subtractAs key []                  = 0	--Key couldn't find in secondMap so subtract 0 from that key
		subtractAs key sndMap'@(snd1:sndT) = if key == (fst snd1) then snd snd1 else subtractAs key sndT


--8
{-
sentenceAnagram :: [[Char]] -> [[[Char]]]
sentenceAnagram wordList 
	where
-}

{-
anagramFind :: [(Char, Int)] -> [[(Char, Int)]] -> [[[Char]]]
anagramFind sentCount subsetsOfSentCount@(sOSC1:sOSCt)
	| null sOSC1 = anagramFind compSOSC1 (charCountsSubsets compSOSC1)
	| null compSOSC1 = anagramFind sOSC1 (charCountsSubsets sOSC1)
	where
		
		 compSOSC1 = subtractCounts sentCount sOSC1 --find the complementery of the subset
-}	 



