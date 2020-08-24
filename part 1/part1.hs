import System.IO


dayOfWeek :: Integer -> Integer -> Integer -> Integer
dayOfWeek year month day
	| month < 3 = zellerCong (year-1) month day
	| otherwise = zellerCong year month day
	where
		zellerCong :: Integer -> Integer -> Integer -> Integer
		zellerCong year month day = (day + floor (fromIntegral (13 * (m' + 1)) / 5.0) + floor(k) + floor(k/4.0) + floor(j/4.0) + floor (5.0*j)) `mod` 7
			where
				m' = if month < 3 then month + 12 else month
				k = fromIntegral(year `mod` 100)
				j = fromIntegral(floor(fromIntegral(year)/100.0))	--finally convert it to the Float in order to increase readibility of the zellerCong line on above


sundays1 :: Integer -> Integer -> Integer
sundays1 start end = sundays' start 1
	where
		sundays' :: Integer -> Integer -> Integer
		sundays' y m
			| y > end = 0
			| otherwise = if dayOfWeek y m 1 == 1 then rest + 1 else rest
			where
				nextY = if m == 12 then y + 1 else y
				nextM = if m == 12 then 1 else m+1
				rest = sundays' nextY nextM


sundays1tr :: Integer -> Integer -> Integer
sundays1tr start end = sundaystr' 0 start 1
	where
		sundaystr' :: Integer -> Integer -> Integer -> Integer
		sundaystr' acc y m
			| y > end = acc
			| otherwise = sundaystr' nextAcc nextY nextM
			where
				nextAcc = if dayOfWeek y m 1 == 1 then (acc+1) else acc
				nextY = if m == 12 then y + 1 else y
				nextM = if m == 12 then 1 else m+1


leap :: Integer -> Bool
leap y = (y `mod` 4 == 0) && (y `mod` 100 /= 0) || (y `mod` 400 == 0)


daysInMonth :: Integer -> Integer -> Integer
daysInMonth m y = case m of
	4 -> 30
	6 -> 30
	9 -> 30
	11-> 30
	2 -> if leap y then 29 else 28
	_ -> 31


sundays2 :: Integer -> Integer -> Integer
sundays2 start end = sundays2' 0 2 start 1
	where
		sundays2' :: Integer -> Integer -> Integer -> Integer -> Integer
		sundays2' acc weekday y m
			| y > end = acc
			| otherwise = sundays2' nextAcc nextWeekday nextY nextM
			where
				nextWeekday = weekday + (daysInMonth m y `mod` 7)
				nextAcc = if nextWeekday `mod` 7 == 0 then (acc+1) else acc
				nextY = if m == 12 then y+1 else y
				nextM = if m == 12 then 1 else m+1


getFunction :: String -> (Integer -> Integer -> Integer)
getFunction name
	| name == "sundays1"   = sundays1
	| name == "sundays1tr" = sundays1tr
	| name == "sundays2"   = sundays2
	| otherwise            = error "unknown function"


main :: IO ()
main = do
	line <- getLine
	let [f, start, end] = words line
	putStrLn $ show $ (getFunction f) (read start :: Integer) (read end :: Integer)

