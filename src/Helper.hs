module Helper where

import Data.String.Utils
import Classes
import Data.Char
import Data.Typeable
import System.Locale (defaultTimeLocale)
import System.Time (formatCalendarTime, toUTCTime, getClockTime, ClockTime)

--- Sta³e wykorzystywane przy wypisywaniu
just :: Int
just = 20
fill :: Char
fill = ' '

-- Przekszta³cenie obiektu String w obiekt Maybe
readMaybe :: (Read a) => String -> Maybe a
readMaybe s =
	case reads s of
	[(x, "")] -> Just x
	_ -> Nothing
  
readMaybeInt :: String -> Maybe Int
readMaybeInt = readMaybe

toName:: (String,String) -> (FirstName,LastName)
toName (f,l) = (f,l)
  
-- Aliasy u¿ywanych funkcji wbudowanych
--printString = putStr
--printLine = putStrLn
printNewLine = putStrLn ""
printSeparator = putStrLn "---"
printList list = mapM_ putStrLn list
tryCatch catchBody exceptBody =
	catch (catchBody)
	(\_ -> exceptBody)

--- Wczytanie danych z pliku
loadFromFile :: FilePath -> (String -> b) -> IO b
loadFromFile filePath readFunction = do
	raw <- readFile filePath
	return (readFunction (strip raw))

---Zapis danych do pliku
saveToFile :: (Show a) => a -> FilePath -> IO ()
saveToFile object filePath = writeFile filePath (show object)

-- Numeracja elementów listy
enumerate :: [a] -> Int -> [(Int, a)]
enumerate [] _ = []
enumerate (x:xs) start = [(start, x)] ++ (enumerate xs (start + 1))

-- Usniêcie zadanego elementu z listy
removeItem :: (Eq t) => t -> [t] -> [t]
removeItem _ [] = []
removeItem x (y:ys) | x == y    = ys    
                    | otherwise = y : removeItem x ys
                    
                    
-- Wyœrodkowanie ci¹gu znaków
center :: Int -> Char -> String -> String
center width fill string =
	if width <= stringLength then
		string
	else
		centeredString
	where
		stringLength = length string
		fillLength = width - stringLength
		centeredString = beginning ++ string ++ ending
		partition = divide fillLength
		beginning = takeRepeat (fst partition) fill
		ending = takeRepeat (snd partition) fill
		divide number = if mod number two == one then
							(part, complement)
						else
							(half, half)
						where
							part = half + one
							complement = number - part
							half = div number two
							two = 2
							one = 1

--- Powtórzenie znaku podan¹ iloœæ razy							
takeRepeat :: Int -> a -> [a]
takeRepeat number item = take number (repeat item)

--- Otoczenie listy danym elementem
surround :: [a] -> a -> [a]
surround list item = [item] ++ list ++ [item]

--- Pobranie œrodkowego elementu listy
middle :: [a] -> [a]
middle [] = []
middle [x] = []
middle (x:xs) = init xs

--- Funkcje pomocnicze formatu¹ce wypisany tekst
centerEach just fill list = map (center just fill) list
format just fill fields = concat (centerEach just fill fields)
formatField = format just fill

--- Usuniêcie duplikatów z listy
removeDuplicates :: [Category] -> [Category]
removeDuplicates [] = []
removeDuplicates [x] = [x]	
removeDuplicates (x:xs) = if elementOf x xs
	then removeDuplicates(xs)
	else [x] ++ removeDuplicates(xs)

--- Sprawdzenie dany czy element nale¿y do listy	
elementOf x [] = False
elementOf x (y:xs) = if x == y
	then True
	else elementOf x xs

--- zmiana znaków w stringu na wielkie litery	
toUpperString :: String -> String
toUpperString [] = []
toUpperString string = map toUpper string

isNumeric [] = True
isNumeric string =  notElem False  (map isDigit string)

--- rozszerzenie liczby 'x' do postaci '0x'
extendNumber (s:d) = do
	if ((length d) == 0) then
		return ("0"++(s:d))
	else 
		return (s:d)
		
--- funkcja pomocnicza do konwersji stringa do inta
power10 :: Int -> Int
power10 a = 10 ^ a

--- konwersja stringa do inta
string2int :: String -> Int
string2int "" = 0
string2int "-" = 0
string2int ('-':xs) = string2int xs * (-1)
string2int (x:xs) = if isDigit(x) 
	then ((digitToInt x) * power10(length xs)) + (string2int xs)
	else 0