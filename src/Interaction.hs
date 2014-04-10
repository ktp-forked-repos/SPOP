module Interaction where 

import Helper
import Data.Maybe
import Data.String.Utils
import Labels
import Data.List.Split
import Data.Char


--- złączenie dwóch słów
splitFullName xs = (firstWord xs, lastWord xs)	  

--- pobranie pierwszego słowa do wystąpnia spacji w ciągu
firstWord:: String ->String	
firstWord [] =[]
firstWord (x:y:ys) = if (y ==' ') then
					 [x]
					else
					 (x : firstWord (y:ys))

--- pobranie ostatniego słowa w
lastWord:: String ->String					
lastWord [] =[]
lastWord (x:ys) = if (x ==' ') then
					 ys
					else
					 lastWord (ys)
					 
-- Stworzenie obramowanego komunikatu
messageBox "" = ""
messageBox message =
	unlines (surround centeredMessageLinesWithBars verticalBar)
	where
		messageLines = map (surroundWith ' ') (lines message)
		messageLengths = map length messageLines
		maximumMessageLength = maximum messageLengths
		verticalBar = surround (take maximumMessageLength (repeat '-')) ' '
		centeredMessageLines = map (center maximumMessageLength ' ') messageLines
		centeredMessageLinesWithBars = map (surroundWith '|') centeredMessageLines
		surroundWith item = flip (surround) item

--- Wyświetlenie komunikatu na ekranie
showMessageBox message = putStr (messageBox message)
    
--- Wyświetlenie menu na ekranie
showMenuBox menuItems = do
	putStr (menuBox menuItems)
	function <- showMenuOptionInputBox menuItems
	return function
  
--- Utworzenie menu
menuBox menuItems =
	unlines numberedMenuItemTexts
	where
		menuItemTexts = map fst menuItems
		numberedMenuItemTexts = map (\(x, y) -> (show x) ++ ". " ++ y) (enumerate menuItemTexts 0)
					
--- Wypisanie komunikatu o możliwości wyboru opcji z menu	
showMenuOptionInputBox menuItems = do
	input <- showInputBox "Wybierz opcję podając numer"
	let optionNumber = readMaybe input
	if (isNothing optionNumber) || (notElem (fromJust optionNumber) [0..(length menuItems)]) then
		do
			putStrLn "Taka opcja nie istnieje."
			showMenuOptionInputBox menuItems
	else
		return (snd (menuItems !! ((fromJust optionNumber) - 0)))
                
--- Wypisanie polecenia zachęty
inputBox message = message ++ " : "

--- Wpisanie pojedyńczego niepustego słowa
showInputBox message = do
	putStrLn (inputBox message)
	input <- getLine
	let strippedInput = strip input
	if null strippedInput then
		showInputBox message
	else
		return strippedInput
		
--- Wpisanie pojedyńczego słowa, które może być pust
showInputBoxEmpty message = do
	putStrLn (inputBox message)
	input <- getLine
	let strippedInput = strip input
	return strippedInput
                 

--- Wpisanie dwóch słów oddzielonych spacją
show2InputBox message = do
	putStrLn (inputBox message)
	input <- getLine
	let strippedInput = strip input
	if null strippedInput then
		show2InputBox message
	else
		return ( splitFullName strippedInput)

--- Wpisanie listy elementów oddzielonych przecinkiem		
showListInputBox message = do
	putStrLn (inputBox message)
	input <- getLine
	let strippedInput = strip input
	if null strippedInput then
		showListInputBox message
	else
		return ( splitOn "," strippedInput)                 
	
		
--- Wypisanie na ekran polecenia dla podania ścieżki pliku
showFileInputBox = showInputBox "Podaj nazwę pliku (wględem folderu programu)"

--- Wczytanie odpowiedzi na zapytanie o ponowienie operacji 
showConfirmationInputBox message = do
	input <- showInputBox (message ++ " [T/N]")
	return ((input == "T") || (input == "t"))

--- Wypisanie zapytania o ponowienie operacji 
showRepeatInputBox = showConfirmationInputBox "Czy chcesz spróbować ponownie?"
									
--- Ponowne wypisanie zachęty
interactRepeatInputBox message callback = do
	showMessageBox message
	repeat <- showRepeatInputBox
	if repeat then
		callback
	else
		return Nothing
									
--- Wyświetlenie polecenie zachęty i wczytanie wielu obiektów
getListOfObjects readFunction message = do
	rawObjects <- showInputBox message
	let stringObjects = words rawObjects
	let maybeObjects = map readFunction stringObjects
	if any isNothing maybeObjects then
		interactRepeatInputBox invalidFormatErrorString (getListOfObjects readFunction message)
	else do
		let objects = map fromJust maybeObjects
		return (Just objects)
		
--- Wyświetlenie polecenia zachęty i wczytanie pojedynczego obiektu
getSingletonObject readFunction message = do
	stringObject <- showInputBox message
	let maybeObject = readFunction stringObject
	if isNothing maybeObject then
		interactRepeatInputBox invalidFormatErrorString (getSingletonObject readFunction message)
	else
		return maybeObject
		
		
--- Pobranie nazwy obiektu z funkcją weryfikującą
getFullObjectName fullNames checkFunction message = do
	(firstName,lastName) <- show2InputBox message

	if (checkFunction (firstName,lastName) fullNames) then
		return (Just (firstName, lastName))
	else
		return Nothing

--- Pobranie listy grup
getListAsString message = do
	categories <- showListInputBox message
	if (1==1) then
		return (Just (categories))
	else
		return Nothing		
		
--- Pobranie nazwy obiektu z funkcją weryfikującą
getObjectName objectNames checkFunction message = do
	objectName <- showInputBox message
	if checkFunction objectName objectNames then
		return (Just objectName)
	else
		return Nothing
		
--- Pobranie nazwy obiektu 
getSimpleObjectName   message = do
	objectName <- showInputBox message
	return (Just objectName)
	
--- Pobranie nazwy obiektu 
getObject message = do
	objectName <- showInputBoxEmpty message
	return (Just objectName)
		
--- Pobranie poprawnej wartości roku
getCorrectYear checkFunction message = do
	objectName <- showInputBox message
	if (checkFunction objectName)  then do
		let n = (string2int objectName)
		if ((n < 1)) then do
			putStrLn "Niepoprawny rok."
			getCorrectYear checkFunction message
		else
			return (Just objectName)
	else do
		putStrLn "Niepoprawny format."
		getCorrectYear checkFunction message
		
--- Pobranie poprawnej wartosci miesiąca
getCorrectMonth checkFunction message = do
	objectName <- showInputBox message
	if (checkFunction objectName)  then do
		let n = (string2int objectName)
		if ((n < 1) || (n > 12)) then do
			putStrLn "Niepoprawny miesiąc."
			getCorrectMonth checkFunction message
		else
			return (Just objectName)
	else do
		putStrLn "Niepoprawny format."
		getCorrectMonth checkFunction message
		
--- Pobranie poprawnej wartosci dnia
--getCorrectDay checkFunction message = do
getCorrectDay checkFunction message = do
	objectName <- showInputBox message
	if (checkFunction objectName)  then do
		let n = (string2int objectName)
		if ((n < 0) || (n > 31)) then do
			putStrLn "Niepoprawny dzień."			
			getCorrectDay checkFunction message	
		else
			return (Just objectName)
	else do
		putStrLn "Niepoprawny format."
		getCorrectDay checkFunction message
		
--- Pobranie nowej wartości lub jesli jest ona pusta - pozostawienie starej
getNotEmpty newName oldName  = 	if (newName /= "")
	then
		return oldName
	else
		return newName

--- Pytanie True/False
ask old message question= do
	bool <- showInputBox message
	if ((bool == "N")||(bool == "n")) then
		return (Just old)
	else do
		maybeNew <- getSimpleObjectName question
		return maybeNew

--- Pytanie True/False o datę
askDate old message = do
	bool <- showInputBox message
	if ((bool == "N")||(bool == "n")) then
		return (Just old)
	else do
		maybeYear <- getSimpleObjectName "Podaj rok"
		maybeMonth <- getSimpleObjectName "Podaj miesiąc"
		maybeDay <- getSimpleObjectName "Podaj dzień"
		let maybeDate = (Just ((fromJust maybeDay) ++ "/" ++(fromJust maybeMonth)++ "/" ++(fromJust maybeYear)))
		return maybeDate

