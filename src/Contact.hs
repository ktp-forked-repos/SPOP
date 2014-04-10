module Contact where 

import Classes
import Search
import Persistance
import Helper
import Interaction
import Data.Maybe
import Labels
import Data.List

import System.Locale (defaultTimeLocale)
import System.Time (formatCalendarTime, toUTCTime, getClockTime, ClockTime)

----Wyświetlenie kontaktów
listContacts model = do 
  showMessageBox "Lista kontaktów: "
  showMessageBox l
  printSeparator 
  return model
  where 
    contacts = getContactsFromModel model
    l = unlines $ map (\x -> (getFirstName x) ++ " " ++(getLastName x) ++ " ur: " ++(getBirthDate x) ++ " email: " ++(getEmail x) ++ " firma: " ++(getCompany x) ++ " nr: " ++(getTelephone x) ++ " grupy: " ++ (show(getCategories x))) contacts

----Wyświetlenie listy osób, które obchodzą dziś urodziny
listBirthday model = do 
  showMessageBox "Dziś urodziny obchodzą: "
  let contacts = getContactsFromModel model
  checkBirthDates contacts
  return model
	

getToday :: ClockTime -> String
getToday = formatCalendarTime defaultTimeLocale "%d/%m" . toUTCTime

checkBirthDates [] = do putStr ""
checkBirthDates (c:cs) = do
	checkBirthDate c
	checkBirthDates cs


checkBirthDate x = do 
		now <- getClockTime
		if ((getShortDate x)==(getToday now)) then
			putStr ((getFirstName x) ++ " " ++(getLastName x)++"\n")
		else
			putStr ""
		

	
----- Funkcje umoæliwiające wprowadzenie nowej nazwy dla poszczególnych operacji dla grup-----
----------------------------------------------------------------------------------------------
--- Tworzenie nowej grupy
getNewCategoryName name = getObjectName name notElem "Podaj nazwę grupy"	
--- Łączenie grup
getNewJoinedCategoryName name = getObjectName name notElem "Podaj nazwę scalonej grupy"	
--- Edycja grupy
getNewEditedCategoryName name = getObjectName name notElem "Podaj nowa nazwę edytowanej grupy"	

	

----- Funkcje umożliwiające wprowadzenie nowej nazwy dla poszczególnych operacji dla kontaktów-----
---------------------------------------------------------------------------------------------------
--- Imię dla kontaktu
getNewFirstName  = getSimpleObjectName  "Podaj imię"
--- Nazwisko dla kontaktu
getNewLastName  = getSimpleObjectName  "Podaj nazwisko"
--- Lista grup dla kontaktu
getGroupsNames  = getListAsString  "Podaj grupy oddzielone przecinkiem"
--- Rok urodzenia
getBirthDateYear  = getCorrectYear isNumeric  "Podaj rok urodzenia"
--- Miesiąc urodzenia
getBirthDateMonth  = getCorrectMonth isNumeric "Podaj miesiąc urodzenia [1-12]"
--- Dzień urodzenia
getBirthDateDay  = getCorrectDay isNumeric "Podaj dzień w miesiącu urodzenia[1-31]"


------ Funkcje umożliwiające wprowadzenie nazw dla istniejących elementów-----
------------------------------------------------------------------------------
--- Wczytanie istniejącego kontaktu poprzez podanie imienia i nazwiska oddzielonego spacją
getExisitingFullName fullNames msg = getFullObjectName fullNames elem msg

---Dodanie osoby
addContact (Model contacts categories )  = do 
  maybeFirstName <- getNewFirstName 
  maybeLastName <- getNewLastName 
  maybeGroupsNames <- getGroupsNames 
  maybeBirtDateYear <- getBirthDateYear
  maybeBirtDateMonth <- getBirthDateMonth
  maybeBirtDateDay <- getBirthDateDay
  maybeemail <- getObject  "Podaj email lub zostaw puste"
  maybecompany <- getObject  "Podaj firmę lub zostaw puste"
  maybenumber <- getSimpleObjectName  "Podaj numer telefonu"

  let newContacts = doAddContact ( fromJust maybeFirstName)  ( fromJust maybeLastName)  ( fromJust maybeBirtDateYear)  ( fromJust maybeBirtDateMonth)  ( fromJust maybeBirtDateDay)  ( fromJust maybeemail) ( fromJust maybecompany) ( fromJust maybenumber) ( fromJust maybeGroupsNames)  contacts 
  let newCategories = doAddCategoryUniqueList  ( fromJust maybeGroupsNames) categories 
  showMessageBox successfulOperationString
  return (Model newContacts newCategories )

--- Dodanie do listy kontaktów
doAddContact firstName lastName year month day email company number groups contacts = [(Contact firstName lastName date email company number groups)] ++ contacts
	where
	date = (head(extendNumber(day)) ++ "/" ++ head(extendNumber(month))++ "/" ++ year) 

--- dodanie grup do listy grup , tylko dla tych, które na niej nie istnieją
doAddCategoryUniqueList names categories =  removeDuplicates (names ++ categories)

--- Usuwanie kontaktu
removeContact (Model contacts ca ) = do
	listContacts (Model contacts ca )
	maybeFullName <- getExisitingFullName ( getFullNames contacts )"Podaj imię i nazwisko osoby do usunięcia" 
	if isNothing maybeFullName then do 
		showMessageBox personNonExistErrorString
		return (Model contacts ca)
	else do
		let newContacts = doRemoveContact (toName (fromJust maybeFullName))  contacts  
		showMessageBox successfulOperationString
		return (Model newContacts ca )
    
---Usuniecie kontaktu z listy kontaktów
doRemoveContact:: (FirstName,LastName) -> [Contact] -> [Contact]
doRemoveContact _ [] = []
doRemoveContact s (c:cs) | (getFullName c) == s    = cs    
                     | otherwise              = [c] ++ doRemoveContact s cs
 
 ---Modyfikacja kontaktu
editContact (Model contacts x )  = do 
  --listContact
  maybeFullName <- getExisitingFullName ( getFullNames contacts )"Podaj imię i nazwisko(oddzielone spacją) osoby do modyfikacji"    
  
  if ((isNothing maybeFullName)) then do 
    showMessageBox personNonExistErrorString
    return (Model contacts x )
  else do
	let (first,last) = fromJust maybeFullName
	let c = doSearchContact (first,last) contacts
	newFirstName <- ask (first)( "Czy chcesz zmienić imię(T/N)?") "Podaj nowe umię"
	newLastName <- ask (last)( "Czy chcesz zmienić nazwisko(T/N)?") "Podaj nowe nazwisko"
	newBirthDate <- askDate (getBirthDate c )( "Czy chcesz zmienić datę urodzenia(T/N)?")
	newEmail <- ask (getEmail c)( "Czy chcesz zmienić email(T/N)?") "Podaj nowy email"
	newCompany <- ask (getCompany c)( "Czy chcesz zmienić firmę(T/N)?") "Podaj nową firmę"
	newNumber <- ask (getTelephone c)( "Czy chcesz zmienić numer telefonu(T/N)?") "Podaj nowy numer"
		
	let newContacts = doEditContactInList (toName (fromJust maybeFullName)) (fromJust newFirstName) (fromJust newLastName) (fromJust newBirthDate) (fromJust newEmail) (fromJust newCompany) (fromJust newNumber)  contacts 
	showMessageBox successfulOperationString
	return (Model newContacts x) 
	
---Edycja porządanego kontaktu
doEditContactInList :: (FirstName,LastName) -> FirstName -> LastName -> BirthDate -> Email -> Company -> Number -> [Contact] -> [Contact]
doEditContactInList _ _ _ _ _ _ _[] = []
doEditContactInList s firstName lastName birthDate email company number (c:cs)  | ((getFullName c) == s) = ((doEditContact c firstName lastName birthDate email company number): cs )   
													| otherwise              = [c] ++ (doEditContactInList s firstName lastName birthDate email company number cs)	

doEditContact:: Contact -> FirstName -> LastName -> BirthDate -> Email -> Company -> Number -> Contact
doEditContact (Contact oldFirstName oldLastName oldDate oldemail oldcompany oldnumber list) firstName lastName date email company number = (Contact firstName lastName date email company number list)
