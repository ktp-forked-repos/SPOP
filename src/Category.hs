module Category where 

import Classes
import Persistance
import Helper
import Interaction
import Data.Maybe
import Labels
import Data.List


--- Wyświetlenie wszystkich grup
listCategories (Model co ca) = do 
  showMessageBox "Grupy kontaktów: "
  showMessageBox l
  printSeparator 
  return (Model co categories)
  where 
    categories = ca
    l = unlines $ map (\x -> x  ) categories
    --sep = " || "

	
----- Funkcje umoæliwiające wprowadzenie nowej nazwy dla poszczególnych operacji dla grup-----
----------------------------------------------------------------------------------------------
--- Tworzenie nowej grupy
getNewCategoryName name = getObjectName name notElem "Podaj nazwę grupy"	
--- Łączenie grup
getNewJoinedCategoryName name = getObjectName name notElem "Podaj nazwę scalonej grupy"	
--- Edycja grupy
getNewEditedCategoryName name = getObjectName name notElem "Podaj nowa nazwę edytowanej grupy"	
	

------ Funkcje umożliwiające wprowadzenie nazw dla istniejących elementów-----
------------------------------------------------------------------------------
--- Wczytanie istniejącej grupy
getExisitingCategoryName subject msg = getObjectName subject elem msg


 
--- dodanie nowej grupy
addCategory (Model x categories )  = do 
  maybeCategoryName <- getNewCategoryName (  categories )
  if isNothing maybeCategoryName then do 
    showMessageBox categoryExistErrorString
    return (Model x categories )
  else do
    let newCategories = doAddCategory ( fromJust maybeCategoryName)  categories --( fromJust maybeDurationTime) category
    showMessageBox successfulOperationString
    return (Model x newCategories  ) 
  
--- dodanie grupy do listy grup  
doAddCategory name categories = [name] ++ categories


--- Usuwanie grupy
removeCategory (Model contacts categories ) = do
  maybeCategoryName <- getExisitingCategoryName categories "Podaj nazwę grupy do usunięcia" 
  if isNothing maybeCategoryName then do 
    showMessageBox categoryNonExistingErrorString
    return (Model contacts categories)
  else do
    let newCategories = removeCategoryFromList  (fromJust maybeCategoryName)  categories 
	newContacts = doRemoveCategoryFromContacts  (fromJust maybeCategoryName) contacts 
    showMessageBox successfulOperationString
    return (Model newContacts newCategories )
    

---Usunięcie grupy z listy grup 
removeCategoryFromList:: Category -> [Category] -> [Category]
removeCategoryFromList _ [] = []
removeCategoryFromList s (c:cs) | ( c == s )   = cs    
                     | otherwise              = [c] ++ removeCategoryFromList s cs

---Usunięcie grupy z listy kontaktów
doRemoveCategoryFromContacts:: Category -> [Contact] -> [Contact]
doRemoveCategoryFromContacts _ [] = []
doRemoveCategoryFromContacts category (c:cs) = (doRemoveCategoryFromContact category c):(doRemoveCategoryFromContacts category cs)

---Usunięcie grupy z pojedyńczego kontaktu
doRemoveCategoryFromContact:: Category -> Contact -> Contact
doRemoveCategoryFromContact category (Contact firstName lastName birthDate email company number list) = Contact  firstName lastName birthDate email company number  (removeElementFromList category  list)

---Usunięcie grupy z listy grup
removeElementFromList:: Category -> [Category] -> [Category]
removeElementFromList cat (x:xs) =  if  (x == cat ) then
	xs
	else 
	(x:(removeElementFromList cat xs))
	
---scalenie grup
joinCategories (Model contacts categories )  = do 
  maybeCategoryName1 <- getExisitingCategoryName categories "Podaj nazwę pierwszej grupy do scalenia" 
  maybeCategoryName2 <- getExisitingCategoryName categories "Podaj nazwę drugiej grupy do scalenia"    
  maybeCategoryNew <- getNewJoinedCategoryName categories                   
  if ((isNothing maybeCategoryName1)||(isNothing maybeCategoryName2)||(isNothing maybeCategoryNew)) then do 
    showMessageBox categoryExistErrorString
    return (Model contacts categories )
  else do
    let newCategories =  (removeDuplicates (doJoinCategories (fromJust maybeCategoryName1) (fromJust maybeCategoryName2) ( fromJust maybeCategoryNew)  categories) )
	newContacts = doJoinCategoryInContacts  (fromJust maybeCategoryName1) (fromJust maybeCategoryName2) ( fromJust maybeCategoryNew) contacts 
    showMessageBox successfulOperationString
    return (Model newContacts newCategories  ) 
	
---Połączenie grup	
doJoinCategories name1 name2 newName categories = changeNames name1 newName (changeNames name2 newName categories)

---Zmiana nazwy elementu listy
changeNames _ _ [] = []
changeNames oldName newName (category:rest) | (oldName == category) = (newName:rest)
										  | (oldName /= category) = (category:(changeNames oldName newName rest))

---Scalenie w kontaktach										  
doJoinCategoryInContacts  _ _ _ [] = []
doJoinCategoryInContacts  name1 name2 newName (c:contacts) = ((doJoinCategoriesInContact  name1 name2 newName c):(doJoinCategoryInContacts  name1 name2 newName contacts))

---Scalenie grup w kontaktach
doJoinCategoriesInContact  name1 name2 newName (Contact firstName lastName birthDate email company number []) = (Contact firstName lastName birthDate  email company number [])
doJoinCategoriesInContact  name1 name2 newName (Contact firstName lastName birthDate email company number categories) = Contact firstName lastName birthDate email company number(removeDuplicates (doJoinCategories name1 name2 newName  categories))

---Zmiana nazwy grupy
changeCategoryName (Model contacts categories )  = do 
  maybeCategoryName <- getExisitingCategoryName categories "Podaj nazwę grupy do edycji"    
  maybeCategoryNew <- getNewEditedCategoryName categories                   
  if ((isNothing maybeCategoryName)||(isNothing maybeCategoryNew)) then do 
    showMessageBox categoryExistErrorString
    return (Model contacts categories )
  else do
    let newCategories =  (removeDuplicates (changeNames (fromJust maybeCategoryName) ( fromJust maybeCategoryNew)  categories) )
	newContacts = doChangeCategoryInContacts  (fromJust maybeCategoryName) ( fromJust maybeCategoryNew) contacts 
    showMessageBox successfulOperationString
    return (Model newContacts newCategories  ) 
	
--Zmiana nazwy w liście kontaktów	
doChangeCategoryInContacts  _ _ [] = []
doChangeCategoryInContacts  oldName  newName (c:contacts) = ((doChangeCategoryInContact  oldName  newName c):(doChangeCategoryInContacts  oldName  newName contacts))

--Zmiana nazwy w kontakcie
doChangeCategoryInContact  oldName  newName (Contact firstName lastName birthDate email company number []) = (Contact firstName lastName birthDate email company number [])
doChangeCategoryInContact  oldName  newName (Contact firstName lastName birthDate email company number categories) = Contact firstName lastName birthDate email company number (removeDuplicates (changeNames oldName newName categories))

