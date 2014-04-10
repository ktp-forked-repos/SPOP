module Search where 

import Classes
import Persistance
import Helper
import Interaction
import Data.Maybe
import Labels
import Data.List
import Category

--- Imię dla szukanego kontaktu
getSearchFirstName  = getObject  "Podaj imię lub zostaw puste"
--- Imię dla szukanego kontaktu
getSearchLastName  = getObject  "Podaj nazwisko lub zostaw puste"
--- Imię dla szukanego kontaktu
getSearchGroupNames  = getObject  "Podaj grupę lub zostaw puste"

--- Wyszukiwanie wg imienia i nazwiska
searchByName (Model contacts categories )  = do 
  maybeFirstName <- getSearchFirstName 
  maybeLastName <- getSearchLastName 
  maybeGroupsNames <- getSearchGroupNames 
  let searchContacts = doSearchName ( fromJust maybeFirstName)  ( fromJust maybeLastName) ( fromJust maybeGroupsNames)  contacts 
  let l = unlines $ map (\x -> (getFirstName x) ++ " " ++(getLastName x) ++ " ur: " ++(getBirthDate x) ++ " email: " ++(getEmail x) ++ " firma: " ++(getCompany x) ++ " nr: " ++(getTelephone x) ++ " grupy: " ++ (show(getCategories x))) searchContacts
  showMessageBox "Lista wyszukanych kontaktów: "
  showMessageBox l
  return (Model contacts categories )		

doSearchName firstName lastName groups contacts =  filter (isResultName firstName lastName groups)  contacts

isResultName (firstName)  (lastName) (group) contact = (((toUpperString(lastName) == toUpperString(getLastName contact)) || ("" == lastName )) && (toUpperString(firstName) == toUpperString(getFirstName contact) || ("" == (firstName)))   &&   (( elem group (getCategories contact)) || ("" == (group))) )

--- Numer telefonu
getNumber  = getSimpleObjectName  "Podaj numer telefonu"

--- Wyszukiwanie wg numeru telefonu 
searchByNumber (Model contacts categories )  = do 
  maybeNumber <- getNumber 
  let searchContacts = doSearchNumber ( fromJust maybeNumber)   contacts 

  let l = unlines $ map (\x -> (getFirstName x) ++ " " ++(getLastName x) ++ " ur: " ++(getBirthDate x) ++ " email: " ++(getEmail x) ++ " firma: " ++(getCompany x) ++ " nr: " ++(getTelephone x) ++ " grupy: " ++ (show(getCategories x))) searchContacts
  showMessageBox "Lista wyszukanych kontaktów: "
  showMessageBox l
  return (Model contacts categories )		

doSearchNumber  number contacts =  filter (isResultNumber number)  contacts

isResultNumber (number) contact = ((number) == (getTelephone contact))



--- Wyszukiwanie wg grupy
searchByGroup (Model contacts categories )  = do 
  maybeGroupName <- getExisitingCategoryName categories "Podaj nazwę grupy" 
  if (isNothing maybeGroupName) then do 
	showMessageBox categoryNonExistingErrorString
	return (Model contacts categories)
  else do
	let searchContacts = doSearchByGroup  ( fromJust maybeGroupName)  contacts 
	let l = unlines $ map (\x -> (getFirstName x) ++ " " ++(getLastName x) ++ " ur: " ++ " email: " ++(getEmail x) ++ " firma: " ++(getCompany x) ++ " nr: " ++(getTelephone x) ++ (getBirthDate x) ) searchContacts
	showMessageBox "Lista kontaktów w grupie: "
	showMessageBox l
	return (Model contacts categories )		

doSearchByGroup  groupName contacts =  filter (isResultGroup groupName)  contacts

isResultGroup (groupName) contact = elem groupName (getCategories contact)



doSearchContact  (first,last) contacts =  head(filter (isResultContact (first,last))  contacts)

isResultContact (first,last) contact = ((first) == (getFirstName contact) && (last) == (getLastName contact))