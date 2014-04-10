module Main where

import System.Exit
import Classes
import Data.Char
import Persistance
import Helper
import Interaction
import Contact
import Category
import Search

--Pętla główna
start = do
      showMainMenuInLoop createModel
      
--Główna pętla, w której wyświetlane jest menu
showMainMenuInLoop model = do
	printNewLine
	function <- showMainMenu
	model <- function model
	showMainMenuInLoop model

      
showMainMenu =
	showMenuBox [
	("Zakończ", exit),
	("Zarządzanie kontaktami", showMenuContactsInLoop),
	("Zarządzanie grupami", showMenuCategoriesInLoop),
        ("Zapis danych do pliku",  saveModel),
        ("Odczyt danych z pliku", loadModel),
        ("Wyczyszczenie danych", clearModel)]
        
        
--- Menu kontaktów
showMenuContactsInLoop model = do
  printNewLine
  model <- menuSubject model
  showMenuContactsInLoop model
 
--- Menu grup
showMenuCategoriesInLoop model = do
  printNewLine
  model <- menuCategory model
  showMenuCategoriesInLoop model

        
--- Podmenu kontakty
menuSubject model = do
  function <- showMenuBox[
	("Powrót do głównego menu", showMainMenuInLoop),
	("Wyświetl wszystkie kontakty", listContacts),
	("Dodaj kontakt", addContact),
	("Edytuj kontakt", editContact),
	("Urodziny", listBirthday),
	("Wyszukiwanie wg nazwy", searchByName),
	("Czyj to numer", searchByNumber),
	("Usuń kontakt", removeContact)]
  model <- function model
  return model
  
--- Podmenu grupy  
menuCategory model = do
  function <- showMenuBox[
	("Powrót do głównego menu", showMainMenuInLoop),
	("Wyświetl wszystkie grupy", listCategories),
    ("Dodaj grupę", addCategory),
	("Zmień nazwe grupy",changeCategoryName),
	("Pokaż kontakty w grupie", searchByGroup),
    ("Scal grupę", joinCategories),
    ("Usuń grupę", removeCategory)]
  model <- function model
  return model
  
returnToMainMenu model = do 
   return model
   
--- Wyjście kończące działanie programu
exit _ = exitWith ExitSuccess
