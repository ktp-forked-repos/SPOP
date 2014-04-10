module Classes where

import Data.Time
import System.Locale 
import System.Time 
import Data.List
 
type LastName = String
type FirstName = String
type BirthDate = String
type Category = String
type Email = String
type Company = String
type Number = String
type FullName = (FirstName,LastName)

--- Kontakt w postaci: Imię Naziwsko Lista_grup_do_których_należy
data Contact = Contact FirstName LastName BirthDate Email Company Number [Category] deriving (Show,Read,Eq)

--getContact :: Contact -> FirstName
--getContact (Contact s _  _ _) = s

getFirstName :: Contact -> FirstName
getFirstName (Contact s _  _ _ _ _ _) = s

getLastName :: Contact -> LastName
getLastName (Contact _ s _ _ _ _ _) = s

getFullName :: Contact -> (FirstName, LastName)
getFullName  (Contact s a b _ _ _ _) = f 
	where f = (s,a)
	
getCategories :: Contact -> [Category]
getCategories (Contact _ _ _ _ _ _ s) = s

getLastNames :: [Contact] -> [LastName]
getLastNames [] = []
getLastNames (c:cs) = [(getLastName c)] ++ (getLastNames cs)

getBirthDate:: Contact -> String
getBirthDate (Contact f l birthDate _ _ _ g) = birthDate

getShortDate:: Contact -> String
getShortDate (Contact f l birthDate  _ _ _ g) = take 5 birthDate

getTelephone:: Contact -> String
getTelephone (Contact f l birthDate _ _ n g) = n

getCompany:: Contact -> String
getCompany (Contact f l birthDate _ c _ g) = c

getEmail:: Contact -> String
getEmail (Contact f l birthDate e _ _ g) = e

getFirstNames :: [Contact] -> [FirstName]
getFirstNames [] = []
getFirstNames (c:cs) = [(getFirstName c)] ++ (getFirstNames cs)

getFullNames :: [Contact] -> [(FirstName,LastName)]
getFullNames [] = []
getFullNames (c:cs) = [(getFullName c)] ++ (getFullNames cs)

getCategoryList (Model _ c ) =  c

data Model = Model [Contact] [Category] deriving (Show, Read)
getContactsFromModel (Model c _) =  c
getCategoriesFromModel (Model _ c) =  c


