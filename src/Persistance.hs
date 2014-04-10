module Persistance where
 
import Classes
import Data.Maybe
import Helper
import Interaction
import Labels

--- Utworzenie pustego modelu danych
createModel = Model [] []

--- Zapisanie danych do pliku
saveModel model = do 
  filePath <- showFileInputBox
  saveToFile model filePath
  showMessageBox successfulOperationString
  return model

--- Wczytanie danych z pliku
loadModel model =
  tryCatch (doLoadModel model) (showError model)
  where 
    doLoadModel model = do
      filePath <- showFileInputBox
      maybeModel <- loadFromFile filePath readMaybeModel
      if isNothing maybeModel then do
        showMessageBox invalidFormatErrorString
        return model
      else do 
          showMessageBox successfulOperationString
          let newModel = fromJust maybeModel
          return newModel
			where
				readMaybeModel :: String -> Maybe Model
				readMaybeModel = readMaybe
        
    showError model = do
      showMessageBox cannotOpenFileErrorString
      return model
		

--- Wyczyszczenie danych
clearModel model = do
	showMessageBox successfulOperationString
	return createModel