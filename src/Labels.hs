module Labels where

successfulOperationString = "Operacja zakończona sukcesem"

errorStr msg = "Wystąpił błąd: " ++ msg
personNonExistErrorString = "Osoba nie istenieje"

categoryNonExistingErrorString = errorStr "Podana grupa nie istnieje"
categoryExistErrorString = errorStr "Grupa o podanej nazwie już istnieje"

invalidFormatErrorString =  errorStr "Błędny format danych"
cannotOpenFileErrorString = errorStr "Błąd odczytu pliku"
