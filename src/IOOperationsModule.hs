
module IOOperationsModule where

import System.IO.Unsafe
import StringOperationsModule
import DocumentModule

readAllFiles :: [String] -> [Document] -> [Document]
readAllFiles [] buffer = buffer
readAllFiles (x:xs) buffer = do
								let text = unsafePerformIO (readEntireFile x)
								let raw = splitLines text
								let document = linesToDocument raw
								readAllFiles xs (document:buffer)

{- readEntireFile:
Función que lee un archivo de texto y devuelve un string con todo el contenido
del mismo.
 -}
readEntireFile :: String -> IO String
readEntireFile directory = readFile directory

convertIOtoDocument :: IO Document -> Document
convertIOtoDocument original = unsafePerformIO original 




 
 

