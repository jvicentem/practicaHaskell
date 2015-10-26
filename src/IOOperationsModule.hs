
module IOOperationsModule where

import System.IO.Unsafe
import System.IO.Error
import StringOperationsModule
import DocumentModule

handler :: IOError -> IO String
handler e  = if isDoesNotExistError e then
				do
					putStrLn "El fichero no existe."
					return ""
			  else
				do
					putStrLn "Error desconocido"
					return ""
					
{- readFiles:
Función que lee todos los archivos de texto cuya dirección esté incluida en el
primer parámetro de entrada de esta función, que es una lista de Strings y cada
String será una dirección del archivo a leer.

Como salida, devolverá una lista con tipos Document y cada uno tendrá la 
la información de los archivos que se han leído.
-}
readFiles :: [String] -> [Document]
readFiles [] = []
readFiles (x:xs) = do
					let text = unsafePerformIO (readEntireFile x)
					let raw = splitLines text
					let document = linesToDocument raw
					document:readFiles xs 

{- readEntireFile:
Función que lee un archivo de texto y devuelve un string con todo el contenido
del mismo.
 -}
readEntireFile :: String -> IO String
readEntireFile directory = catchIOError (readFile directory) handler





 
 

