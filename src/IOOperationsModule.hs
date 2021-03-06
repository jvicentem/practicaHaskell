
module IOOperationsModule (readFiles) where

import System.IO.Unsafe
import System.IO.Error
import System.Directory
import System.FilePath
import StringOperationsModule (splitLines, getContent)
import DocumentModule (Document(..), linesToDocument)

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
Función que lee todos los archivos de texto incluidos en el directorio que se 
pasa como primer parámetro de entrada.
Como salida, devolverá una lista con tipos Document y cada uno tendrá la 
la información de los archivos que se han leído.
-}

readFiles :: String -> [Document]
readFiles "" = []
readFiles (x:xs) = readFilesImpl (getPaths (x:xs)) []

readFilesImpl :: [FilePath]->[Document] -> [Document]
readFilesImpl [] buffer = reverse buffer
readFilesImpl (x:xs) buffer = do
								let text = unsafePerformIO (readEntireFile x)
								let raw = splitLines text
								let document = linesToDocument raw
								readFilesImpl xs (document:buffer)

{- readEntireFile:
Función que lee un archivo de texto y devuelve un string con todo el contenido
del mismo.
 -}
readEntireFile :: String -> IO String
readEntireFile directory = catchIOError (readFile directory) handler

{- getPaths:
A partir de un path de una carpeta, obtiene todas las rutas de archivos
contenidos en ella.
-}
getPaths :: String -> [FilePath]
getPaths folderPath = [makeValid folderPath ++ "/" ++ x | x <- unsafePerformIO (getDirectoryContents folderPath),x /= ".." && x /= "."]
 





 
 

