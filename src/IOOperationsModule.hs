
module IOOperationsModule (readFiles) where

import System.IO.Unsafe
import System.IO.Error
import System.Directory
import System.FilePath
import StringOperationsModule (splitLines,linesToDocument,getContent)
import DocumentModule (Document(..))

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
readFiles (x:xs) = readFilesImpl (getPaths (x:xs))

readFilesImpl :: [FilePath] -> [Document]
readFilesImpl [] = []
readFilesImpl (x:xs) = do
						let text = unsafePerformIO (readEntireFile x)
						let raw = splitLines text
						let document = linesToDocument raw
						document:readFilesImpl xs 

{- readEntireFile:
Función que lee un archivo de texto y devuelve un string con todo el contenido
del mismo.
 -}
readEntireFile :: String -> IO String
readEntireFile directory = catchIOError (readFile directory) handler

{- getPaths
A partir de un path de una carpeta, obtiene todas las rutas de archivos
contenidos en ella.
-}
getPaths :: String -> [FilePath]
getPaths folderPath = [makeValid folderPath ++ "/" ++ x | x <- unsafePerformIO (getDirectoryContents folderPath),x /= ".." && x /= "."]
 





 
 

