
module StringOperationsModule (splitLines,linesToDocument,getContent) where

import Data.List
import DocumentModule (Document(..))
import AcronymModule (Acronym(..),getListOfAcronyms)

{- getSource:
Función que obtiene el nombre de la revista en la que está publicado el artículo.
Realmente obtiene el primer String de la lista de Strings que se le pasa como
parámetro de entrada.

Algunos nombres de revista tienen un espacio delante, así que se elimina el 
espacio.
 -}
getSource :: [String] -> String
getSource [] = ""
getSource ((x:xs):(y:ys)) = if x == ' ' then (xs) else (x:xs)

{- getIdDocument:
Función que obtiene el id del artículo.
Realmente llama a la función convertLineToInt que se encarga de seleccionar
el String adecuado dentro de la lista de Strings (gracias a que le paso como
parámetro de entrada el índice) y lo devuelve en forma de Int si es que se puede
convertir.
 -}
getIdDocument :: [String] -> Int
getIdDocument (x:(y:ys)) = convertLineToInt (x:(y:ys)) 1

{- getYear:
Función que obtiene el año en el que se ha publicado el artículo.
Realmente llama a la función convertLineToInt que se encarga de seleccionar
el String adecuado dentro de la lista de Strings (gracias a que le paso como
parámetro de entrada el índice) y lo devuelve en forma de Int si es que se puede
convertir.
 -}
getYear :: [String] -> Int
getYear (x:(y:ys)) = convertLineToInt (x:(y:ys)) 2

{- getTitle:
Función que obtiene el título del artículo.
Realmente obtiene el quinto String de la lista de Strings que se le pasa como
parámetro de entrada.
 -}
getTitle :: [String] -> String
getTitle [] = ""
getTitle (x:(y:ys)) = (x:(y:ys))!!4

{- getAbstract:
Función que obtiene el resumen del artículo.
Realmente obtiene el séptimo String de la lista de Strings que se le pasa como
parámetro de entrada.
 -}
getAbstract :: [String] -> String
getAbstract [] = ""
getAbstract (x:(y:ys)) = (x:(y:ys))!!6

{- getSections:
Función que obtiene un listado de las secciones del artículo.
Realmente utiliza dos funciones: getLinesBetweenDashes y deleteContentSections.

Se eliminan los siete primeros elementos de la lista de Strings que se pasa como
primer argumento a la función porque todas esas líneas anteriores ya han sido 
tratadas por otras funciones ya que contienen información distinta a la referente
a las secciones.
-}
getSections :: [String] -> [String]
getSections [] = []
getSections (x:(y:ys)) = deleteContentSections ( getLinesBetweenDashes (drop 7 (x:(y:ys))) )	

{- getContent:
Función que obtiene un listado de los contenidos de las secciones además del 
abstract.
Realmente utiliza dos funciones: getLinesBetweenDashes y deleteSections.

Se eliminan los siete primeros elementos de la lista de Strings que se pasa como
primer argumento a la función porque todas esas líneas anteriores ya han sido 
tratadas por otras funciones ya que contienen información distinta a la referente
a las secciones.

Esta función está para que se pueda analizar el contenido del artículo y 
encontrar acrónimos y sus formas expandidas.
-}
getContent :: [String] -> [String]
getContent [] = []
getContent (x:(y:ys)) = deleteSectionsTitles ( getLinesBetweenDashes (drop 7 (x:(y:ys))) ) ++ [getTitle (x:(y:ys))] ++ [getAbstract (x:(y:ys))] 

{- getAcronyms:
Función que obtiene todos los acrónimos de un artículo.

Realmente, devuelve una lista de tipos Acronym y cada tipo Acronym contiene el
acrónimo y su forma expandida.
-}
getAcronyms :: [String] -> [Acronym]
getAcronyms [] = []
getAcronyms (x:xs) = getListOfAcronyms (getContent (x:xs))

{-linesToDocument:
Función que almacena los Strings que están en una lista en un tipo Document.

Esa lista de Strings, al igual que en otras funciones, contendrá la información
de un artículo. Cada String de la lista será una línea de un archivo de texto de
un artículo (si es que previamente la lista con Stringsn no ha sido tratada por
otra función).
-}
linesToDocument :: [String] -> Document
linesToDocument [] = Doc {
					  source = "",
					  id_document = -1, 
					  year = -1,
					  title = "",
					  sections = [], 
					  abstract = "",
					  acronyms_list = []
					 }								   	
linesToDocument (x:xs) = Doc {
						  source = getSource (x:xs),
					  	  id_document = getIdDocument (x:xs), 
					  	  year = getYear (x:xs),
					  	  title = getTitle (x:xs),
					  	  sections = getSections (x:xs), 
					  	  abstract = getAbstract (x:xs),
					  	  acronyms_list = getAcronyms (x:xs) -- Dentro del getAcronyms usaré el getContent
					  	 }			

{- splitLines:
Función que coloca cada línea precedida de un salto de línea en una posición de
una lista de Strings.
 -}
splitLines :: String -> [String]
splitLines "" = [""]
splitLines (x:xs) = lines (x:xs)

{- stringToInt:
Función que convierte un String en Int.
 -}
stringToInt :: String -> Int
stringToInt "" = -1
stringToInt (x:xs) = read (x:xs) :: Int

{- convertLineToInt:
Función que convierte un String dentro de una lista de Strings a Int. 
Como segundo parámetro de la función se le pasa un número que será la posición
que ocupa la línea que se desea convertir dentro de la lista.
 -}
convertLineToInt :: [String]->Int -> Int
convertLineToInt [] _ = -1
convertLineToInt (x:(y:ys)) i = stringToInt ((x:(y:ys)) !! i)

{- getLinesBetweenDashes:
Función que extrae las líneas de texto que se encuentran entre guiones (--).
Realmente llama a la función deleteByString y se le pasa como argumento el 
String "--".
 -}
getLinesBetweenDashes :: [String] -> [String]
getLinesBetweenDashes [] = []
getLinesBetweenDashes (x:(y:ys)) = deleteByString "--" (x:(y:ys)) []

{- deleteByString:
Función que elimina de una lista de Strings aquellos Strings de esa lista que 
son iguales a uno que se le pasa como parámetro de entrada.
-}
deleteByString :: String->[String]->[String] -> [String]
deleteByString _ [] [] = []
deleteByString string [x] (z:zs) = if x == string then 
									 reverse (drop 1 (z:zs)) 
								   else reverse (drop 1 (x:(z:zs)))
deleteByString string (x:(y:ys)) buffer = if x == string then 
											deleteByString string (y:ys) buffer
										  else
							   				deleteByString string (y:ys) (x:buffer)																 
deleteByString _ [""] _ = []

{- deleteContentSections:
Función que elimina el contenido de las secciones.
 -}					  
deleteContentSections :: [String] -> [String]
deleteContentSections [] = []
deleteContentSections list = deleteContentSectionsImpl list 0 []

deleteContentSectionsImpl :: [String]->Int->[String] -> [String]
deleteContentSectionsImpl [] _ buffer = reverse buffer
deleteContentSectionsImpl (x:xs) i buffer = if odd i then 
												deleteContentSectionsImpl xs (i+1) buffer
											else
												deleteContentSectionsImpl xs (i+1) (x:buffer)	
												
{- deleteSectionsTitle:
Función que elimina las secciones.
 -}					  
deleteSectionsTitles :: [String] -> [String]
deleteSectionsTitles [] = []
deleteSectionsTitles list = deleteSectionsTitlesImpl list 0 []

deleteSectionsTitlesImpl :: [String]->Int->[String] -> [String]
deleteSectionsTitlesImpl [] _ buffer = reverse buffer
deleteSectionsTitlesImpl (x:xs) i buffer = if odd i then 
											deleteSectionsTitlesImpl xs (i+1) (x:buffer)
									 	   else
											deleteSectionsTitlesImpl xs (i+1) buffer
											
				  	 