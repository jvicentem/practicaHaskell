
module DocumentModule (Document(..), existsAcronym, existsAcronymAndSource, noAcronyms, linesToDocument) where

import AcronymModule (Acronym(..), removeCharsForOperations, getListOfAcronyms)
import StringOperationsModule (splitLines, getContent, getSource, getIdDocument, 
								getYear, getTitle, getContent, getAbstract, 
								getSections)

data Document = Doc { source :: String, -- Nombre de la revista
					  id_document :: Int, -- Número identificador
					  year :: Int, -- Año de publicación
					  title :: String, -- Título
					  sections :: [String], -- Lista de secciones
					  abstract :: String, -- Abstract del artículo
					  acronyms_list :: [Acronym], -- Lista de acrónimos
					  content :: [String]
					} 
					
instance Show Document where
	show (Doc source id_document year title sections abstract acronyms_list content) =
		show "---------------" ++ "\n" ++
			 "Title: " ++ title ++ " (" ++ (show year) ++ ")" ++ "\n" ++
			 "Abstract: " ++ abstract ++ "\n" ++ 
			 "Section number: " ++ show (length sections) ++ "\n" ++ 
			 "Sections: " ++ "\n" ++ unlines sections ++ "\n" ++
			 "---------------" ++ "\n"

instance Eq Document where
	(Doc source1 id_document1 year1 title1 sections1 abstract1 acronyms_list1 content1) == (Doc source2 id_document2 year2 title2 sections2 abstract2 acronyms_list2 content2) = id_document1 == id_document2
instance Ord Document where
	(Doc source1 id_document1 year1 title1 sections1 abstract1 acronyms_list1 content1) `compare` (Doc source2 id_document2 year2 title2 sections2 abstract2 acronyms_list2 content2) = title1 `compare` title2
{- existsAcronym:
Función que devuelve True si existe un acrónimo en ese artículo. False en caso 
contrario.
-}
existsAcronym :: String->Document -> Bool
existsAcronym "" _ = False
existsAcronym acr doc = (length [x|x<-(acronyms_list doc), (minAcronym x) == (removeCharsForOperations acr)]) >= 1	

{- existsAcronymAndSource:
Función que devuelve True si el artículo tiene el acrónimo y la revista que se 
le pasan como argumentos. False en caso contrario.
-}
existsAcronymAndSource :: String->String->Document -> Bool
existsAcronymAndSource "" _ _ = False
existsAcronymAndSource _ "" _ = False
existsAcronymAndSource acr sourceName doc = (existsAcronym acr doc) && (source doc == sourceName)	

{- noAcronyms:
Función que devuelve True si el artículo no tiene acrónimos. False en caso contrario
-}
noAcronyms :: Document -> Bool
noAcronyms doc = length (acronyms_list doc) == 0

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
un artículo (si es que previamente la lista con Strings no ha sido tratada por
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
					  acronyms_list = [],
					  content = []
					 }								   	
linesToDocument (x:xs) = Doc {
						  source = getSource (x:xs),
					  	  id_document = getIdDocument (x:xs), 
					  	  year = getYear (x:xs),
					  	  title = getTitle (x:xs),
					  	  sections = getSections (x:xs), 
					  	  abstract = getAbstract (x:xs),
					  	  acronyms_list = getAcronyms (x:xs), -- Dentro del getAcronyms usaré el getContent
					  	  content = getContent (x:xs)
					  	 }