
module DocumentModule (Document(..), existsAcronym, existsAcronymAndSource, noAcronyms) where

import AcronymModule (Acronym(..), removeCharsForOperations)

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
		show "Nombre de revista: " ++ source ++ "\n" ++
			 "Id de artículo: " ++ show id_document ++ "\n" ++
			 "Año de publicación: " ++ show year ++ "\n" ++
			 "Título: " ++ title ++ "\n" ++
			 "Secciones: " ++ show sections ++ "\n" ++
			 "Abstract: " ++ abstract ++ "\n" ++
			 "Acrónimos: " ++ show acronyms_list ++ "\n" ++
			 "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" ++ "\n"

existsAcronym :: String->Document -> Bool
existsAcronym "" _ = False
existsAcronym acr doc = (length [x|x<-(acronyms_list doc), (minAcronym x) == (removeCharsForOperations acr)]) >= 1	

existsAcronymAndSource :: String->String->Document -> Bool
existsAcronymAndSource "" _ _ = False
existsAcronymAndSource _ "" _ = False
existsAcronymAndSource acr sourceName doc = (existsAcronym acr doc) && (source doc == sourceName)	

noAcronyms :: Document -> Bool
noAcronyms doc = length (acronyms_list doc) == 0