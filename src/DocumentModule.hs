
module DocumentModule (Document(..)) where

import AcronymModule (Acronym(..))

data Document = Doc { source :: String, -- Nombre de la revista
					  id_document :: Int, -- Número identificador
					  year :: Int, -- Año de publicación
					  title :: String, -- Título
					  sections :: [String], -- Lista de secciones
					  abstract :: String, -- Abstract del artículo
					  acronyms_list :: [Acronym] -- Lista de acrónimos
					} 
					
instance Show Document where
	show (Doc source id_document year title sections abstract acronyms_list) =
		show "Nombre de revista: " ++ source ++ "\n" ++
			 "Id de artículo: " ++ show id_document ++ "\n" ++
			 "Año de publicación: " ++ show year ++ "\n" ++
			 "Título: " ++ title ++ "\n" ++
			 "Sections: " ++ show sections ++ "\n" ++
			 "Abstract: " ++ abstract ++ "\n" ++
			 "Acrónimos: " ++ show acronyms_list ++ "\n" ++
			 "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" ++ "\n"
			