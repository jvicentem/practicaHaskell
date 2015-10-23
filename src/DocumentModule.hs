
module DocumentModule where

import AcronymModule

data Document = Doc { source :: String, -- Nombre de la revista
					  id_document :: Int, -- Número identificador
					  year :: Int, -- Año de publicación
					  title :: String, -- Título
					  sections :: [String], -- Lista de secciones
					  abstract :: String, -- 3 primeras líneas del abstract
					  acronyms_list :: [Acronym] -- Lista de acrónimos
					} deriving Show