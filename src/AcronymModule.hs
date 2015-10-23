
module AcronymModule where

data Acronym = Acr {
					min_list :: [String], -- Contiene los acrónimos
					max_list :: [String] -- Contiene los acrónimos expandidos
					} deriving Show
{-- Para cada acrónimo i-ésimo contenido en min_list, estará su forma expandida
en la posición i-ésima de max_list --}