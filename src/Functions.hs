
module Functions where

import Data.List
import DocumentModule (Document(..))
import AcronymModule (Acronym(..))
import IOOperationsModule (readFiles)


articles_list = readFiles "../papersUTF8"

main :: IO ()
main = do
		putStrLn ""
		putStrLn "Práctica Haskell de Paradigmas de Programación Curso 2015/2016 GIS"
		putStrLn "------------------------------------------------------------------"
		putStrLn "Selecciona una opción:"
		putStrLn "1. Mostrar los títulos de los artículos ordenados alfabéticamente y publicados en un año dado."
		putStrLn "2. Mostrar el listado de revistas en las que se han publicado los artículos de toda la colección."
		
		option <- readLn
		
		if option > 9 || option < 0 then -- OJO CAMBIAR EL option < 0 A option < 1
			do
				putStrLn "Opción elegida incorrecta."
				putStrLn ""
				main
		else
			case option of
				0 -> do
						putStrLn (show articles_list)
				1 -> do
						putStrLn "Introduce un año: "
						year <- readLn
						putStrLn (show (articlesByYear articles_list year))		
						main
				2 -> do
						putStrLn (show (sourcesOfArticles articles_list))
						main			
				
articlesByYear :: [Document] -> Int -> [String]
articlesByYear [] _ = []
articlesByYear (x:xs) yearArticle = if year x == yearArticle then
										insert (title x) (articlesByYear (xs) yearArticle)
									else
										articlesByYear (xs) yearArticle

sourcesOfArticles :: [Document] -> [String]
sourcesOfArticles [] = []
sourcesOfArticles (x:xs) = insert (source x) (sourcesOfArticles (xs))
