
module Functions where

import Data.List
import DocumentModule (Document(..), existsAcronym, existsAcronymAndSource)
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
		putStrLn "3. Dado un acrónimo, buscarlo en los diferentes artículos y mostrar los títulos de aquellos que contengan el acŕonimo."
		
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
				3 -> do
						putStrLn "Introduce un acrónimo: "
						acronym <- readLn
						putStrLn (show (articlesWithAcronym articles_list acronym))		
						main
				4 -> do
						putStrLn "Introduce el nombre de una revista: "
						source <- readLn
						putStrLn "Introduce un acrónimo: "
						acronym <- readLn
						putStrLn (show (articlesWithSourceAndAcronym articles_list acronym source))		
						main
-- 1				
articlesByYear :: [Document]->Int -> [String]
articlesByYear [] _ = []
articlesByYear (x:xs) yearArticle = if year x == yearArticle then
										insert (title x) (articlesByYear (xs) yearArticle)
									else
										articlesByYear (xs) yearArticle
-- 2
sourcesOfArticles :: [Document] -> [String]
sourcesOfArticles [] = []
sourcesOfArticles (x:xs) = insert (source x) (sourcesOfArticles (xs))

-- 3
articlesWithAcronym :: [Document]->String -> [String]
articlesWithAcronym [] _ = []
articlesWithAcronym _ "" = []
articlesWithAcronym (x:xs) acronym = if existsAcronym acronym x then
										insert (title x) (articlesWithAcronym (xs) acronym)
									 else
									 	articlesWithAcronym (xs) acronym
									 	
-- 4
articlesWithSourceAndAcronym :: [Document]->String->String -> [String]
articlesWithSourceAndAcronym [] _ _ = []
articlesWithSourceAndAcronym _ "" _ = []
articlesWithSourceAndAcronym _ _ "" = []
articlesWithSourceAndAcronym (x:xs) acronym source = if existsAcronymAndSource acronym source x then
														insert (title x) (articlesWithSourceAndAcronym (xs) acronym source)
									 				 else
									 					articlesWithSourceAndAcronym (xs) acronym source									 	