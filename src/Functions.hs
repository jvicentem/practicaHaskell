
module Functions where

import Data.List
import DocumentModule (Document(..), existsAcronym, existsAcronymAndSource, noAcronyms)
import AcronymModule (Acronym(..), timesAcronyms)
import IOOperationsModule (readFiles)
import ClusterModule (Cluster(..), groupArticles, createEmptyCluster)


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
		putStrLn "4. Dado el nombre de una revista y un acrónimo, mostrar los títulos de los artículos publicados en dicha revista que contengan el acrónimo."
		putStrLn "5. Dado un año de publicación, mostrar para cada artículo publicado en ese año el listado de acrónimos que contiene acompañados de sus formas expandidas."
		putStrLn "6. Dado un identificador de artículo, mostrar un listado de los acrónimos que contiene, acompañado del número de veces que aparece cada acrónimo en el artículo."
		putStrLn "7. Mostrar los títulos e identificador de todos aquellos artículos que no contengan ningún acrónimo."
		putStrLn "8. Dado el nombre de una revista, mostrar toda la información de los artículos publicados en dicha revista."
		putStrLn "9. Agrupar artículos por acrónimos."
		
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
				5 -> do
						putStrLn "Introduce un año: "
						year <- readLn
						putStrLn (show (meaningsAcronymsFromYear articles_list year))		
						main
				6 -> do
						putStrLn "Introduce un ID: "
						idArticle <- readLn
						putStrLn (show (acronymsFromId articles_list idArticle))		
						main	
				7 -> do
						putStrLn (show (articlesWithoutAcronyms articles_list))		
						main	
				8 -> do
						putStrLn "Introduce el nombre de una revista: "
						source <- readLn
						putStrLn (show (articlesFromSource articles_list source))		
						main	
				9 -> do
						putStrLn (show (clusterArticles articles_list))		
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
									 													 					
-- 5
meaningsAcronymsFromYear :: [Document]->Int -> [String]
meaningsAcronymsFromYear [] _ = []
meaningsAcronymsFromYear (x:xs) yearArticle  =  if year x == yearArticle then
													insert (string) (meaningsAcronymsFromYear (xs) yearArticle)
												else
													meaningsAcronymsFromYear (xs) yearArticle	
													
												where
													string = (title x)++"\n"++(show (acronyms_list x))++"\n"		
													
-- 6
acronymsFromId :: [Document]->Int -> [(String,Int)]
acronymsFromId [] _ = []
acronymsFromId (x:xs) idArticle = 	if id_document x == idArticle then
										timesAcronyms (acronyms_list x) (content x)
									else
										acronymsFromId (xs) idArticle
										
										
-- 7
articlesWithoutAcronyms :: [Document] -> [(Int,String)]
articlesWithoutAcronyms [] = []						
articlesWithoutAcronyms (x:xs) = articlesWithoutAcronymsImpl (x:xs) []

articlesWithoutAcronymsImpl :: [Document]->[(Int,String)] -> [(Int,String)]
articlesWithoutAcronymsImpl [] [] = []
articlesWithoutAcronymsImpl [] buffer = buffer
articlesWithoutAcronymsImpl (x:xs) buffer = if noAcronyms x then 
												articlesWithoutAcronymsImpl (xs) ( (id_document x, title x):buffer)		
											else
												articlesWithoutAcronymsImpl (xs) buffer		
												
-- 8
articlesFromSource :: [Document]->String -> [Document]
articlesFromSource [] _ = []
articlesFromSource (x:xs) sourceArticles = articlesFromSourceImpl (x:xs) sourceArticles []

articlesFromSourceImpl :: [Document]->String->[Document] -> [Document]
articlesFromSourceImpl [] _ buffer = buffer
articlesFromSourceImpl (x:xs) sourceArticles buffer = if source x == sourceArticles then
														articlesFromSourceImpl (xs) sourceArticles (x:buffer)
												  	  else
												  		articlesFromSourceImpl (xs) sourceArticles buffer												
									 
-- 9
clusterArticles :: [Document] -> (Cluster,[Cluster])
clusterArticles [] = (createEmptyCluster,[])
clusterArticles (x:xs) = groupArticles (x:xs)																										 												 	