
module MainModule where

import FunctionsModule (articlesByYear, sourcesOfArticles, articlesWithAcronym, 
				  articlesWithSourceAndAcronym, meaningsAcronymsFromYear, 
				  acronymsFromId, articlesWithoutAcronyms, articlesFromSource, 
				  clusterArticles, articlesSortedByYear)
import IOOperationsModule (readFiles)

articles_list = readFiles "../papersUTF8"

main :: IO ()
main = do
		putStrLn ""
		putStrLn "Práctica Haskell de Paradigmas de Programación Curso 2015/2016 GIS"
		putStrLn "------------------------------------------------------------------"
		putStrLn "Selecciona una opción:"
		putStrLn "0. Mostrar todos los artículos."
		putStrLn "1. Mostrar los títulos de los artículos ordenados alfabéticamente y publicados en un año dado."
		putStrLn "2. Mostrar el listado de revistas en las que se han publicado los artículos de toda la colección."
		putStrLn "3. Dado un acrónimo, buscarlo en los diferentes artículos y mostrar los títulos de aquellos que contengan el acŕonimo."
		putStrLn "4. Dado el nombre de una revista y un acrónimo, mostrar los títulos de los artículos publicados en dicha revista que contengan el acrónimo."
		putStrLn "5. Dado un año de publicación, mostrar para cada artículo publicado en ese año el listado de acrónimos que contiene acompañados de sus formas expandidas."
		putStrLn "6. Dado un identificador de artículo, mostrar un listado de los acrónimos que contiene, acompañado del número de veces que aparece cada acrónimo en el artículo."
		putStrLn "7. Mostrar los títulos e identificador de todos aquellos artículos que no contengan ningún acrónimo."
		putStrLn "8. Dado el nombre de una revista, mostrar toda la información de los artículos publicados en dicha revista."
		putStrLn "9. Agrupar artículos por acrónimos."
		putStrLn "10. Mostrar artículos (sólo id y título) ordenados cronológicamente (más antiguo a más nuevo)."
		putStrLn "11. Salir."
		
		option <- readLn
		
		if option > 11 || option < 0 then 
			do
				putStrLn "Opción elegida incorrecta."
				putStrLn ""
				main
		else
			case option of
				0 -> do
						putStrLn (show articles_list)
						main
				1 -> do
						putStrLn "Introduce un año: "
						year <- readLn
						putStrLn (unlines (articlesByYear articles_list year))		
						main
				2 -> do
						putStrLn (unlines (sourcesOfArticles articles_list))
						main			
				3 -> do
						putStrLn "Introduce un acrónimo: "
						acronym <- readLn
						putStrLn (unlines (articlesWithAcronym articles_list acronym))		
						main
				4 -> do
						putStrLn "Introduce el nombre de una revista: "
						source <- readLn
						putStrLn "Introduce un acrónimo: "
						acronym <- readLn
						putStrLn (unlines (articlesWithSourceAndAcronym articles_list acronym source))		
						main
				5 -> do
						putStrLn "Introduce un año: "
						year <- readLn
						putStrLn (unlines (meaningsAcronymsFromYear articles_list year))		
						main
				6 -> do
						putStrLn "Introduce un ID: "
						idArticle <- readLn
						putStrLn (show (acronymsFromId articles_list idArticle))		
						main	
				7 -> do
						sequence_ $ map print (articlesWithoutAcronyms articles_list)	
						main	
				8 -> do
						putStrLn "Introduce el nombre de una revista: "
						source <- readLn
						putStrLn (show (articlesFromSource articles_list source))		
						main	
				9 -> do
						putStrLn (show (clusterArticles articles_list))		
						main			
				10 -> do
						sequence_ $ map print (articlesSortedByYear articles_list)
						main			
				11 -> do
						putStrLn "Ha salido del programa."