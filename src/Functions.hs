
module Functions where

import Data.List
import Control.Exception
import DocumentModule
import AcronymModule
import IOOperationsModule

{-
Estos archivos son raros:
../papersUTF8/PMC4348134.xml
../papersUTF8/PMC3570033.xml
-}

files_paths_list = ["../papersUTF8/PMC3900051.xml","../papersUTF8/PMC4453495.xml","../papersUTF8/PMC3162711.xml","../papersUTF8/PMC4407188.xml","../papersUTF8/PMC4397514.xml","../papersUTF8/PMC3458293.xml","../papersUTF8/PMC3949687.xml","../papersUTF8/PMC4310932.xml","../papersUTF8/PMC3152451.xml","../papersUTF8/PMC4375643.xml","../papersUTF8/PMC4443026.xml","../papersUTF8/PMC3095895.xml","../papersUTF8/PMC3514802.xml","../papersUTF8/PMC4334292.xml","../papersUTF8/PMC3955524.xml","../papersUTF8/PMC3679692.xml","../papersUTF8/PMC4408375.xml","../papersUTF8/PMC3667821.xml","../papersUTF8/PMC4389704.xml","../papersUTF8/PMC4270289.xml","../papersUTF8/PMC3907913.xml"]

articles_list = readFiles files_paths_list 

main :: IO ()
main = do
		putStrLn ""
		putStrLn "Práctica Haskell de Paradigmas de Programación Curso 2015/2016 GIS"
		putStrLn "------------------------------------------------------------------"
		putStrLn "Selecciona una opción:"
		putStrLn "1. Mostrar los títulos de los artículos ordenados alfabéticamente y publicados en un año dado."
		putStrLn "2. Mostrar el listado de revistas en las que se han publicado los artículos de toda la colección."
		
		option <- readLn
		
		if option > 9 || option < 1 then 
			do
				putStrLn "Opción elegida incorrecta."
				putStrLn ""
				main
		else
			case option of
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
