
module FunctionsModule (articlesByYear, sourcesOfArticles, articlesWithAcronym, 
				  		articlesWithSourceAndAcronym, meaningsAcronymsFromYear, 
				  		acronymsFromId, articlesWithoutAcronyms, articlesFromSource, 
				  		clusterArticles) where

import Data.List
import DocumentModule (Document(..), existsAcronym, existsAcronymAndSource, noAcronyms)
import AcronymModule (Acronym(..), timesAcronyms)
import ClusterModule (Cluster(..), groupArticles, createEmptyCluster)
																												
-- 1				
articlesByYear :: [Document]->Int -> [String]
articlesByYear [] _ = []
articlesByYear (x:xs) yearArticle = articlesByYearImpl (x:xs) yearArticle []

articlesByYearImpl :: [Document]->Int->[String] -> [String]
articlesByYearImpl [] _ buffer = buffer
articlesByYearImpl (x:xs) yearArticle buffer = 
			   if year x == yearArticle then
			   		articlesByYearImpl (xs) yearArticle (insert (title x) buffer)
		  	   else
			 		articlesByYearImpl (xs) yearArticle buffer
-- 2
sourcesOfArticles :: [Document] -> [String]
sourcesOfArticles [] = []
sourcesOfArticles (x:xs) = sourcesOfArticlesImpl (x:xs) []

sourcesOfArticlesImpl :: [Document]->[String] -> [String]
sourcesOfArticlesImpl [] buffer = buffer
sourcesOfArticlesImpl (x:xs) buffer = sourcesOfArticlesImpl (xs) (insert (source x) buffer)

-- 3
articlesWithAcronym :: [Document]->String -> [String]
articlesWithAcronym [] _ = []
articlesWithAcronym _ "" = []
articlesWithAcronym (x:xs) acronym = articlesWithAcronymImpl (x:xs) acronym []

articlesWithAcronymImpl :: [Document]->String->[String] -> [String]
articlesWithAcronymImpl [] _ buffer = buffer
articlesWithAcronymImpl _ "" _ = []
articlesWithAcronymImpl (x:xs) acronym buffer = 
			if existsAcronym acronym x then
				articlesWithAcronymImpl (xs) acronym (insert (title x) buffer)
 	 		else
 				articlesWithAcronymImpl (xs) acronym buffer
									 	
-- 4
articlesWithSourceAndAcronym :: [Document]->String->String -> [String]
articlesWithSourceAndAcronym [] _ _ = []
articlesWithSourceAndAcronym _ "" _ = []
articlesWithSourceAndAcronym _ _ "" = []
articlesWithSourceAndAcronym (x:xs) acronym source = articlesWithSourceAndAcronymImpl (x:xs) acronym source []

articlesWithSourceAndAcronymImpl :: [Document]->String->String->[String] -> [String]
articlesWithSourceAndAcronymImpl [] _ _ buffer = buffer
articlesWithSourceAndAcronymImpl _ "" _ _ = []
articlesWithSourceAndAcronymImpl _ _ "" _ = []
articlesWithSourceAndAcronymImpl (x:xs) acronym source buffer = 
	if existsAcronymAndSource acronym source x then
		articlesWithSourceAndAcronymImpl (xs) acronym source (insert (title x) buffer)
	else
		articlesWithSourceAndAcronymImpl (xs) acronym source buffer		
									 													 					
-- 5
meaningsAcronymsFromYear :: [Document]->Int -> [String]
meaningsAcronymsFromYear [] _ = []
meaningsAcronymsFromYear (x:xs) yearArticle  =  meaningsAcronymsFromYearImpl (x:xs) yearArticle []
													
meaningsAcronymsFromYearImpl :: [Document]->Int->[String] -> [String]
meaningsAcronymsFromYearImpl [] _ buffer = buffer
meaningsAcronymsFromYearImpl (x:xs) yearArticle  buffer =  
		if year x == yearArticle then
			meaningsAcronymsFromYearImpl (xs) yearArticle (insert (string) buffer)
	   	else
			meaningsAcronymsFromYearImpl (xs) yearArticle buffer

	   	where
			string = (title x) ++ " --> " ++ (show (acronyms_list x)) ++ "\n"		
													
-- 6
acronymsFromId :: [Document]->Int -> [(String,Int)]
acronymsFromId [] _ = []
acronymsFromId (x:xs) idArticle = if id_document x == idArticle then
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
articlesWithoutAcronymsImpl (x:xs) buffer = 
		if noAcronyms x then 
			articlesWithoutAcronymsImpl (xs) ( (id_document x, title x):buffer)		
		else
			articlesWithoutAcronymsImpl (xs) buffer		
												
-- 8
articlesFromSource :: [Document]->String -> [Document]
articlesFromSource [] _ = []
articlesFromSource (x:xs) sourceArticles = articlesFromSourceImpl (x:xs) sourceArticles []

articlesFromSourceImpl :: [Document]->String->[Document] -> [Document]
articlesFromSourceImpl [] _ buffer = buffer
articlesFromSourceImpl (x:xs) sourceArticles buffer = 
			if source x == sourceArticles then
				articlesFromSourceImpl (xs) sourceArticles (insert (x) buffer)
			else
				articlesFromSourceImpl (xs) sourceArticles buffer												
									 
-- 9
clusterArticles :: [Document] -> (Cluster,[Cluster])
clusterArticles [] = (createEmptyCluster,[])
clusterArticles (x:xs) = groupArticles (x:xs)																										 												 	