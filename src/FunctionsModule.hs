
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
													string = (title x) ++ " --> " ++ (show (acronyms_list x)) ++ "\n"		
													
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