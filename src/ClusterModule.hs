
module ClusterModule (Cluster(..), groupArticles, createEmptyCluster) where

import DocumentModule (Document(..), noAcronyms)
import AcronymModule (Acronym(..), timesAcronyms)
import Data.List

data Cluster = Clu { number :: Int,
					 acronym :: String,
					 articles :: [(Int,String)]
					} 
					
instance Show Cluster where
	show (Clu number acronym articles) =
		show "Cluster " ++ show number ++ "\n" ++
			  acronym ++ "\n" ++
			  show articles ++ "\n" ++
			  "" ++ "\n"
			  
instance Eq Cluster where
	(Clu number1 acronym1 articles1) == (Clu number2 acronym2 articles2) = (acronym1 == acronym2) 			  

{- createEmptyCluster:
Función que crea un cluster "vacío".
-}		
createEmptyCluster :: Cluster
createEmptyCluster = Clu {number = -1, acronym = "", articles = []}
					
{- groupArticles:
Función que crea clusters de artículos.
-}
groupArticles :: [Document] -> (Cluster,[Cluster])
groupArticles [] = (createEmptyCluster,[])
groupArticles (x:xs) = articlesToClusters (splitArticles (x:xs) [] [])

{- splitArticles:
Función que separa los artículos en dos grupos, los que no tienen acrónimos y 
los que sí tienen.
-}
splitArticles :: [Document]->[Document]->[Document] -> ([Document],[Document])
splitArticles [] [] [] = ([],[])
splitArticles [] articlesNoAcronyms articlesWithAcronyms = (articlesNoAcronyms,articlesWithAcronyms)
splitArticles (x:xs) articlesNoAcronyms articlesWithAcronyms = 
				if noAcronyms x then
					splitArticles (xs) (x:articlesNoAcronyms) articlesWithAcronyms
				else
					splitArticles (xs) articlesNoAcronyms (x:articlesWithAcronyms)
					
{- articlesToClusters:
Función que devuelve en una única tupla el clúster que contiene los artículos
sin acrónimos y en el otro campo de la tupla una lista de clústers 
-}					
articlesToClusters :: ([Document],[Document]) -> (Cluster,[Cluster])
articlesToClusters ([],[]) = ( createEmptyCluster , [] )
articlesToClusters (articlesNoAcronyms,articlesWithAcronyms) = 
			( Clu {number = 0, acronym = "" ,articles = [((id_document x),(title x))|x<-articlesNoAcronyms]}
			  , 
			  reverse (nub (removeRepeatedClusters (splitArticlesWithAcronyms articlesWithAcronyms 1 [])))
			)		
			
{- splitArticlesWithAcronyms:
Función que recibe todos los artículos con acrónimos y crea clústers a partir de
ellos.
-}			
splitArticlesWithAcronyms :: [Document]->Int->[Cluster] -> [Cluster]
splitArticlesWithAcronyms [] _ [] = []
splitArticlesWithAcronyms [] _ buffer = buffer
splitArticlesWithAcronyms (x:xs) i buffer = 
		    						splitArticlesWithAcronyms (xs) (i+1) ((Clu 
										{number = i
										, 
										acronym = (acronymWithMostAppearances (timesAcronyms (acronyms_list x) (content x)) ("",0) 0)
										, 
										articles = [((id_document x),(title x))]}
									 )
									:buffer
								   ) 

{- acronymWithMostAppearances:
Función que devuelve el acrónimo que aparece más veces en un artículo.
-}					
acronymWithMostAppearances :: [(String,Int)]->(String,Int)->Int -> String
acronymWithMostAppearances [] (acrChosen,timesChosen) _ = acrChosen 
acronymWithMostAppearances ((acr,times):xs) (acrChosen,timesChosen) ap = 
											if times > ap then
												acronymWithMostAppearances (xs) (acr,times) times
											else
												acronymWithMostAppearances (xs) (acrChosen,timesChosen) ap			

{- removeRepeatedClusters:
Función que elimina los clusters repetidos. Realmente no es que los clusters
estén repetidos, sino que inicialmente se crea un clúster por artículo, entonces
esta función se encarga de agrupar aquellos clústers con igual acrónimo.
-}												
removeRepeatedClusters :: [Cluster] -> [Cluster]
removeRepeatedClusters [] = []
removeRepeatedClusters (x:xs) = 
								(removeRepeatedClustersImpl x (xs) (createEmptyCluster) False)
								:
								(removeRepeatedClusters (xs))


removeRepeatedClustersImpl :: Cluster->[Cluster]->Cluster->Bool -> Cluster
removeRepeatedClustersImpl cluster [] cl flag = if flag then cl else cluster
removeRepeatedClustersImpl cluster (x:xs) cl flag = if (acronym cluster) == (acronym x) then
														if flag then
															removeRepeatedClustersImpl cluster (xs) ((
																Clu {
																		number=(number cluster), 
																		acronym=(acronym cluster), 
																		articles=(articles x)++(articles cl)
																	})) True
														else
															removeRepeatedClustersImpl cluster (xs) ((
																Clu {
																		number=(number cluster), 
																		acronym=(acronym cluster), 
																		articles=(articles cluster)++(articles x)
																	})) True															
											  		else
											  			removeRepeatedClustersImpl cluster (xs) cl flag																								