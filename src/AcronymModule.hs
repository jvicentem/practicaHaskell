
module AcronymModule (Acronym(..), getListOfAcronyms) where

import Data.List
import Data.Char

data Acronym = Acr {
					minAcronym :: String, -- Contiene el acrónimo
					maxAcronym :: String -- Contiene la forma expandida
					}
					
instance Show Acronym where
	show (Acr minAcronym maxAcronym) =
		show "Acronimo: " ++ minAcronym ++ " => " ++
			 "Significado: " ++ maxAcronym ++ " " 				

instance Eq Acronym where
	(Acr minAcronym1 maxAcronym1) == (Acr minAcronym2 maxAcronym2) = (minAcronym1 == minAcronym2) && (maxAcronym1 == maxAcronym2)  

{- getListOfAcronyms:
Función que devuelve una lista de acrónimos (el acrónimo y su significado) 
de un artículo.
-}
getListOfAcronyms :: [String] -> [Acronym]
getListOfAcronyms [] = []
getListOfAcronyms (x:xs) = removeDuplicatedAcronyms ((getListOfAcronymsImpl (words x))++(getListOfAcronyms (xs)))

getListOfAcronymsImpl :: [String] -> [Acronym]
getListOfAcronymsImpl [] = []
getListOfAcronymsImpl (x:xs) = if isAcronym xWithoutParenthesis then
								(Acr {minAcronym = xWithoutParenthesis, maxAcronym=""}):(getListOfAcronymsImpl (xs))
							   else
							 	getListOfAcronymsImpl (xs)
							 
							   where
							 	xWithoutParenthesis = removeCharacter ')' (removeCharacter '(' x)
							 	
{- isAcronym
Función que comprueba si un String es un acrónimo o no.

Entiendo que un String es un acrónimo si:
- Si no es un índice (por ejemplo: phase III. III no es un acrónimo)
- Tiene más de un carácter
- Su primer carácter es una letra en mayúsculas
- Si el resto de caracteres que lo componen son:
	- Letras en mayúscula
	- Números
	- El carácter '-'
- Si su último carácter es una letra o un número
-}							 	
isAcronym :: String -> Bool
isAcronym [] = False
isAcronym (x:xs) = isAcronymImpl (x:xs) 0

isAcronymImpl :: String->Int -> Bool
isAcronymImpl [] _ = True
isAcronymImpl [x] i = (i > 0) && (isUpper x || isDigit x) {- Si el String sólo tiene un elemento y:
															i es mayor que 0 (es el último carácter de un String con más de 1 carácter) y 
															además x (un carácter) es una letra mayúscula o un número, finalmente (tras
															haber hecho las comprobaciones en los caracteres anteriores) el String
															es un acrónimo. 
														  -}
isAcronymImpl (x:xs) i = 
				  		if i == 0 then --Compruebo el primer carácter
				  			if isUpper x && (not (isIndex (x:xs))) then
				  				isAcronymImpl (xs) 1
				  			else
				  				False
				  		else
				  			if isUpper x || x == '-' || isDigit x then
								isAcronymImpl (xs) i
				   			else
				   				False 
{- isIndex
Función que determina si un String es un índice (I,II,III,IV...X) o no.

Contemplo sólo hasta el número 10 (X en números romanos).
-}
isIndex :: String -> Bool
isIndex [] = False
isIndex (x:xs) = (x:xs) == "I" || (x:xs) == "II" || (x:xs) == "III"  
				|| (x:xs) == "IV" || (x:xs) == "V" || (x:xs) == "VI"
 				|| (x:xs) == "VII" || (x:xs) == "VIII" || (x:xs) == "IX" 
				|| (x:xs) == "X"			 
			   			   			   			   			   			   			   			   			    				   	
{- removeCharacter
Función que elimina un carácter de una lista.

Pondría esta función en StringOperationsModule, pero al importar el módulo
StringOperationsModule en éste se crea un ciclo, resultando en error de 
compilación.
-}

removeCharacter :: Char->String -> String
removeCharacter _ [] = []
removeCharacter char@_ (x:xs) = if x == char then
									removeCharacter char (xs)
								else 
									x:removeCharacter char (xs)
{- removeDuplicatedAcronyms 
Función que elimina los acrónimos duplicados de una lista de acrónimos.
-}									
removeDuplicatedAcronyms :: [Acronym] -> [Acronym]
removeDuplicatedAcronyms [] = []
removeDuplicatedAcronyms (x:xs) = nub (x:xs)


									
									




