
module AcronymModule (Acronym(..), getListOfAcronyms, removeCharsForOperations) where

import Data.List
import Data.Char

data Acronym = Acr {
					minAcronym :: String, -- Contiene el acrónimo
					maxAcronym :: [String] -- Contiene la forma expandida
					}
					
instance Show Acronym where
	show (Acr minAcronym maxAcronym) =
		show "Acronimo: " ++ minAcronym ++ " => " ++
			 "Significado: " ++ show maxAcronym ++ " " 				

instance Eq Acronym where
	(Acr minAcronym1 maxAcronym1) == (Acr minAcronym2 maxAcronym2) = (minAcronym1 == minAcronym2) && (maxAcronym1 == maxAcronym2)  

{- getListOfAcronyms:
Función que devuelve una lista de acrónimos (el acrónimo y su significado) 
de un artículo.
El primer parámetro es una lista de String y cada String es una línea de contenido
del artículo.
-}

getListOfAcronyms :: [String] -> [Acronym]
getListOfAcronyms [] = []
getListOfAcronyms (x:xs) = getAcronymsWithMeaning (x:xs)

{- getAcronymsWithoutMeaning
Función que obtiene todos los acrónimos con su significado.
El primer parámetro es una lista de String y cada String es una línea de contenido
del artículo.
-}
getAcronymsWithMeaning :: [String] -> [Acronym]
getAcronymsWithMeaning [] = []
getAcronymsWithMeaning (x:xs) =  removeDuplicatedAcronyms (
									(getAcronymsWithMeaningImpl lineToWords 0 lineToWords [])
									++
									(getAcronymsWithMeaning (xs))
						   		 )
						   		
						   		 where lineToWords = words x

{-getAcronymsWithoutMeaningImpl :: [String] -> [Acronym]
getAcronymsWithoutMeaningImpl [] = []
getAcronymsWithoutMeaningImpl (x:xs) = 
   if isAcronym xWithoutWeirdCharacters then
	(Acr {minAcronym = xWithoutWeirdCharacters, maxAcronym=[""]})
	:
	(getAcronymsWithoutMeaningImpl (xs))
   else
 	getAcronymsWithoutMeaningImpl (xs)
 
   where
 	xWithoutWeirdCharacters = cleanWord x -}
 	
getAcronymsWithMeaningImpl :: [String]->Int->[String]->[Acronym] -> [Acronym]
getAcronymsWithMeaningImpl [] _ [] _ = []
getAcronymsWithMeaningImpl [] _ (x:xs) buffer@(y:ys) = buffer
getAcronymsWithMeaningImpl [] _ _ [] = []
getAcronymsWithMeaningImpl wordsList@(x:xs) positionAcr originalWordsList buffer = 
   if isAcronym x then
    getAcronymsWithMeaningImpl (xs) (positionAcr+1) originalWordsList
	((Acr 
		{minAcronym = cleanedAcronym, 
		maxAcronym = (getMeaningForAcronym x originalWordsList positionAcr)
		}
	 ):buffer) 
   else
 	getAcronymsWithMeaningImpl (xs) (positionAcr+1) originalWordsList buffer
 
   where 
		cleanedAcronym = cleanWord x -- Se quitan los paréntesis del acrónimo		


getMeaningForAcronym :: String->[String]->Int -> [String]
getMeaningForAcronym "" _ _ = []
getMeaningForAcronym _ [] _ = []
getMeaningForAcronym acr (x:xs) positionAcr = 
		getMeaningForAcronymImpl acr (x:xs) positionAcr []

getMeaningForAcronymImpl :: String->[String]->Int->[String] -> [String]
getMeaningForAcronymImpl _ [] _ buffer = buffer
getMeaningForAcronymImpl "" _ _ _ = []
getMeaningForAcronymImpl acr@(x:xs) wordsList@(y:ys) positionAcr buffer =
				getMeaningForAcronymImpl acr (ys) (positionAcr+1) (meanings++buffer) 
				where
					meanings = -- (criterion2 acr (y:ys) positionAcr)
							-- 	++
							   (criterion3 acr (y:ys) positionAcr)
							 --    ++
							 --   (criterion4 acr (y:ys) positionAcr)
												   				
	
{- isAcronym
Función que comprueba si un String es un acrónimo o no.

Entiendo que un String es un acrónimo si:
- Está entre paréntesis
- Si no es un índice (por ejemplo: phase III. III no es un acrónimo)
- Tiene más de un carácter
- Su primer carácter es una letra en mayúsculas
- Si el resto de caracteres que lo componen son:
	- Letras en mayúscula
	- Números
	- El carácter '-'
- Si su último carácter es una letra o un número
- Si el último carácter del String (no el acrónimo), además de cumplir lo anterior,
es un '.' , ',' , ':' ó ';'
-}							 	
isAcronym :: String -> Bool
isAcronym [] = False
isAcronym (x:xs) = isAcronymImpl (x:xs) 0
		   				
isAcronymImpl :: String->Int -> Bool
isAcronymImpl [] flag = flag == 3
isAcronymImpl acr 3 = if acr == [] then 
						True 
					  else
					  	if ((length acr) == 1) then
					  		if (acr == "." || acr == "," || acr == ":" || acr == ";") then
					  			True
					  		else
					  			False
					  	else
					  		False
isAcronymImpl ('(':xs) _ = isAcronymImpl (xs) 1
isAcronymImpl (x:xs) flag = if flag == 1 then -- El siguiente caracter de '('
					  			if (isUpper x) && (not (isIndex (x:xs))) && ((length (x:xs)) > 2) then
					  				isAcronymImpl (xs) 2
					  			else
					  				False								
							else
								if flag == 0 then -- Si es 0 eso quiere decir que no tiene '(' y directamente no es acrónimo
									False 
								else -- Si flag es 2, se refiere a los caracteres restantes no vistos antes
						  			if (isUpper x) || (x == '-') || (isDigit x) then
										isAcronymImpl (xs) 2
						   			else
						   				if (x == ')') then
						   					isAcronymImpl (xs) 3
						   				else
						   					False											   				
{- isIndex
Función que determina si un String es un índice (I,II,III,IV...XX) o no.

También hay algunos índices que son del tipo "B-III-A".

Contemplo sólo hasta el número 20 (XX en números romanos).
-}
isIndex :: String -> Bool
isIndex [] = False
isIndex (_:'I':'-':_) = True
isIndex (_:'I':'I':'-':_) = True
isIndex (_:'I':'I':'I':'-':_) = True
isIndex (_:'I':'V':'-':_) = True
isIndex (_:'V':'-':_) = True
isIndex (_:'V':'I':'-':_) = True
isIndex (_:'V':'I':'I':'-':_) = True
isIndex (_:'V':'I':'I':'I':'-':_) = True
isIndex (_:'I':'X':'-':_) = True
isIndex (_:'X':'-':_) = True
isIndex (_:'X':'I':'-':_) = True
isIndex (_:'X':'I':'I':'-':_) = True
isIndex (_:'X':'I':'I':'I':'-':_) = True
isIndex (_:'X':'I':'V':'-':_) = True
isIndex (_:'X':'V':'-':_) = True
isIndex (_:'X':'V':'I':'-':_) = True
isIndex (_:'X':'V':'I':'I':'-':_) = True
isIndex (_:'X':'V':'I':'I':'I':'-':_) = True
isIndex (_:'X':'I':'X':'-':_) = True
isIndex (_:'X':'X':'-':_) = True
isIndex (x:xs) = (x:xs) == "I" || (x:xs) == "II" || (x:xs) == "III"  
				|| (x:xs) == "IV" || (x:xs) == "V" || (x:xs) == "VI"
 				|| (x:xs) == "VII" || (x:xs) == "VIII" || (x:xs) == "IX" 
				|| (x:xs) == "X" || (x:xs) == "XI" || (x:xs) == "XII"
				|| (x:xs) == "XIII" || (x:xs) == "XIV" || (x:xs) == "XV"
				|| (x:xs) == "XVI" || (x:xs) == "XVII" || (x:xs) == "XVIII"
				|| (x:xs) == "XIX" || (x:xs) == "XX"
	 
			   			   			   			   			   			   			   			   			    				   	
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

replaceCharacter :: Char->Char->String -> String
replaceCharacter _ _ [] = []
replaceCharacter char1@_ char2@_ (x:xs) = if x == char1 then
											char2:replaceCharacter char1 char2 (xs)
										  else 
											x:replaceCharacter char1 char2 (xs)									

{- cleanWord
Función que elimina de una palabra ciertos caracteres.

Pondría esta función en StringOperationsModule, pero al importar el módulo
StringOperationsModule en éste se crea un ciclo, resultando en error de 
compilación.
-}									
cleanWord :: String -> String
cleanWord [] = []
cleanWord (x:xs) = removeCharacter ']'(
				   	removeCharacter '[' (
						removeCharacter ')' (
							removeCharacter '(' (x:xs)
							)
						)							
				   	)
				   	
{- validWord

Función que determina si una palabra es válida o no para formar parte de un 
significado. No es válida si la palabra es un acrónimo o tiene como último 
carácter un '.', ',', ':', ';'.

Si la palabra no es un acrónimo pero tiene como último carácter alguno de los
mencionados anteriormente, es posible que sí forme parte del significado si se 
trata de la última palabra, pero esto es algo que regulará 
cada función que hace uso de esta (validWord).
-}
validWord :: String -> Bool
validWord [] = False
validWord (x:xs) = validWordImpl (x:xs) False

validWordImpl :: String->Bool -> Bool
validWordImpl (x:xs) False = if (not (isAcronym (x:xs))) then
								validWordImpl (x:xs) True
							 else
							 	False					 	
validWordImpl [] _ = False
validWordImpl (x:xs) _ = 
				if lastCharacter == ',' || lastCharacter == '.' || lastCharacter == ':' || lastCharacter == ';' then
					False
				else
					True	 
				where lastCharacter = last (x:xs)					


				   	
{- removeDuplicatedAcronyms 
Función que elimina los acrónimos duplicados de una lista de acrónimos.
-}									
removeDuplicatedAcronyms :: [Acronym] -> [Acronym]
removeDuplicatedAcronyms [] = []
removeDuplicatedAcronyms (x:xs) = nub (x:xs)

{- findMeaningsForAcronyms

findMeaningsForAcronyms :: [Acronym]->[String]-> [Acronym]
findMeaningsForAcronyms [] _ = []
findMeaningsForAcronyms _ [] = []
findMeaningsForAcronyms (x:xs) (y:ys) = findMeaningsForAcronymsImpl (x:xs) (y:ys) 

findMeaningsForAcronymsImpl :: [Acronym]->[String] -> [Acronym]
findMeaningsForAcronymsImpl [] _  = []
findMeaningsForAcronymsImpl _ []  = []
findMeaningsForAcronymsImpl (x:xs)(y:ys) = (findMeaningForAcronym (y:ys) x []):findMeaningsForAcronymsImpl (xs) (y:ys)
-}
{- findMeaningAcronym
Encuentra el significado de un acrónimo. 

Como parámetro de entrada se le pasa a la función en primer lugar una lista
de Strings (cada String es una línea del contenido del artículo).
							
findMeaningForAcronym :: [String]->Acronym->[String]-> Acronym
findMeaningForAcronym [] acr@(Acr minAcronym maxAcronym) buffer = Acr {minAcronym=minAcronym,maxAcronym=buffer}
findMeaningForAcronym (x:xs) acr@(Acr minAcronym maxAcronym) buffer = 
													findMeaningForAcronym (xs) acr (meanings++buffer) 
													where
														meanings = (criterion2 minAcronym x)++(criterion3 minAcronym x)++(criterion4 minAcronym x)
-}								 

realAcronymLength :: String -> Int
realAcronymLength "" = 0
realAcronymLength (x:xs) = length (removeCharsForOperations (x:xs))  		
				
removeCharsForOperations :: String -> String
removeCharsForOperations "" = ""
removeCharsForOperations (x:xs) = 
			removeCharacter ';' (
				removeCharacter ':' (
					removeCharacter '.'(
					   	removeCharacter ',' (
							removeCharacter ')' (
								removeCharacter '(' (x:xs)
								)
							)							
				   	)
				 )
			)						   			  
			 
criterion2 :: String->[String]->Int -> [String]
criterion2 "" _ _ = [""]
criterion2 _ [] _ = [""]
criterion2 acronym (x:xs) positionAcr = criterion2Impl acronymCleaned (x:xs) [] positionAcr 0 acronymCleaned
											where acronymCleaned = map (\a -> toLower a) (removeCharsForOperations acronym)

criterion2Impl :: String->[String]->[String]->Int->Int->String -> [String]
criterion2Impl "" _ _ _ _ "" = []
criterion2Impl "" (y:ys) buffer acrPos i originalAcr = (filter (/="") [intercalate " " (reverse buffer)])
														++
														criterion2Impl originalAcr (y:ys) [] acrPos i originalAcr
criterion2Impl "" [] buffer _ _ _ = filter (/="") [intercalate " " (reverse buffer)]
criterion2Impl _ [] _ _ _ _ = []
criterion2Impl (x:xs) (y:ys) buffer acrPos i originalAcr = 
	if i < acrPos then
		if (x == head y) then
			if not wordWeirdChar then
				criterion2Impl (xs) (ys) (y:buffer) acrPos (i+1) originalAcr
			else
				if (length (x:xs) == 1) then
					criterion2Impl (xs) (ys) (y:buffer) acrPos (i+1) originalAcr
				else
					criterion2Impl originalAcr (ys) [] acrPos (i+1) originalAcr
		else
			criterion2Impl (x:xs) (ys) [] acrPos (i+1) originalAcr
	else
		filter (/="") [intercalate " " (reverse buffer)]
				
	where 
		lastChar = last y
		wordWeirdChar = lastChar == '.' || lastChar == ',' || lastChar == ';' || lastChar == ':'		  

criterion1 :: String->[String]->Int -> [String]
criterion1 "" _ _ = [""]
criterion1 _ [] _ = [""]
criterion1 acronym (x:xs) positionAcr = criterion1Impl acronymCleaned (x:xs) [] positionAcr 0 acronymCleaned False
											where acronymCleaned = map (\a -> toLower a) (removeCharacter '-' (removeCharsForOperations acronym))

criterion1Impl :: String->[String]->[String]->Int->Int->String->Bool -> [String]
criterion1Impl "" _ _ _ _ "" _ = []
criterion1Impl "" (y:ys) buffer acrPos i originalAcr _ = (filter (/="") [intercalate " " (reverse buffer)])
														++
														criterion1Impl originalAcr (y:ys) [] acrPos i originalAcr False
criterion1Impl "" [] buffer _ _ _ _ = filter (/="") [intercalate " " (reverse buffer)]
criterion1Impl _ [] _ _ _ _ _ = []
criterion1Impl (x:xs) (y:ys) buffer acrPos i originalAcr alreadyWordInMiddle = 
	if i < acrPos then
		if (x == head y) then
			if not wordWeirdChar then
				criterion1Impl (xs) (ys) (y:buffer) acrPos (i+1) originalAcr alreadyWordInMiddle
			else
				if (length (x:xs) == 1) then
					criterion1Impl (xs) (ys) (y:buffer) acrPos (i+1) originalAcr alreadyWordInMiddle
				else
					criterion1Impl originalAcr (ys) [] acrPos (i+1) originalAcr False
		else
			if (not alreadyWordInMiddle) && (not wordWeirdChar) && (length buffer >= 1) then
				criterion1Impl (x:xs) (ys) (y:buffer) acrPos (i+1) originalAcr True
			else
				criterion1Impl originalAcr (ys) [] acrPos (i+1) originalAcr False
	else
		filter (/="") [intercalate " " (reverse buffer)]
				
	where 
		lastChar = last y
		wordWeirdChar = lastChar == '.' || lastChar == ',' || lastChar == ';' || lastChar == ':'	
{- criterion1
Función que obtiene un significado de un acrónimo a partir de un acrónimo una
lista de palabras.

En esta función se sigue el criterio de que un significado de acrónimo lo es si
la primera letra de la palabra i-ésima es igual que la letra i-ésima del acrónimo.
También sigue le criterio de si hay una palabra entre medias de las palabras que
tienen iniciales que coinciden con las letras del acrónimo.
En todo momento se tiene en cuenta si una palabra esa acrónimo o no, ya que 
si es acrónimo no puede formar parte de un significado.

En realidad se invoca el método criterion2Impl, que como parámetros de entrada tiene:
- El acrónimo. Este parámetro se usará para trabajar con él.
- La lista de palabras
- Una lista donde se irán almacenando las palabras que en teoría dan significado
al acrónimo.
- El acrónimo. En este caso, no se trabaja sobre él.
- Una parámetro de entrada booleano que sirve para indicar si ya hay una palabra
en el medio o no. Inicialmente vale False.
-}

criterion3 :: String->[String]->Int -> [String]
criterion3 "" _ _ = [""]
criterion3 _ [] _ = [""]
criterion3 acronym (x:xs) positionAcr = criterion3Impl acronym (x:xs) [] positionAcr 0

criterion3Impl :: String->[String]->[String]->Int->Int -> [String]
criterion3Impl acr [] buffer _ _ = filter (/="") [intercalate " " buffer]
criterion3Impl acr wordsList buffer@[x,y] positionAcr i = 
											(intercalate " " buffer)
											:
											(criterion3Impl acr wordsList [] positionAcr i)
criterion3Impl acr@(x:xs) wordsList@(y:ys) buffer positionAcr i =
	if (i < positionAcr) then
		if (isAcronym y) then 
			criterion3Impl (x:xs) (ys) buffer positionAcr (i+1)  
		else -- Si la palabra en cuestión no se trata de un acrónimo...
			if ((partOfWordToLower) == (acrToLower)) && (validWord wordCleanedSafe) then -- Si la palabra tiene el acrónimo como substirng y es válida...
				criterion3Impl (x:xs) (ys) (wordCleanedSafe:nextWord:buffer) positionAcr (i+1)
			else
				criterion3Impl (x:xs) (ys) buffer positionAcr (i+1)
	else
		criterion3Impl acr [] buffer positionAcr i				
			   			  
	where
		realAcronym = removeCharsForOperations acr
		acrToLower = map (\a->toLower a) realAcronym
		partOfWordToLower = map (\a->toLower a) (take lengthAcronym wordCleanedSafe)
		lengthAcronym = realAcronymLength acr			   			  
		wordCleaned = cleanWord y 
		wordCleanedSafe = if wordCleaned == " " then " " else wordCleaned 
		nextWord = if (ys) == [] then "" else cleanWord (head (ys))
		
criterion4 :: String->[String]->Int -> [String]
criterion4 "" _ _ = []
criterion4 _ [] _ = []
criterion4 acronym (x:xs) positionAcr = criterion4Impl acronym (x:xs) [] acronym positionAcr 0

criterion4Impl :: String->[String]->[String]->String->Int->Int -> [String]
criterion4Impl acr [] buffer originalAcronym _ _ = filter (/="") [intercalate " " (reverse buffer)]
criterion4Impl [] wordsList buffer originalAcronym positionAcr i = 
	(intercalate " " (reverse buffer))
	:
	(criterion4Impl originalAcronym wordsList buffer originalAcronym positionAcr i)
criterion4Impl acr@(x:xs) wordsList@(y:ys) buffer originalAcronym positionAcr i = 
{- Las palabras tienen que ser seguidas. Si hay una que no tiene la siguiente
letra del acrónimo que toca, o la palabra es un acrónimo o
incluso la primera letra de la primera palabra que da significado al acrónimo no 
coincide con la primera letra del acrónimo, se descartan las N últimas palabras 
analizadas.-}
	if (i < positionAcr) then
		if (remainingAcronym /= realAcronym) && (not (isAcronym y))  then
			if (validWord wordCleanedSafe) then
				if (buffer == []) then
					if toLower (head wordCleanedSafe) == (toLower x) then 
						criterion4Impl remainingAcronym (ys) (wordCleanedSafe:buffer) originalAcronym positionAcr (i+1)
					else 
						criterion4Impl originalAcronym (ys) [] originalAcronym positionAcr (i+1)
				else
					criterion4Impl remainingAcronym (ys) (wordCleanedSafe:buffer) originalAcronym positionAcr (i+1)
			else
				if length remainingAcronym == 0 then
					criterion4Impl remainingAcronym (ys) (wordCleanedSafe:buffer) originalAcronym positionAcr (i+1)
				else
					criterion4Impl originalAcronym (ys) [] originalAcronym positionAcr (i+1)
		else
			criterion4Impl originalAcronym (ys) [""] originalAcronym positionAcr (i+1)
	else
		criterion4Impl originalAcronym [] [] originalAcronym positionAcr i
		
	where
		realAcronym = map (\a -> toLower a) (removeCharsForOperations acr)
		remainingAcronym = checkWord realAcronym wordCleanedSafe
		lengthAcronym = realAcronymLength acr			   			  
		wordCleaned = cleanWord y 
		wordCleanedSafe = if wordCleaned == " " then " " else wordCleaned 	

{- checkWord:
Función que devuelve la diferencia entre una palabra y un acrónimo teniendo en
cuenta el orden de aparación de las letras de los acrónimos.

Ejemplo:
Palabra: "methylenetetra-hydrofolate"
Acrónimo: "MTHFR"

Resultado: "R"
-}		
checkWord :: String->String -> String
checkWord acronym@[] _ = acronym
checkWord acronym word@[] = acronym
checkWord (x:xs) (y:ys) = if x == y then
							checkWord (xs) (ys)
						  else
						  	checkWord (x:xs) (ys) 
					  	
{- Las palabras tienen que ser seguidas. Si hay una que no tiene la siguiente
letra del acrónimo que toca, se descartan las N últimas palabras analizadas.-}		
{-				  	
criterion4Impl2 :: String->[String]->[String]->String->String -> [String]
criterion4Impl2 acr [] buffer originalAcronym nextWord = [intercalate " " buffer]
criterion4Impl2 [] wordsList buffer originalAcronym nextWord = (intercalate " " buffer)
																:
													  		   (criterion4Impl2 originalAcronym wordsList buffer originalAcronym nextWord)
criterion4Impl2 acr@(x:xs) wordsList@(y:ys) buffer originalAcronym nextWord = 

	if remainingAcronym /= acr then
		criterion4Impl2 remainingAcronym (ys) (wordCleanedSafe:buffer) originalAcronym ifFailureStartFromThisWord
	else
		criterion4Impl2 originalAcronym (ys) [] originalAcronym nextWord
	
	where
		remainingAcronym = checkWord acr wordCleanedSafe
		lengthAcronym = length acr			   			  
		wordCleaned = cleanWord y 
		wordCleanedSafe = if wordCleaned == " " then " " else wordCleaned	
		ifFailureStartFromThisWord = if (ys) == [] then "" else cleanWord (head (ys))	-}				  	