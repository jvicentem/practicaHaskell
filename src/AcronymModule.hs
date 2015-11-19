
module AcronymModule (Acronym(..), getListOfAcronyms, removeCharsForOperations, timesAcronyms) where

import Data.List
import Data.Char
import StringOperationsModule (removeCharacter, replaceCharacter)

data Acronym = Acr {
					minAcronym :: String, -- Contiene el acrónimo
					maxAcronym :: String -- Contiene la forma expandida
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

{- getAcronymsWithoutMeaning:
Función que obtiene todos los acrónimos con su significado.
El primer parámetro es una lista de String y cada String es una línea de contenido
del artículo.
-}
getAcronymsWithMeaning :: [String] -> [Acronym]
getAcronymsWithMeaning [] = []
getAcronymsWithMeaning (x:xs) =  nub (
										(abstractAcronyms lineToWords)
										++
										(getAcronymsWithMeaningImpl lineToWords 0 lineToWords [])
										++
										(getAcronymsWithMeaning (xs))
						   		 )
						   		
						   		 where lineToWords = words x
 	
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
		cleanedAcronym = removeCharsForOperations x -- Se quitan los paréntesis del acrónimo		

{- getMeaningForAcronym:
Función que obtiene un significado para un acrónimo.
-}
getMeaningForAcronym :: String->[String]->Int -> String
getMeaningForAcronym "" _ _ = []
getMeaningForAcronym _ [] _ = []
getMeaningForAcronym acr (x:xs) positionAcr = 
							getMeaningForAcronymImpl acr (x:xs) positionAcr

getMeaningForAcronymImpl :: String->[String]->Int -> String
getMeaningForAcronymImpl _ [] _ = []
getMeaningForAcronymImpl "" _ _ = []
getMeaningForAcronymImpl acr@(x:xs) wordsList@(y:ys) positionAcr =
	if meanings2 /= [] then
		last meanings2
	else
		if meanings1 /= [] then
			last meanings1
		else
			if meanings3 /= [] then
				last meanings3
			else
				if meanings4 /= [] then
					last meanings4
				else []
	where
		meanings2 = criterion2 acr (y:ys) positionAcr
		meanings1 = criterion1 acr (y:ys) positionAcr
		meanings3 = criterion3 acr (y:ys) positionAcr
		meanings4 = criterion4 acr (y:ys) positionAcr
												   				
{- isAcronym:
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

{- isIndex:
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
	 
{- cleanWord:
Función que elimina de una palabra ciertos caracteres.
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

{- removeCharsForOperations:
Función que elimina ciertos caracteres de una palabra. Elimina más caracteres 
que la función cleanWord-}
removeCharsForOperations :: String -> String
removeCharsForOperations [] = []
removeCharsForOperations (x:xs) = 
					removeCharacter ';'(
						removeCharacter ':' (
							removeCharacter ',' (
								removeCharacter '.' (
									removeCharacter ']'(
				   						removeCharacter '[' (
											removeCharacter ')' (
												removeCharacter '(' (x:xs)
											)
										)							
				   					)
				   				)	
				   			)	
				   		)					
				   	)  			
					  					   	
{- validWord:
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

{- realAcronymLength:
Función que mide la longitud real de un acrónimo.
-}
realAcronymLength :: String -> Int
realAcronymLength "" = 0
realAcronymLength (x:xs) = length (removeCharsForOperations (x:xs))  					
						   			  
-- Sólo las iniciales			 
criterion2 :: String->[String]->Int -> [String]
criterion2 "" _ _ = [""]
criterion2 _ [] _ = [""]
criterion2 acronym (x:xs) positionAcr = criterion2Impl acronymCleaned (x:xs) [] positionAcr 0 acronymCleaned
											where acronymCleaned = map (\a -> toLower a) (removeCharsForOperations acronym)

criterion2Impl :: String->[String]->[String]->Int->Int->String -> [String]
criterion2Impl "" _ _ _ _ "" = []
criterion2Impl "" (y:ys) buffer acrPos i originalAcr = 	if length buffer == length originalAcr then
															(nub (filter (/="") [intercalate " " (reverse buffer)]))
															++
															criterion2Impl originalAcr (y:ys) [] acrPos i originalAcr
														else 
															criterion2Impl originalAcr (y:ys) [] acrPos i originalAcr
criterion2Impl "" [] buffer _ _ originalAcr = if length buffer == length originalAcr then
												nub (filter (/="") [intercalate " " (reverse buffer)])
											  else 
												[]
criterion2Impl _ [] _ _ _ _ = []
criterion2Impl (x:xs) (y:ys) buffer acrPos i originalAcr = 
	if i < acrPos then
		if (x == (toLower (head y))) then
			if validWord y then
				criterion2Impl (xs) (ys) (y:buffer) acrPos (i+1) originalAcr
			else
				if (length (x:xs) == 1) then
					criterion2Impl (xs) (ys) (y:buffer) acrPos (i+1) originalAcr
				else
					criterion2Impl originalAcr (ys) [] acrPos (i+1) originalAcr
		else
			criterion2Impl originalAcr (ys) [] acrPos (i+1) originalAcr
	else
		if length buffer == length originalAcr then
			nub (filter (/="") [intercalate " " (reverse buffer)])
		else 
			[]
	  

-- Las iniciales y una palabra en el medio
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
criterion1 :: String->[String]->Int -> [String]
criterion1 "" _ _ = [""]
criterion1 _ [] _ = [""]
criterion1 acronym (x:xs) positionAcr = criterion1Impl acronymCleaned (x:xs) [] positionAcr 0 acronymCleaned False
											where acronymCleaned = map (\a -> toLower a) (removeCharacter '-' (removeCharsForOperations acronym))

criterion1Impl :: String->[String]->[String]->Int->Int->String->Bool -> [String]
criterion1Impl "" _ _ _ _ "" _ = []
criterion1Impl "" (y:ys) buffer acrPos i originalAcr _ = if (length buffer == length originalAcr) || (length buffer == (length originalAcr)+1) then
															if (length buffer == (length originalAcr)+1) && (((reverse buffer)!!0)!!0 == ((reverse buffer)!!1)!!0) then
																(nub (filter (/="") [intercalate " " (tail (reverse buffer)) ]))
																++
																criterion1Impl originalAcr (y:ys) [] acrPos i originalAcr False																
															else
																(nub (filter (/="") [intercalate " " (reverse buffer)]))
																++
																criterion1Impl originalAcr (y:ys) [] acrPos i originalAcr False
														 else
														 	criterion1Impl originalAcr (y:ys) [] acrPos i originalAcr False
criterion1Impl "" [] buffer _ _ originalAcr _ = if (length buffer == length originalAcr) || (length buffer == (length originalAcr)+1) then
													nub (filter (/="") [intercalate " " (reverse buffer)])
									  			else 
													[]
criterion1Impl _ [] _ _ _ _ _ = []
criterion1Impl (x:xs) (y:ys) buffer acrPos i originalAcr alreadyWordInMiddle = 
	if i < acrPos then
		if (x == (toLower (head y))) then
			if validWord y then
				criterion1Impl (xs) (ys) (y:buffer) acrPos (i+1) originalAcr alreadyWordInMiddle
			else
				if (length (x:xs) == 1) then
					criterion1Impl (xs) (ys) (y:buffer) acrPos (i+1) originalAcr alreadyWordInMiddle
				else
					criterion1Impl originalAcr (ys) [] acrPos (i+1) originalAcr False 
		else
			if (not alreadyWordInMiddle) && (validWord y) && (length buffer >= 1) then
				criterion1Impl (x:xs) (ys) (y:buffer) acrPos (i+1) originalAcr True
			else
				criterion1Impl originalAcr (ys) [] acrPos (i+1) originalAcr False 
	else
		if (length buffer == length originalAcr) || (length buffer == (length originalAcr)+1) then
			nub (filter (/="") [intercalate " " (reverse buffer)])
		else 
			[] 

{-Cuando el acrónimo está formado por caracteres de una misma palara más la palabra siguiente-}
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
			if ((partOfWordToLower) == (acrToLower)) && (validWord wordCleanedSafe) && (realAcronym /= wordCleaned) then -- Si la palabra tiene el acrónimo como substirng y es válida...
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
		
{- Cuando los caracteres de los acrónioms están desperdigados -}		
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

{- timesAcronyms:
Devuelve el número de veces que aparece un acrónimo en un artículo.
El segundo argumento es un array y cada posición tiene un String que es 
cada línea del contenido del artículo.
-}									
timesAcronyms :: [Acronym]->[String] -> [(String,Int)]
timesAcronyms [] _ = []
timesAcronyms _ [] = []
timesAcronyms (x:xs) (y:ys) = timesAcronymsImpl (x:xs) linesToWords []
							  where 
							  	linesToWords = concat (map (\a -> words a ) (y:ys))

{- El segundo argumento es un array y cada posición tiene un String que es cada
palabra del contenido del artículo.
-}
timesAcronymsImpl :: [Acronym]->[String]->[(String,Int)] -> [(String,Int)]
timesAcronymsImpl [] _ buffer = buffer
timesAcronymsImpl _ [] _ = []
timesAcronymsImpl (x:xs) wordsArticle@(y:ys) buffer = 
					timesAcronymsImpl (xs) wordsArticle ((timesOneAcronym (x) wordsArticle 0):buffer)

{- timesOneAcronym:
Función que devuelve el número de veces que aparece un acrónimo.
-}					
timesOneAcronym :: Acronym->[String]->Int -> (String,Int)
timesOneAcronym acr [] i = (minAcronym acr,i)
timesOneAcronym acr (y:ys) i = if ((minAcronym acr) == y) || ((minAcronym acr) == (removeCharsForOperations y)) then
								timesOneAcronym acr (ys) (i+1)
							   else
							   	timesOneAcronym acr (ys) i				

{- abstractAcronyms:
Función que detecta aquellos acrónimos y sus formas expandidas que vienen
explícitamente en el apartado abstract.
-}
abstractAcronyms :: [String] -> [Acronym]
abstractAcronyms [] = []
abstractAcronyms wordsList@(x:xs) = if (x) == "Abbreviations:" then
										abstractAcronymsImpl (xs) [] []
						  			else
						  				[]
						  	 
abstractAcronymsImpl :: [String]->[String]->[Acronym] -> [Acronym]
abstractAcronymsImpl [] buffer bufferAcr = bufferAcr
abstractAcronymsImpl (x:xs) buffer bufferAcr = if x /= "=" then
												if last x == '.' || last x == ',' then
													abstractAcronymsImpl (xs) [] (Acr {minAcronym = head (reverse buffer) , maxAcronym = intercalate " " ((tail(reverse buffer))++[lastWord])}:bufferAcr)
												else 
													abstractAcronymsImpl (xs) (x:buffer) bufferAcr
											   else
											   	abstractAcronymsImpl (xs) buffer bufferAcr
											   	
											   where lastWord = init x