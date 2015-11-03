
module AcronymModule (Acronym(..), getListOfAcronyms) where

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
-}

getListOfAcronyms :: [String] -> [Acronym]
getListOfAcronyms [] = []
getListOfAcronyms (x:xs) = findMeaningsForAcronyms (getAcronymsWithoutMeaning (x:xs)) (x:xs) 

{- getAcronymsWithoutMeaning
Función que obtiene todos los acrónimos pero sin su significado.
-}
getAcronymsWithoutMeaning :: [String] -> [Acronym]
getAcronymsWithoutMeaning [] = []
getAcronymsWithoutMeaning (x:xs) =	(removeDuplicatedAcronyms (
										(getAcronymsWithoutMeaningImpl (words x))
										++
										(getAcronymsWithoutMeaning (xs))
							   		))



getAcronymsWithoutMeaningImpl :: [String] -> [Acronym]
getAcronymsWithoutMeaningImpl [] = []
getAcronymsWithoutMeaningImpl (x:xs) = if isAcronym xWithoutWeirdCharacters then
								(Acr {minAcronym = xWithoutWeirdCharacters, maxAcronym=[""]}):(getAcronymsWithoutMeaningImpl (xs))
							   else
							 	getAcronymsWithoutMeaningImpl (xs)
							 
							   where
							 	xWithoutWeirdCharacters = cleanWord x
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
-}
validWord :: String -> Bool
validWord [] = False
validWord (x:xs) = validWordImpl (x:xs) False

validWordImpl :: String->Bool -> Bool
validWordImpl (x:xs) False = if (not (isAcronym wordCleanedSafe)) then
								validWordImpl (x:xs) True
							 else
							 	False
							 	
							 where
			  					wordCleaned = cleanWord (x:xs) 
			  					wordCleanedSafe = if wordCleaned == [] then " " else wordCleaned							 	
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
-}
findMeaningsForAcronyms :: [Acronym]->[String]-> [Acronym]
findMeaningsForAcronyms [] _ = []
findMeaningsForAcronyms _ [] = []
findMeaningsForAcronyms (x:xs) (y:ys) = findMeaningsForAcronymsImpl (x:xs) (y:ys) 

findMeaningsForAcronymsImpl :: [Acronym]->[String] -> [Acronym]
findMeaningsForAcronymsImpl [] _  = []
findMeaningsForAcronymsImpl _ []  = []
findMeaningsForAcronymsImpl (x:xs)(y:ys) = (findMeaningForAcronym (y:ys) x []):findMeaningsForAcronymsImpl (xs) (y:ys)

{- findMeaningAcronym
Encuentra el significado de un acrónimo. 

Como parámetro de entrada se le pasa a la función en primer lugar una lista
de Strings (cada String es una línea del contenido del artículo).
-}							
findMeaningForAcronym :: [String]->Acronym->[String]-> Acronym
findMeaningForAcronym [] acr@(Acr minAcronym maxAcronym) buffer = Acr {minAcronym=minAcronym,maxAcronym=buffer}
findMeaningForAcronym (x:xs) acr@(Acr minAcronym maxAcronym) buffer = 
													findMeaningForAcronym (xs) acr (meanings++buffer) 
													where
														meanings = (criterion2 minAcronym x)++(criterion3 minAcronym x)++(criterion4 minAcronym x)
								   								   			  
{- criterion2
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
criterion2 :: String->String -> [String]
criterion2 "" _ = [""]
criterion2 _ [] = [""]
criterion2 acronym (x:xs) = criterion2Impl acronymWithoutDash (words (x:xs)) [] acronymWithoutDash False
							where acronymWithoutDash = removeCharacter '-' acronym
							{- Elimino el guión del acrónimo para reconocer los
							acrónimos del tipo "TTF-1 = Thyroid transcription factor 1"
							-}
criterion2Impl :: String->[String]->[String]->String->Bool -> [String]
criterion2Impl _ [] buffer originalAcronym _  =  if length buffer == lengthAcronym 
												  || 
												  length buffer == lengthAcronym+1 
											   	 then 
													[intercalate " " (reverse buffer)] 
								 			   	 else []
								 			   
								 			   	 where 
									  				lengthAcronym = length originalAcronym	
									  									
criterion2Impl [] wordsList@(y:ys) buffer originalAcronym@_ alreadyAWordInTheMiddle = (intercalate " " (reverse buffer)):(criterion2Impl originalAcronym (y:ys) [] originalAcronym False)
criterion2Impl acr@(x:xs) wordsList@(y:ys) buffer originalAcronym alreadyAWordInTheMiddle = 
		if ( (head wordCleanedSafe == toLower x) || (head wordCleanedSafe == x) ) 
			&& 
		   (length buffer <= lengthAcronym) && (not (isAcronym wordCleanedSafe)) 
		then
			if (not (validWord wordCleanedSafe)) then -- *1
				if (not alreadyAWordInTheMiddle) then
					if length buffer == lengthAcronym-1 then
						criterion2Impl (xs) (ys) (wordCleanedSafe:buffer) originalAcronym alreadyAWordInTheMiddle
					else
						criterion2Impl originalAcronym (ys) [] originalAcronym False
				else
					if length buffer == lengthAcronym then
						criterion2Impl (xs) (ys) (wordCleanedSafe:buffer) originalAcronym alreadyAWordInTheMiddle
					else
						criterion2Impl originalAcronym (ys) [] originalAcronym False
			else
				criterion2Impl (xs) (ys) (wordCleanedSafe:buffer) originalAcronym alreadyAWordInTheMiddle			
		else
			if (not alreadyAWordInTheMiddle) && (length acr /= lengthAcronym) 
				&& (wordCleanedSafe /= " ") && (not (isAcronym wordCleanedSafe)) then -- *2
				if (not (validWord wordCleanedSafe)) then
					if length buffer == lengthAcronym then
						criterion2Impl (x:xs) (ys) (wordCleanedSafe:buffer) originalAcronym True
					else
						criterion2Impl originalAcronym (ys) [] originalAcronym False
				else
					criterion2Impl (x:xs) (ys) (wordCleanedSafe:buffer) originalAcronym True
								   
			else																							    
				criterion2Impl originalAcronym (ys) [] originalAcronym False									    
			
		where 
			  lengthAcronym = length originalAcronym
			  wordCleaned = cleanWord y 
			  wordCleanedSafe = if wordCleaned == [] then " " else wordCleaned
			  
{- 
1* Si la primera letra de la palabra coincide con la letra que toca comprobar
del acrónimo, y el tamaño del buffer es menor o igual que la longitud del acrónimo
(se pueden seguir teniendo en cuenta palabras para el significado) y la palabra 
no es acrónimo, compruebo si la palabra es válida. Si la palabra no es válida pero
es la última del significado, entonces sí la tengo en cuenta. En caso contrario,
no la tengo en cuenta y eso quiere decir que ese conjunto de palabras no dan 
significado al acrónimo.

2* Si no hay ya una palabra en el medio (alreadyAWordInTheMiddle = True) 
ni se trata de la primera palabra que da significado al acrónimo, ni la palabra 
es en realidad un espacio en blanco, ni tampoco esa palabra es una acrónimo,
entonces tengo encuenta la palabra. Además, compruebo igual que en el punto anterior
si la palabra es válida o no y en función de si es válida o no es válida pero 
es la última, la tengo en cuenta.
Si no se cumple alguna de las condiciones anteriores, eso quiere decir que ese
conjunto de palabras no dan significado al acrónimo.-}

criterion3 :: String->String -> [String]
criterion3 "" _ = [""]
criterion3 _ [] = [""]
criterion3 acronym (x:xs) = criterion3Impl acronym (words (x:xs)) [] 

criterion3Impl :: String->[String]->[String] -> [String]
criterion3Impl acr [] buffer = [intercalate " " buffer]
criterion3Impl acr wordsList buffer@[x,y] = (intercalate " " buffer)
											:
											(criterion3Impl acr wordsList [])
criterion3Impl acr@(x:xs) wordsList@(y:ys) buffer  =
	if (isAcronym wordCleanedSafe) then 
		criterion3Impl (x:xs) (ys) buffer  
	else -- Si la palabra en cuestión no se trata de un acrónimo...
		if ((partOfWordToLower) == (acrToLower)) && (validWord wordCleanedSafe) then -- Si la palabra tiene el acrónimo como substirng y es válida...
			criterion3Impl (x:xs) (ys) (wordCleanedSafe:nextWord:buffer)
		else
			criterion3Impl (x:xs) (ys) buffer
			   			  
	where
		acrToLower = map (\a->toLower a) acr
		partOfWordToLower = map (\a->toLower a) (take lengthAcronym wordCleanedSafe)
		lengthAcronym = length acr			   			  
		wordCleaned = cleanWord y 
		wordCleanedSafe = if wordCleaned == " " then " " else wordCleaned 
		nextWord = if (ys) == [] then "" else cleanWord (head (ys))
		
criterion4 :: String->String -> [String]
criterion4 "" _ = [""]
criterion4 _ [] = [""]
criterion4 acronym (x:xs) = criterion4Impl acronym (words (x:xs)) [] acronym

criterion4Impl :: String->[String]->[String]->String -> [String]
criterion4Impl acr [] buffer originalAcronym = [intercalate " " (reverse buffer)]
criterion4Impl [] wordsList buffer originalAcronym = (intercalate " " (reverse buffer))
														:
													 (criterion4Impl originalAcronym wordsList buffer originalAcronym)
criterion4Impl acr@(x:xs) wordsList@(y:ys) buffer originalAcronym = 
{- Las palabras tienen que ser seguidas. Si hay una que no tiene la siguiente
letra del acrónimo que toca, o la palabra es un acrónimo o
incluso la primera letra de la primera palabra que da significado al acrónimo no 
coincide con la primera letra del acrónimo, se descartan las N últimas palabras 
analizadas.-}

	if remainingAcronym /= acr && (not (isAcronym wordCleanedSafe))  then
		if (validWord wordCleanedSafe) then
			if (buffer == []) then
				if ((head wordCleanedSafe) == x) then 
					criterion4Impl remainingAcronym (ys) (wordCleanedSafe:buffer) originalAcronym
				else 
					criterion4Impl originalAcronym (ys) [] originalAcronym
			else
				criterion4Impl remainingAcronym (ys) (wordCleanedSafe:buffer) originalAcronym
		else
			if length buffer == lengthAcronym-1 then
				criterion4Impl remainingAcronym (ys) (wordCleanedSafe:buffer) originalAcronym
			else
				criterion4Impl originalAcronym (ys) [] originalAcronym
	else
		criterion4Impl originalAcronym (ys) [] originalAcronym
	
	where
		remainingAcronym = checkWord acr wordCleanedSafe
		lengthAcronym = length acr			   			  
		wordCleaned = cleanWord y 
		wordCleanedSafe = if wordCleaned == " " then " " else wordCleaned 	
		
checkWord :: String->String -> String
checkWord acronym@[] _ = acronym
checkWord acronym word@[] = acronym
checkWord (x:xs) (y:ys) = if x == y then
							checkWord (xs) (ys)
						  else
						  	checkWord (x:xs) (ys) 
					  	
{- Las palabras tienen que ser seguidas. Si hay una que no tiene la siguiente
letra del acrónimo que toca, se descartan las N últimas palabras analizadas.-}		
{--				  	
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
		ifFailureStartFromThisWord = if (ys) == [] then "" else cleanWord (head (ys))	--}				  	