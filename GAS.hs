{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances #-}

module GAS where

import ProblemState
import Data.Maybe
import qualified Data.Map.Strict as M

{-
    Pozițiile tablei de joc, în formă (linie, coloană), unde ambele coordonate
    pot fi negative.
-}
type Position = (Int, Int)

{-
    Culorile pătratelor și cercurilor.
-}
data Color = Red | Blue | Gray
	deriving (Eq, Ord, Show)

{-
    Orientările pătratelor și săgeților.
-}
data Heading = North | South | East | West
	deriving (Eq, Ord)

{-
    Reprezetarea textuală a unei directi.
-}
instance Show Heading where
	show North = "^"
	show South = "v"
	show East  = ">"
	show West  = "<"

{-
    Un obiect de pe tabla de joc: pătrat/ cerc/ săgeată.
-}

data Object = Square Color Heading Position | Circle Color Position | Arrow Heading Position
	deriving (Eq, Ord)

{-
    Reprezetarea textuală a unui obiect.
-}

instance Show Object where
	show (Square color heading _)
		| color == Red = "R" ++ (show heading)
		| color == Blue = "B" ++ (show heading)
		| color == Gray = "G" ++ (show heading)

	show (Circle color _)
		| color == Red = "r"
		| color == Blue = "b"
		| color == Gray = "g"

	show (Arrow heading _) =
		show heading
 
{-
    Un nivel al jocului.
-}
data Level = Level (M.Map Position [Object])
	deriving (Eq, Ord)

{-
    Reprezetarea textuală a unui nivel.
-}
showObjects :: [Object] -> String
showObjects objects
	| (length objects == 1 && length (show (objects !! 0)) == 1) = "  " ++ (show (objects !! 0))
	| (length objects == 1 && length (show (objects !! 0)) == 2) = (show (objects !! 0)) ++ " "
	| otherwise = (show (objects !! 0)) ++ (show (objects !! 1)) 

{-
    Adaugarea obiectelor in string-ul care va fi afisat la apelarea
    functiei show.
-}
addToTable :: [((Int, Int), [Object])] -> String -> Position -> [Object] -> [Int] -> String
addToTable tableList table (x, y) obj tableSize
    | (length table) == 0 = table ++ (concat (take (y - (tableSize !! 0)) (repeat "   |"))) ++ (showObjects obj)
    | (length table) /= 0 && (x == xPrev) = table ++ (concat (take (y - yPrev - 1) (repeat "|   "))) ++ "|" ++ (showObjects obj)
	| (length table) /= 0 && (x /= xPrev) = table ++ (concat (take ((tableSize !! 1) - yPrev) (repeat "|   "))) ++ "\n" ++ 
											(concat (take (x - xPrev - 1) (repeat ((concat (take (tableSize !! 1) (repeat "   |"))) ++ "   \n")))) ++ 
											(concat (take (y - (tableSize !! 0)) (repeat "   |"))) ++ (showObjects obj)
	where ((xPrev, yPrev), _) = last (takeWhile (/= ((x, y), obj)) tableList)

{-
    Reprezetarea textuală a unui nivel.
-}
instance Show Level where
    show (Level level) = let tableList = M.toList level
                             lastElement = fst (last tableList)
                             tableLengthMax = maximum ([x | ((_, x), _) <- (M.toList level)])
                             tableLengthMin = minimum ([x | ((_, x), _) <- (M.toList level)]) 
                             tableSize = [tableLengthMin, tableLengthMax]
                         in (foldl (\ table (pos, obj) -> (addToTable tableList table pos obj tableSize)) "" tableList)
                            ++ (concat (take (tableLengthMax - (snd lastElement)) (repeat "|   ")))
{-
    Nivelul vid, fără obiecte.
-}
emptyLevel :: Level
emptyLevel = Level M.empty

{-
    Adaugă un pătrat cu caracteristicile date la poziția precizată din nivel.
-}
addSquare :: Color -> Heading -> Position -> Level -> Level
addSquare color heading (x, y) (Level level) = Level (M.insertWith (++) (x, y) [(Square color heading (x, y))] level)

{-
    Adaugă un cerc cu caracteristicile date la poziția precizată din nivel.
-}
addCircle :: Color -> Position -> Level -> Level
addCircle color (x, y) (Level level) = Level (M.insertWith (++) (x, y) [(Circle color (x, y))] level)

{-
    Adaugă o săgeată cu caracteristicile date la poziția precizată din nivel.
-}
addArrow :: Heading -> Position -> Level -> Level
addArrow heading (x, y) (Level level) = Level (M.insertWith (++) (x, y) [(Arrow heading (x, y))] level)

{-
    Functie care schimba directia unui patrat.
-}
changeDirection :: (Position, [Object]) -> Level -> (Position, [Object])
changeDirection ((x, y), [(Square color heading pos)]) (Level level)
    | headings == [] = ((x, y), [(Square color heading pos)])
    | otherwise = ((x, y), [(Square color (head headings) pos)])
    where
    obj = fromMaybe [] $ M.lookup (x, y) level
    headings = [ heading | (Arrow heading _) <- obj]

{-
    Functie folosita de M.alter pentru stergerea unui patrat
    de la o pozitie.
-}
alterDrop :: Maybe [Object] -> Maybe [Object]
alterDrop objects
    | (length (fromMaybe [] objects)) < 2 = Nothing
    | otherwise = Just $ drop 1 (fromMaybe [] objects)


{-
    Functie care sterge patratele care urmeaza sa fie mutate
    si le introduce la noile lor pozitii.
-}
updateLevel :: [(Position, [Object])] -> [(Position, [Object])] -> Level -> Level
updateLevel oldObjects newObjects (Level level) = let deleteOldObjects = foldl (\acc x -> (M.alter alterDrop (fst x) acc)) level oldObjects
                                                      changedDirection = map (\x -> changeDirection x (Level deleteOldObjects)) newObjects  
                                                      in Level (foldl (\acc x -> M.alter (\y -> Just ((snd x) ++ (fromMaybe [] y))) (fst x) acc) 
                                                                deleteOldObjects changedDirection)

{-
    Obtine recursiv toate patrate dintr-o stare
    si o directie.
-}
getObjects :: Position -> Heading -> Level -> [(Position, [Object])] -> [(Position, [Object])]
getObjects (x, y) heading (Level oldObjects) newObjects = 
    if obj /= []
    then if (length squares) /= 0 
         then case heading of 
                   North -> getObjects (x - 1, y) heading (Level oldObjects) (newObjects ++ [((x, y), squares)])
                   South -> getObjects (x + 1, y) heading (Level oldObjects) (newObjects ++ [((x, y), squares)])
                   East  -> getObjects (x, y + 1) heading (Level oldObjects) (newObjects ++ [((x, y), squares)])
                   West  -> getObjects (x, y - 1) heading (Level oldObjects) (newObjects ++ [((x, y), squares)])
         else newObjects
    else newObjects
    where
    obj = fromMaybe [] $ M.lookup (x, y) oldObjects
    squares = [s | s <- obj, (length (show s)) == 2]

{-
    Actualizeaza pentru fiecare patrat noua poitie pe care
    o va ocupa dupa mutare.
-}
getNewObjects :: Heading -> [(Position, [Object])] -> [(Position, [Object])]
getNewObjects heading oldObjects = case heading of
                                        North -> map (\((z, t), objects) -> ((z - 1, t), objects)) oldObjects
                                        South -> map (\((z, t), objects) -> ((z + 1, t), objects)) oldObjects
                                        East  -> map (\((z, t), objects) -> ((z, t + 1), objects)) oldObjects
                                        West  -> map (\((z, t), objects) -> ((z, t - 1), objects)) oldObjects

{-
    Mută pătratul de la poziția precizată din nivel. Dacă la poziția respectivă
    nu se găsește un pătrat, întoarce direct parametrul.
-}
move :: Position  -- Poziția
     -> Level     -- Nivelul inițial
     -> Level     -- Nivelul final
move (x, y) (Level level) = if obj /= [] 
							then if (length [t | t <- obj, (length (show t)) == 2 ]) /= 0 
                                 then let (Square _ heading _) = head [t | t <- obj, (length (show t)) == 2 ]
                                          objectsToMove = getObjects (x, y) heading (Level level) []
                                          newObjects = getNewObjects heading objectsToMove
                                      in updateLevel objectsToMove newObjects (Level level)
								 else (Level level)
							else (Level level)
							where
							obj = fromMaybe [] $ M.lookup (x, y) level              

{-
    Instanțiere clasa `ProblemState` pentru jocul nostru.
-}
instance ProblemState Level Position where
    successors (Level level) = map (\x -> (x, (move x (Level level)))) positionSquares
                               where
                               levelTable = M.toList level
                               moves = filter (\((_, _), obj) -> (length (show (head obj))) == 2) levelTable
                               positionSquares = map (\x -> fst x) moves

    isGoal (Level level) = length [x | ((_, _), [(Circle _ x)]) <- (M.toList level)] == 0

    {-
        Euristica folosita in problema reprezinta numarul de cercuri ramase fara un patrat.
    -}
    heuristic (Level level) = (length [x | ((_, _), [(Circle _ x)]) <- (M.toList level)])
