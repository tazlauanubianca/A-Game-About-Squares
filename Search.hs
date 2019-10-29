{-# OPTIONS_GHC -Wall #-}

module Search where

import ProblemState
import Data.Maybe
import Data.List
import Data.Ord
import qualified Data.Set as S

{-
    Tipul unei nod utilizat în procesul de căutare.
-}
data Node s a = GNode { nState :: s
                      , nAction :: Maybe a
                      , nParent :: Maybe (Node s a)
                      , nHeight :: Int
                      } deriving (Eq, Show)

{-
    Întoarce starea stocată într-un nod.
-}
nodeState :: Node s a -> s
nodeState (GNode state _ _ _) = state

{-
    Functie care sorteaza o lista de noduri folosind euristica din ProblemState.
-}
sortNodes :: (ProblemState s a, Ord s)
          => [Node s a]
          -> [Node s a]
sortNodes gameState = sortedNodes
                      where
                      sortedResult = sortBy (comparing ProblemState.heuristic) (map (\(GNode state _ _ _ ) -> state) gameState) 
                      sortedNodes = map (\x -> head [node | node <- gameState, nodeState node == x]) sortedResult
                    
{-
    Functie recursiva pentru DFS pana la o inaltime data.
-}
recursiveDFS :: (ProblemState s a, Ord s) 
             => S.Set s 
             -> [Node s a]
             -> [Node s a]
             -> Int
             -> [Node s a]
recursiveDFS _ done [] _ = reverse done
recursiveDFS check done ((GNode state action parentNode height) : tail) maxHeight
    | S.member state check = recursiveDFS check done tail maxHeight
    | height == maxHeight = recursiveDFS (S.insert state check) ((GNode state action parentNode height) : done) tail maxHeight
    | otherwise = recursiveDFS (S.insert state check) ((GNode state action parentNode height) : done) (newNodes ++ tail) maxHeight
    where
    newNodes = map (\x -> (GNode (snd x) (Just (fst x)) (Just (GNode state action parentNode height)) (height + 1))) (ProblemState.successors state)

{-
    Întoarce lista nodurilor rezultate prin parcurgerea limitată în adâncime
    a spațiului stărilor, pornind de la starea dată ca parametru.
-}
limitedDfs :: (ProblemState s a, Ord s)
           => s           -- Starea inițială
           -> Bool        -- Pentru BONUS, `True` dacă utilizăm euristica
           -> Int         -- Adâncimea maximă de explorare
           -> [Node s a]  -- Lista de noduri
limitedDfs state condition depth
    | condition == False =  myResult
    | otherwise = sortNodes myResult 
    where myResult = recursiveDFS S.empty [] [(GNode state Nothing Nothing 0)] depth


{-
    Explorează în adâncime spațiul stărilor, utilizând adâncire iterativă,
    pentru determinarea primei stări finale întâlnite.

    Întoarce o perche între nodul cu prima stare finală întâlnită și numărul
    de stări nefinale vizitate până în acel moment.
-}
deepeningHelper :: (ProblemState s a, Ord s)
    => s
    -> Int
    -> Int
    -> Bool
    -> (Node s a, Int)
deepeningHelper state currentDepth fails condition
    | length succNodes /= 0 = ((head succNodes), ((fails + (length failNodes))))
    | otherwise = deepeningHelper state (currentDepth + 1) (fails + (length nodes)) condition
    where 
    nodes = limitedDfs state condition currentDepth
    succNodes = filter (\(GNode state _ _ _) -> isGoal state) nodes
    failNodes = takeWhile (\(GNode state _ _ _) -> not (isGoal state)) nodes

iterativeDeepening :: (ProblemState s a, Ord s)
    => s                
    -> Bool             
    -> (Node s a, Int)
iterativeDeepening state condition = deepeningHelper state 0 0 condition


{-
    Pornind de la un nod, reface calea către nodul inițial, urmând legăturile
    către părinți.

    Întoarce o listă de perechi (acțiune, stare), care se încheie în starea
    finală, dar care EXCLUDE starea inițială.
-}
extractPathHelper :: [(a, s)] -> Node s a -> [(a, s)]
extractPathHelper path (GNode state action parent height)
    | (GNode _ Nothing Nothing 0) <- (fromJust parent) = ((fromJust action), state) : path
    | otherwise = extractPathHelper (((fromJust action), state) : path) (fromJust parent)

extractPath :: Node s a -> [(a, s)]
extractPath (GNode state action parent height) = extractPathHelper [] (GNode state action parent height)