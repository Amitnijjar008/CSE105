module Main where

import Text.XML.Light 
import Data.List (sort, find, nub)
import Data.Maybe (isJust, listToMaybe)
import Control.Monad (when)

import HW1

findUnique :: (a -> Bool) -> [a] -> Maybe a
findUnique p xs =
  case filter p xs of
  [x] -> Just x
  otherwise -> Nothing

data State =
  State { stateIndex :: Int, initial :: Bool, final :: Bool } 

data Trans =
  Trans { fromState :: Int, toState :: Int, input :: Char }

readState :: Element -> Maybe State
readState e = do
  i <- findAttr (QName "id" Nothing Nothing) e
  let initial = hasChild "initial" e
  let final   = hasChild "final" e
  return $ State (read i) initial final

hasChild name = isJust . findChild (QName name Nothing Nothing)

readTrans :: Element -> Maybe Trans
readTrans e = do
  q1 <- child "from" e
  q2 <- child "to" e
  str <- child "read" e
  c <- listToMaybe str
  when (length str > 1) Nothing
  return $ Trans (read q1) (read q2) c

child name = fmap strContent . findChild (QName name Nothing Nothing)

-- |Parse DFA from JFLAP xml string. Return Nothing is parsing fails.
jflapToDFA :: String -> Maybe (DFA Int)
jflapToDFA dfa = do
  xmlDoc <- parseXMLDoc dfa
  states <- mapM readState (findElements (QName "state"      Nothing Nothing) xmlDoc)
  trans  <- mapM readTrans (findElements (QName "transition" Nothing Nothing) xmlDoc)
  s <- fmap stateIndex (findUnique initial states)
  let qs = sort (map stateIndex states)
  let sigma = sort (nub (map input trans))
  let deltaM q x = do
        tr <- findUnique (\t -> (fromState t == q) && (input t == x)) trans 
        let q1 = toState tr
        if (elem q1 qs) then Just q1 else Nothing
  delta <- maybeFunction2 deltaM qs sigma
  let isFinalM q = fmap final (findUnique ((==q) . stateIndex) states)
  isFinal <- maybeFunction isFinalM qs
  return (DFA (qs, sigma, delta, s, isFinal))

-- | Check that a function f is defined on all inputs
maybeFunction :: (a -> Maybe b) -> [a] -> Maybe (a -> b)
maybeFunction f dom = 
  case sequence (fmap f dom) of
  Nothing   -> Nothing
  otherwise -> Just (\a -> let (Just b) = f a in b)

maybeFunction2 :: (a -> b -> Maybe c) -> [a] -> [b] -> Maybe (a -> b -> c)
maybeFunction2 f domA domB =
  fmap curry (maybeFunction (uncurry f) [(a,b) | a <- domA, b <- domB])

diffDFA :: DFA st1 -> DFA st2 -> DFA (st1,st2)
diffDFA (DFA (qs1,alpha1,delta1,start1,isFinal1)) (DFA (qs2,alpha2,delta2,start2,isFinal2))
    | alpha1 == alpha2 -- Use only on automata with the same input alphabet
  = DFA ([(q1,q2) | q1 <- qs1, q2 <- qs2],
         alpha1, delta, (start1,start2), isFinal) where
      delta (q1,q2) a = (delta1 q1 a, delta2 q2 a) 
      isFinal (q1,q2) = (isFinal1 q1 /= isFinal2 q2)

withError :: Maybe b -> a -> Either a b
withError Nothing  err = Left err
withError (Just x) err = Right x

ifNot :: Bool -> a -> Either a ()
ifNot True err = Right ()
ifNot False err = Left err

test1 :: String -> Either String ()
test1 jff = do 
  let DFA (qs1,alpha1,delta1,s1,isF1) = machine1
  dfa <- (jflapToDFA jff) `withError` "machine1.jff is not a valid DFA."
  let DFA (qs,alpha,delta,s,isF) = dfa
  ifNot (length qs == length qs1) "machine1.jff and machine1 have a different number of states"
  ifNot (sort alpha == sort alpha1) "machine1.jff and machine1 have a different alphabets"
  case findInput (diffDFA dfa machine1) of
    Nothing  -> Right ()
    Just str -> Left ("machine1.jff gives the wrong answer on input \"" ++ str ++ "\"")
  
reachableStates :: (Eq st) => DFA st -> [(String,st)]
reachableStates (DFA (states,alphabet,delta, start,isFinal)) =
  worker [] [("",start)] where
  worker visited [] = visited
  worker visited (q : qs) =
    let new = [(a:fst q, q1) | a <- alphabet, let q1 = delta (snd q) a, isNew q1]
        isNew q1 = not (q1 `elem` (map snd (q : qs ++ visited)))
    in worker (q:visited) (new ++ qs)
                                                            
findInput :: (Eq st) => DFA st -> Maybe String
findInput dfa = 
  let qs = reverse (reachableStates dfa)
      DFA (_,_,_,_,isFinal) = dfa
  in case find (isFinal . snd) qs of
       Nothing -> Nothing
       Just (str,q) -> Just (reverse str)

errorStr = "\x1b[31mError:\x1b[0m "
correctStr = "\x1b[32mCorrect:\x1b[0m "
  
test :: IO ()
test = do
  jff <- readFile "machine1.jff"
  case test1 jff of
    Left err -> putStrLn (errorStr ++ err)
    Right () -> putStrLn (correctStr ++ "Your machine passed all tests. Good Job!")
                 
main = test
