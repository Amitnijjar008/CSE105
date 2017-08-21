{- |
Module          : DFA
Description     : Definition of Deterministic Finite Automata
Author          : Daniele Micciancio
Stability       : experimental
-}

{-# LANGUAGE ExistentialQuantification #-}

module DFA where

import Text.XML.Light 
import Data.List (sort, find, nub, findIndex)
import Data.Maybe (isJust, listToMaybe)
import Control.Monad (when)

-- | Deterministic Finite Automaton (DFA) with states of type st is a 5-tuple consisting of 
-- a list of states, a list of input characters, a transition function, a start state and a 
-- predicate mapping accepting states to True 

data DFA st = DFA ([st], [Char], st->Char->st, st, st->Bool)

-- | Run a DFA on an input string
evalDFA :: DFA st -> String -> Bool
evalDFA (DFA (states,alphabet,delta,startState,isFinal)) =
  isFinal . foldl delta startState

---- JFLAP Import/Export Functions

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

dfaToJFLAP :: (Eq st) => DFA st -> Maybe String
dfaToJFLAP dfa = do 
  DFA (qs, sigma, delta, start, isFinal) <- intDFA (SomeDFA dfa)
  let states = [ makeState q (q==start) (isFinal q) | q <- qs]
  let transitions = [makeTrans p x (delta p x) | p <- qs, x <- sigma]
  let xml = unode "structure" [ unode "type" "fa",
                                unode "automaton" (states ++ transitions)]
  return (showTopElement xml)

data State = State { stateIndex :: Int, initial :: Bool, final :: Bool } 
data Trans = Trans { fromState  :: Int, toState :: Int,  input :: Char }

hasChild name = isJust . findChild (QName name Nothing Nothing)
getChild name = fmap strContent . findChild (QName name Nothing Nothing)

readState :: Element -> Maybe State
readState e = do
  i <- findAttr (QName "id" Nothing Nothing) e
  let initial = hasChild "initial" e
  let final   = hasChild "final" e
  return $ State (read i) initial final

readTrans :: Element -> Maybe Trans
readTrans e = do
  q1 <- getChild "from" e
  q2 <- getChild "to" e
  str <- getChild "read" e
  c <- listToMaybe str
  when (length str > 1) Nothing
  return $ Trans (read q1) (read q2) c

-- | Check that a function f is defined on all inputs
maybeFunction :: (a -> Maybe b) -> [a] -> Maybe (a -> b)
maybeFunction f dom = 
  case sequence (fmap f dom) of
  Nothing   -> Nothing
  otherwise -> Just (\a -> let (Just b) = f a in b)

maybeFunction2 :: (a -> b -> Maybe c) -> [a] -> [b] -> Maybe (a -> b -> c)
maybeFunction2 f domA domB =
  fmap curry (maybeFunction (uncurry f) [(a,b) | a <- domA, b <- domB])

findUnique :: (a -> Bool) -> [a] -> Maybe a
findUnique p xs =
  case filter p xs of
  [x] -> Just x
  otherwise -> Nothing

{- JFLAP State and Transition XML generation -}

makeState :: Int -> Bool -> Bool -> Element
makeState q isS isF = 
  add_attr (Attr (unqual "id") (show q))
    (unode "state" ((if isS then [unode "initial" ()] else [])
                    ++ (if isF then [unode "final" ()] else [])))

makeTrans :: Int -> Char -> Int -> Element
makeTrans q x p =
  unode "transition" 
  [ unode "from" (show q)
  , unode "to"   (show p)
  , unode "read" [x]]

-- |Convert a (DFA st) to an equivalent (NFA Int)     

data SomeDFA = forall st. Eq st => SomeDFA (DFA st)
               
intDFA :: SomeDFA -> Maybe (DFA Int)
intDFA (SomeDFA (DFA (qs, alpha, delta, s, isF))) = 
  let qs1 = [0 .. length qs - 1]
      stToIntM q = findIndex (==q) qs
      delta1M n x = stToIntM (delta (qs!!n) x)
  in do s1 <- stToIntM s
        delta1 <- maybeFunction2 delta1M qs1 alpha
        return (DFA (qs1, alpha, delta1, s1, isF . (qs!!)))

writeDFA :: (Eq st) => DFA st -> String -> IO ()
writeDFA dfa file =
  case dfaToJFLAP dfa of
    Nothing -> error "Invalid DFA"
    Just jff -> writeFile file jff
    
readDFA :: String -> IO (DFA Int)
readDFA file = do
  jff <- readFile file
  case jflapToDFA jff of
    Nothing -> error "Input is not a valid JFLAP DFA"
    Just dfa -> return dfa

-- Computes the list of reachable states, together with a string that leads to them
reachableStates :: (Eq st) => DFA st -> [(String,st)]
reachableStates (DFA (states,alphabet,delta, start,isFinal)) =
  worker [] [("",start)] where
  worker visited [] = [(reverse w,q) | (w,q) <- visited]
  worker visited (q : qs) =
    let new = [(a:fst q, q1) | a <- alphabet, let q1 = delta (snd q) a, isNew q1]
        isNew q1 = not (q1 `elem` (map snd (q : qs ++ visited)))
    in worker (q:visited) (new ++ qs)

-- Find a string that is accepted by a DFA, if it exists       
findInput :: (Eq st) => DFA st -> Maybe String
findInput dfa = 
  let qs = reverse (reachableStates dfa)
      DFA (_,_,_,_,isFinal) = dfa
  in case find (isFinal . snd) qs of
       Nothing -> Nothing
       Just (str,q) -> Just str