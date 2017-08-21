module HW1 where

{- A Deterministic Finite Automaton (DFA) with states of type st is a 5-tuple consisting of 
   a list of states, a list of input characters, a transition function, a start state and a 
   predicate mapping accepting states to True -}

data DFA st = DFA ([st], [Char], st->Char->st, st, st->Bool)


{- You can use the following function to run a DFA on an input string -}

evalDFA :: DFA st -> String -> Bool
evalDFA (DFA (states,alphabet,delta,startState,isFinal)) =
  isFinal . foldl delta startState

{- Here is a DFA example, with set of states and transition function 
   represented by haskell programs. Can you tell what's the language of this machine? -}
  
machine1 :: DFA Int
machine1 = DFA (states, alphabet, delta, startState, isFinal) where
  states = [0..4]
  alphabet = ['0','1']
  startState = 0
  isFinal q = not (q == 0)
  delta q '0' = (2*q) `mod` 5
  delta q '1' = (2*q + 1) `mod` 5                  

{- You can even write programs that generate DFAs as output. The following function, 
   on input a positive integer n, outputs a DFA (buildMyDFA n) with n states.
   Notice, (buildDFA 5) gives the same DFA as machine1.
   Can you tell what's the language of (buildDFA 32)? -}

buildMachine :: Int -> DFA Int
buildMachine n = DFA (states, alphabet, delta, startState, isFinal) where
  states = [0..n-1]
  alphabet = ['0','1']
  startState = 0
  isFinal q = not (q == 0)
  delta q '0' = (2*q) `mod` n
  delta q '1' = (2*q + 1) `mod` n                  