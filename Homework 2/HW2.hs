module Main where

import DFA
import System.Environment (getArgs)

problem2transformation :: DFA st1 -> DFA st2 -> DFA (st1,st2)
problem2transformation
  (DFA (qs1,alpha1,delta1,start1,isFinal1)) -- Assume automata have the same input alphabet
  (DFA (qs2,alpha2,delta2,start2,isFinal2))  | alpha1 == alpha2 
  = DFA ([(q1,q2) | q1 <- qs1, q2 <- qs2],
         alpha1, delta, (start1,start2), isFinal) where
      delta (q1,q2) a = (delta1 q1 a, delta2 q2 a) 
      isFinal (q1,q2) = (isFinal1 q1) && not (isFinal2 q2)

test jff1 jff2 = do
  dfa1 <- jflapToDFA jff1
  dfa2 <- jflapToDFA jff2
  dfaToJFLAP (problem2transformation dfa1 dfa2)

main = do
  [file1,file2] <- getArgs
  jff1 <- readFile file1
  jff2 <- readFile file2
  case test jff1 jff2 of
    Nothing -> error "Input is not two valid DFAs"
    Just jff3 -> putStrLn jff3
    