{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- ex1
parseMessage :: String -> LogMessage
parseMessage ('E':xs) =
  let err = Error((read(head(words xs))))
      tStamp = read (head(tail(words xs))) :: TimeStamp
      message = unwords (tail(tail(words xs)))
  in LogMessage err tStamp message

parseMessage ('I':xs) =
  let tStamp = read(head(words xs)) :: TimeStamp
      message = unwords (tail(words xs))
  in LogMessage Info tStamp message

parseMessage ('W':xs) =
  let tStamp = read(head(words xs)) :: TimeStamp
      message = unwords (tail(words xs))
  in LogMessage Warning tStamp message

parseMessage xs = Unknown xs

parse :: String -> [LogMessage]
parse text = [parseMessage line | line <- lines text]

-- ex2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert message Leaf = Node Leaf message Leaf
insert message (Node l m r) =
  if getTStamp(message) < getTStamp(m)
  then Node (LogAnalysis.insert message l) m r
  else Node l m (LogAnalysis.insert message r)

getTStamp :: LogMessage -> Int
getTStamp (Unknown _) = 0
getTStamp (LogMessage _ t _) = t

-- ex3
build :: [LogMessage] -> MessageTree
build [] = Leaf
build (x:xs) = LogAnalysis.insert x (build xs)
