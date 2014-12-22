{-# OPTIONS_GHC -Wall #-}
module LogAnlysis where
import Data.List
import Log

-- ex1
parseMessage :: String -> LogMessage
parseMessage ('E':xs) =
  let err = Error((read(head(words xs))))
      tStamp = read(head(tail(words xs))) :: TimeStamp
      message = intercalate " " (tail(tail(words xs)))
  in LogMessage err tStamp message

parseMessage ('I':xs) =
  let tStamp = read(head(words xs)) :: TimeStamp
      message = intercalate " " (tail(words xs))
  in LogMessage Info tStamp message

parseMessage ('W':xs) =
  let tStamp = read(head(words xs)) :: TimeStamp
      message = intercalate " " (tail(words xs))
  in LogMessage Warning tStamp message

parseMessage xs = Unknown xs

parse :: String -> [LogMessage]
parse text = [parseMessage line | line <- lines text]
