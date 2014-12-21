{-# OPTIONS_GHC -Wall #-}
module LogAnlysis where

import Log

parseMessage :: String -> LogMessage
parseMessage ('E':xs) = LogMessage Error
parseMessage ('I':xs) = Unknown xs
parseMessage ('W':xs) = Unknown xs
parseMessage _ = Unknown _
