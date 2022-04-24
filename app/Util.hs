module Util (maybeToEither, isBlank, trim, bold, underline) where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither a = maybe (Left a) Right

isBlank :: String -> Bool
isBlank = all isSpace

trim :: String -> String
trim str = dropWhileEnd isSpace (dropWhile isSpace str)

bold :: String -> String
bold str = "\ESC[1m" <> str <> "\ESC[0m"

underline :: String -> String
underline str = "\ESC[4m" <> str <> "\ESC[0m"
