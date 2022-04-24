{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types (SearchResult (..), Heteronym (..), Definition (..)) where

import Data.Aeson (FromJSON (parseJSON), withObject, (.:), (.:?))
import Data.List.NonEmpty (NonEmpty)
import GHC.Generics (Generic)

data SearchResult = SearchResult {
    title :: String,
    heteronyms :: NonEmpty Heteronym
} deriving (Show, Generic)

data Heteronym = Heteronym {
    bopomofo :: String,
    definitions :: NonEmpty Definition
} deriving (Show, Generic)

data Definition = Definition {
    partOfSpeech :: Maybe String,
    def :: String,
    examples :: Maybe (NonEmpty String),
    quotes :: Maybe (NonEmpty String),
    synonyms :: Maybe String,
    links :: Maybe (NonEmpty String)
} deriving (Show)

instance FromJSON SearchResult

instance FromJSON Heteronym

instance FromJSON Definition where
    parseJSON = withObject "Definition" $ \v -> do
        p <- v .:? "type"
        d <- v .: "def"
        e <- v.:? "example"
        q <- v.:? "quote"
        s <- v .:? "synonyms"
        l <- v .:? "link"

        return Definition {
            partOfSpeech = p,
            def = d,
            examples = e,
            quotes = q,
            synonyms = s,
            links = l
        }
