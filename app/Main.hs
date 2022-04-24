{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Arrow (ArrowChoice (left))
import Control.Exception (IOException, catch)
import Control.Monad (forM_, unless, when)
import Control.Monad.Except (ExceptT, liftEither, liftIO, runExceptT)
import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy (ByteString)
import qualified Data.List.NonEmpty as NonEmpty (NonEmpty ((:|)), zip)
import Network.HTTP (RequestMethod (GET), Response (rspBody, rspCode), mkRequest, simpleHTTP, urlEncode)
import Network.URI (URI, parseURI)
import System.Environment (getArgs)
import System.Exit (die)
import qualified Types as T (Definition (..), Heteronym (..), SearchResult (..))
import Util (bold, isBlank, maybeToEither, trim, underline)

main :: IO ()
main = do
    eitherResult <- runExceptT $ do
        word <- urlEncode <$> getSearchWord
        uri <- liftEither . maybeToEither "Invalid URI" . parseURI $ "http://www.moedict.tw/uni/" <> word
        body <- callDictionaryAPI uri
        liftEither $ eitherDecode body

    either (die . ("取得資料失敗：\n" <>)) printResult eitherResult

getSearchWord :: ExceptT String IO String
getSearchWord = do
    args <- liftIO getArgs

    liftEither $
        if null args || isBlank (head args)
            then Left "未提供查詢字詞"
            else Right . trim . head $ args

callDictionaryAPI :: URI -> ExceptT String IO ByteString
callDictionaryAPI uri = do
    eitherResponse <- liftIO $ sendRequest `catch` (\(e :: IOException) -> return . Left . show $ e)
    liftEither $ eitherResponse >>= (\is404 -> when is404 $ Left "查無字詞") . ((4, 0, 4) ==) . rspCode
    liftEither $ rspBody <$> eitherResponse
    where
        sendRequest = left show <$> simpleHTTP (mkRequest GET uri)

printResult :: T.SearchResult -> IO ()
printResult T.SearchResult{..} = do
    forM_ (NonEmpty.zip (0 NonEmpty.:| [1 ..]) heteronyms) $ \(i :: Int, heteronym) -> do
        when (i > 0) $ putStrLn ""
        printHeteronym heteronym
    where
        printHeteronym T.Heteronym{..} = do
            putStrLn $ bold title <> " " <> bopomofo
            forM_ definitions printDefinition
        printDefinition T.Definition{..} = do
            mapM_ (putStrLn . underline) partOfSpeech

            unless (isBlank def) $
                putStrLn $ "  " <> def

            maybe (return ()) (mapM_ (putStrLn . ("  " <>))) examples
            maybe (return ()) (mapM_ (putStrLn . ("  " <>))) quotes
            maybe (return ()) (mapM_ (putStrLn . ("  " <>))) links

            mapM_ (putStrLn . (("  " <> underline "近義" <> " ") <>)) synonyms
