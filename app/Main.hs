{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Aeson
import Data.Aeson.Types
import Network.HTTP.Simple
import Network.HTTP.Client.TLS
import System.Environment
import TimeStamped
import qualified Data.Vector as V

dataUrl :: IO (Maybe (TimeStamped String))
dataUrl = do
  apiKey <- getEnv "BLZ_API_KEY"
  res <- fetchJSON ("https://us.api.battle.net/wow/auction/data/illidan?locale=en_US&apikey=" ++ apiKey)
  return $ parseMaybe parseMetaData res

fetchJSON :: String -> IO Value
fetchJSON url = do
  req <- parseRequest $ "GET " ++ url
  fmap getResponseBody (httpJSON req)

parseMetaData :: Value -> Parser (TimeStamped String)
parseMetaData value = do
  files <- withObject "Look up files" (.: "files") value
  firstObj <- withArray "Get first file" (pure . (V.! 0)) files
  lastModified <- withObject "Lookup last mofidifed time" (.: "lastModified") firstObj
  url <- withObject "Lookup data URL" (.: "url") firstObj
  pure (Stamp url lastModified)

main :: IO ()
main = do
  manager <- newTlsManagerWith tlsManagerSettings
  dataUrl <- dataUrl
  putStrLn $ case dataUrl of
    Nothing -> "Parsing failed!"
    Just (Stamp url lastModified) -> "URL (lastModified=" ++ (show lastModified) ++ ") is: " ++ url
