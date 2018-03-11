{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Aeson
import Data.Aeson.Types
import Network.HTTP.Simple
import Network.HTTP.Client.TLS
import qualified Data.Vector as V

metaDataUrlWith :: String -> String
metaDataUrlWith apiKey = "https://us.api.battle.net/wow/auction/data/illidan?locale=en_US&apikey=" ++
          apiKey

fetchJSON :: String -> IO Value
fetchJSON url = do
  req <- parseRequest $ "GET " ++ url
  res <- httpJSON req
  return $ getResponseBody res

parseDataUrl :: Value -> Parser String
parseDataUrl value = do
  files <- withObject "Look up files" (.: "files") value
  firstObj <- withArray "Get first file" (pure . (V.! 0)) files
  withObject "Lookup data URL" (.: "url") firstObj

getDataUrl :: String -> IO (Maybe String)
getDataUrl apiKey = do
  res <- fetchJSON $ metaDataUrlWith $ apiKey
  return $ parseMaybe parseDataUrl res


main :: IO ()
main = do
  manager <- newTlsManagerWith tlsManagerSettings
  dataUrl <- getDataUrl "{use your own :)}"
  putStrLn $ case dataUrl of
    Nothing -> "Parsing failed!"
    Just url -> "URL is: " ++ url
