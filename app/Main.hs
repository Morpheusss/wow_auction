{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Aeson
import Data.Aeson.Types
import Network.HTTP.Simple
import Network.HTTP.Client.TLS
import System.Environment
import qualified Data.Vector as V

dataUrl :: IO (Maybe String)
dataUrl = do
  apiKey <- getEnv "BLZ_API_KEY"
  res <- fetchJSON ("https://us.api.battle.net/wow/auction/data/illidan?locale=en_US&apikey=" ++ apiKey)
  return $ parseMaybe parseDataUrl res

fetchJSON :: String -> IO Value
fetchJSON url = do
  req <- parseRequest $ "GET " ++ url
  fmap getResponseBody (httpJSON req)

parseDataUrl :: Value -> Parser String
parseDataUrl value = do
  files <- withObject "Look up files" (.: "files") value
  firstObj <- withArray "Get first file" (pure . (V.! 0)) files
  withObject "Lookup data URL" (.: "url") firstObj

main :: IO ()
main = do
  manager <- newTlsManagerWith tlsManagerSettings
  dataUrl <- dataUrl
  putStrLn $ case dataUrl of
    Nothing -> "Parsing failed!"
    Just url -> "URL is: " ++ url
