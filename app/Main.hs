{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson
import Data.ByteString.Lazy qualified as B
import Data.Text (Text)
import Data.Text.IO qualified as T
import System.Environment (getArgs)

data Snippet = Snippet
  { name :: Text,
    language :: Text,
    text :: Text
  }
  deriving (Show, Eq)

instance FromJSON Snippet where
  parseJSON = withObject "Snippet" $ \v ->
    Snippet
      <$> v
      .: "name"
      <*> v
      .: "language"
      <*> v
      .: "text"

instance ToJSON Snippet where
  toJSON (Snippet name' lang text') =
    object
      [ "name" .= name',
        "language" .= lang,
        "text" .= text'
      ]

newtype SnippetList = SnippetList { snippets :: [Snippet] }
  deriving (Show, Eq)

instance FromJSON SnippetList where
  parseJSON = withObject "SnippetList" $ \v ->
    SnippetList <$> v .: "snippets"

instance ToJSON SnippetList where
  toJSON (SnippetList snippets') =
    object ["snippets" .= snippets']

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["create"] -> createSnippet
    ["read"] -> readSnippet "mysnippets.json"
    _ -> putStrLn "Usage: haskell-json create|read"

createSnippet :: IO ()
createSnippet = do
  putStrLn "Enter name:"
  name' <- T.getLine
  putStrLn "Enter language:"
  lang' <- T.getLine
  putStrLn "Enter text:"
  text' <- T.getLine
  let snippet = Snippet name' lang' text'
  contents <- B.readFile "mysnippets.json"
  let maybeSnippetList = decode contents :: Maybe SnippetList
  case maybeSnippetList of
    Nothing -> do
      let snippetList = SnippetList [snippet]
      B.writeFile "mysnippets.json" (encode snippetList)
      putStrLn "Snippet created!"
    Just (SnippetList snippetList) -> do
      let snippetList' = SnippetList (snippet : snippetList)
      B.writeFile "mysnippets.json" (encode snippetList')
      putStrLn "Snippet created!"

readSnippet :: FilePath -> IO ()
readSnippet file = do
  contents <- B.readFile file
  let maybeSnippetList = decode contents :: Maybe SnippetList
  case maybeSnippetList of
    Nothing -> putStrLn "Error parsing JSON"
    Just (SnippetList snippetList) -> do
      putStrLn "Enter name:"
      name' <- T.getLine
      let matchingSnippets = filter (\s -> name s == name') snippetList
      case matchingSnippets of
        [] -> putStrLn "No matching snippet found"
        (s:_) -> do
          putStrLn $ "Name: " ++ show (name s)
          putStrLn $ "Language: " ++ show (language s)
          putStrLn $ "Text: " ++ show (text s)
