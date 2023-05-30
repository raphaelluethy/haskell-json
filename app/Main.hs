{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson
    ( eitherDecode,
      encode,
      (.:),
      withObject,
      object,
      FromJSON(parseJSON),
      KeyValue((.=)),
      ToJSON(toJSON) )
import Data.Text (unpack)
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.ByteString.Lazy as B
import Control.Monad (when)
import System.Directory (doesFileExist)

filePath :: FilePath
filePath = "snips.json"

newtype SnippetMetaData = SnippetMetaData { fileTypes :: [String] } deriving (Show)

instance FromJSON SnippetMetaData where
  parseJSON = withObject "SnippetMetaData" $ \v -> do
    fileTypes <- v .: "fileTypes"
    return $ SnippetMetaData fileTypes

instance ToJSON SnippetMetaData where
  toJSON (SnippetMetaData fileTypes) = object ["fileTypes" .= fileTypes]

data Snippet = Snippet { name :: String, content :: [String], meta :: SnippetMetaData } deriving (Show)

instance FromJSON Snippet where
  parseJSON = withObject "Snippet" $ \v -> do
    name <- v .: "name"
    content <- v .: "content"
    meta <- v .: "meta"
    return $ Snippet name content meta

instance ToJSON Snippet where
  toJSON (Snippet name content meta) = object ["name" .= name, "content" .= content, "meta" .= meta]

newtype SnippetList = SnippetList { snippets :: [Snippet] } deriving (Show)

instance FromJSON SnippetList where
  parseJSON = withObject "SnippetList" $ \v -> do
    snippets <- v .: "snippets"
    return $ SnippetList snippets

instance ToJSON SnippetList where
  toJSON (SnippetList snippets) = object ["snippets" .= snippets]

main :: IO ()
main = do
  -- snips <- getAllSnippetsByFileType "java"
  -- putStrLn (show snips)
  -- writeSnippet
  -- deleteSnippetByName "test2"
  -- res <- checkIfSnippetExists "test3"
  -- print res


snippet :: Snippet
snippet = Snippet { name = "test3", content = ["test"], meta = SnippetMetaData { fileTypes = ["test", "py", "java"] } }

writeSnippet :: IO ()
writeSnippet = do
  -- Read existing snippets from file or create an empty list
  existingSnippets <- readExistingSnippets
  -- Add new snippet to the list
  let updatedSnippets = addNewSnippet existingSnippets snippet
  -- Write updated snippets to file
  B.writeFile filePath (encode updatedSnippets)

-- Read existing snippets from file or create an empty list
readExistingSnippets :: IO SnippetList
readExistingSnippets = do
  exists <- doesFileExist filePath
  if exists
    then do
      -- Read existing snippets from file
      bytes <- B.readFile filePath
      case eitherDecode bytes of
        Left err -> error $ "Failed to decode JSON: " ++ err
        Right snippetList -> return snippetList
    else return (SnippetList [])

addNewSnippet :: SnippetList -> Snippet -> SnippetList
addNewSnippet (SnippetList snippets) newSnippet =
  if any (\s -> name s == name newSnippet) snippets
    then SnippetList snippets
    else SnippetList (newSnippet : snippets)


getAllSnippetsByFileType :: String -> IO SnippetList
getAllSnippetsByFileType type' = do
  -- Read existing snippets from file or create an empty list
  existingSnippets <- readExistingSnippets
  -- Filter snippets by file type
  let filteredSnippets = filter (\s -> type' `elem` fileTypes (meta s)) (snippets existingSnippets)
  return (SnippetList filteredSnippets)


deleteSnippetByName :: String -> IO ()
deleteSnippetByName name' = do
  -- Read existing snippets from file or create an empty list
  existingSnippets <- readExistingSnippets
  -- Filter snippets by name
  let filteredSnippets = filter (\s -> name' /= name s) (snippets existingSnippets)
  -- Write updated snippets to file
  B.writeFile filePath (encode (SnippetList filteredSnippets))

checkIfSnippetExists :: String -> IO Bool
checkIfSnippetExists name' = do
  -- Read existing snippets from file or create an empty list
  existingSnippets <- readExistingSnippets
  -- Filter snippets by name
  let filteredSnippets = filter (\s -> name' == name s) (snippets existingSnippets)
  return (not (null filteredSnippets))