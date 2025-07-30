{-
-- EPITECH PROJECT, 2025
-- myPandoc
-- File description:
-- Json.hs
-}

module Input.Json (parseJsonDocument) where

import Document (Header(..), Document(..), Body(..), Block(..), Inline(..))
import Data.Char (isSpace)

class JsonParsable a where
  parseJson :: String -> Either String a

newtype JsonString = JsonString { getJsonString :: String }

instance JsonParsable JsonString where
  parseJson inner = Right (JsonString (getStringInner inner))

parseJsonDocument :: String -> Either String Document
parseJsonDocument str = case parseJson $ trim str of
  Left err -> Left err
  Right docJson -> Right docJson

getNextObjectPair :: String -> Either String ((String, String), String)
getNextObjectPair s = do
    restAfterKey <- extractKey s
    (key, restAfterColon) <- extractKeyValue restAfterKey
    (value, rest) <- extractJsonValue restAfterColon
    Right ((key, value), rest)

extractKey :: String -> Either String String
extractKey s = case skipSpacesAndCommas s of
    ('"':rest) -> Right rest
    _ -> Left "Expected '\"' at beginning of key."

extractKeyValue :: String -> Either String (String, String)
extractKeyValue s = case span (/= '"') s of
    (key, '"':afterKey) -> case skipSpaces afterKey of
        (':':afterColon) -> Right (key, afterColon)
        _ -> Left "Expected ':' after key."
    _ -> Left "Expected closing '\"' after key."

skipSpaces :: String -> String
skipSpaces = dropWhile isSpace

skipSpacesAndCommas :: String -> String
skipSpacesAndCommas = dropWhile (\c -> isSpace c || c == ',')

extractJsonValue :: String -> Either String (String, String)
extractJsonValue s =
    case skipSpaces s of
        ('"':rest) -> let (str, remaining) = spanUntilStringEnd rest
                      in (Right (('"' : str), remaining))
        ('{':rest) -> wrapExtract '{' '}' rest
        ('[':rest) -> wrapExtract '[' ']' rest
        input -> let (val, rest') = span (\c -> c /= ',' && c /= '}') input
                 in (Right ((takeWhile (not . isSpace) val),
                   dropWhile isSpace rest'))

getObjectInner :: String -> String
getObjectInner json = trim . init . tail . trim $ json

getStringInner :: String -> String
getStringInner json = init . tail . trim $ json

spanUntilStringEnd :: String -> (String, String)
spanUntilStringEnd = go ""
  where
    go acc [] = (acc, [])
    go acc ('\\':x:xs) = go (acc ++ ['\\', x]) xs
    go acc ('"':xs)    = (acc ++ ['"'], xs)
    go acc (x:xs)      = go (acc ++ [x]) xs

getNextListValue :: String -> Either String (String, String)
getNextListValue s =
    case skipSpacesAndCommas s of
        ('"':rest) ->
            let (str, remaining) = spanUntilStringEnd rest
            in Right ('"' : str, remaining)
        ('[':rest) -> wrapExtract '[' ']' rest
        ('{':rest) -> wrapExtract '{' '}' rest
        other -> let (val, rest') = span (\c -> c /= ',' && c /= ']') other
                 in if null val then Left "Expected a list value."
                      else Right (trim val, skipSpaces rest')

wrapExtract :: Char -> Char -> String -> Either String (String, String)
wrapExtract open close = go 1 [open]
  where
    go 0 acc rest = Right (reverse acc, rest)
    go _ _ [] = Left $ "Unmatched '" ++ [open] ++ "'"
    go n acc (x:xs)
      | x == open = go (n + 1) (x:acc) xs
      | x == close = go (n - 1) (x:acc) xs
      | otherwise = go n (x:acc) xs

parseJsonDocument' :: Document -> String -> Either String Document
parseJsonDocument' input "" = Right input
parseJsonDocument' input inner = 
  getNextObjectPair inner >>= \((key, valueStr), rest) ->
    parseJsonDocument'handleField input key valueStr rest

parseJsonDocument'handleField :: Document -> String -> String -> String
  -> Either String Document
parseJsonDocument'handleField input "header" valueStr rest = do
  value <- parseJson valueStr
  updated <- parseJsonDocument' input rest
  Right updated { header = value }
parseJsonDocument'handleField input "body" valueStr rest = do
  value <- parseJson valueStr
  updated <- parseJsonDocument' input rest
  Right updated { body = value }
parseJsonDocument'handleField _ key _ _ = Left $ "Unknown key: " ++ key

instance JsonParsable Document where
  parseJson "" = Left "Not a JSON Object"
  parseJson inner = parseJsonDocument' (Document{header = (Header Nothing Nothing Nothing), body = (Body [])}) (getObjectInner inner)

parseJsonHeader' :: Header -> String -> Either String Header
parseJsonHeader' input "" = Right input
parseJsonHeader' input inner = 
  getNextObjectPair inner >>= \((key, valueStr), rest) ->
    parseJsonHeader'handleField input key valueStr rest

parseJsonHeader'handleField :: Header -> String -> String -> String 
  -> Either String Header
parseJsonHeader'handleField input "title" valueStr rest = do
  value <- parseJson valueStr
  updated <- parseJsonHeader' input rest
  Right updated { title = Just (getJsonString value) }
parseJsonHeader'handleField input "author" valueStr rest = do
  value <- parseJson valueStr
  updated <- parseJsonHeader' input rest
  Right updated { author = Just (getJsonString value) }
parseJsonHeader'handleField input "date" valueStr rest = do
  value <- parseJson valueStr
  updated <- parseJsonHeader' input rest
  Right updated { date = Just (getJsonString value) }
parseJsonHeader'handleField _ key _ _ = Left $ "Unknown key: " ++ key

instance JsonParsable Header where
  parseJson inner = parseJsonHeader' (Header Nothing Nothing Nothing) (getObjectInner inner)

parseJsonBody' :: Body -> String -> Either String Body
parseJsonBody' input "" = Right input
parseJsonBody' input inner =
  getNextListValue inner >>= \(valueStr, rest) ->
    handleItem valueStr rest
  where
    handleItem listItem rest = do
      value <- parseJson listItem
      updated <- parseJsonBody' input rest
      Right updated{blocks = (value:(blocks updated))}

instance JsonParsable Body where
  parseJson inner = parseJsonBody' (Body []) (getObjectInner inner)

parseJsonBlockParagraph' :: Block -> String -> Either String Block
parseJsonBlockParagraph' input "" = Right input
parseJsonBlockParagraph' input inner = 
    getNextListValue inner >>= \(valueStr, rest) -> do
      value <- parseJson valueStr
      updated <- case parseJsonBlockParagraph' input rest of
        Left err -> Left err
        Right (Paragraph p) -> Right p
        Right _ -> Left "FATAL: returned not a Paragraph"
      Right $ Paragraph (value:updated)

parseJsonBlockSection' :: Block -> String -> Either String Block
parseJsonBlockSection' input "" = Right input
parseJsonBlockSection' input inner =
  getNextObjectPair inner >>= \((key, valueStr), rest) ->
    parseJsonBlockSection'handleField input key valueStr rest

parseJsonBlockSection'handleField :: Block -> String -> String -> String 
  -> Either String Block
parseJsonBlockSection'handleField input "title" valueStr rest = do
  value <- parseJson valueStr
  updated <- case parseJsonBlockSection' input rest of
    Left err -> Left err
    Right (Section _ p) -> Right p
    Right _ -> Left "FATAL: parseJsonBlockSection' returned not a Section"
  case getJsonString value of 
    "" -> Right (Section (Nothing) updated)
    a -> Right (Section (Just a) updated)
parseJsonBlockSection'handleField input "content" valueStr rest = do
  value <- parseJson valueStr
  updated <- case parseJsonBlockSection' input rest of
    Left err -> Left err
    Right (Section p _) -> Right p
    Right _ -> Left "FATAL: parseJsonBlockSection' returned not a Section"
  Right (Section updated (blocks value))
parseJsonBlockSection'handleField _ key _ _ = Left $ "Unknown key: " ++ key

parseJsonBlockCodeBlock' :: Block -> String -> Either String Block
parseJsonBlockCodeBlock' input "" = Right input
parseJsonBlockCodeBlock' input inner =
    getNextListValue inner >>= \(valueStr, rest) -> do
      value <- parseJson valueStr
      (l, updated) <- case parseJsonBlockCodeBlock' input rest of
        Left err -> Left err
        Right (CodeBlock l p) -> Right (l, p)
        Right _ -> Left "FATAL: returned not a CodeBlock"
      Right $ CodeBlock l (value:updated)

parseJsonBlockList' :: Block -> String -> Either String Block
parseJsonBlockList' input "" = Right input
parseJsonBlockList' input inner =
    getNextListValue inner >>= \(valueStr, rest) -> do
      value <- parseJson valueStr
      updated <- case parseJsonBlockList' input rest of
        Left err -> Left err
        Right (List p) -> Right p
        Right _ -> Left "FATAL: parseJsonBlockList' returned not a List"
      Right $ List (value:updated)

parseJsonBlockObject' :: String -> Either String Block
parseJsonBlockObject' inner = 
  getNextObjectPair inner >>= \((key, valueStr), _) ->
    parseJsonBlockObject'handleField key valueStr

parseJsonBlockObject'handleField :: String -> String -> Either String Block
parseJsonBlockObject'handleField "section" valueStr =
  parseJsonBlockSection' (Section Nothing []) $ getObjectInner valueStr
parseJsonBlockObject'handleField "codeblock" valueStr = 
  parseJsonBlockCodeBlock' (CodeBlock Nothing []) $ getObjectInner valueStr
parseJsonBlockObject'handleField "list" valueStr =
      parseJsonBlockList' (List []) $ getObjectInner valueStr
parseJsonBlockObject'handleField key _ = Left $ "Unknown key: " ++ key

instance JsonParsable Block where
  parseJson inner@('[':_) = parseJsonBlockParagraph' (Paragraph []) (getObjectInner inner)
  parseJson inner@('{':_) = parseJsonBlockObject' (getObjectInner inner)
  parseJson _ = Left "Block::parseJson: Error"

parseJsonInlineLink' :: Inline -> String -> Either String Inline
parseJsonInlineLink' input "" = Right input
parseJsonInlineLink' input inner =
  getNextObjectPair inner >>= \((key, valueStr), rest) ->
    parseJsonInlineLink'handleField input key valueStr rest

parseJsonInlineLink'handleField :: Inline -> String -> String -> String 
  -> Either String Inline
parseJsonInlineLink'handleField input "url" valueStr rest = do
  value <- parseJson valueStr
  updated <- case parseJsonInlineLink' input rest of
    Left err -> Left err
    Right (Link p _) -> Right p
    Right _ -> Left "FATAL: parseJsonInlineLink' returned not a Link"
  Right (Link updated (getJsonString value))
parseJsonInlineLink'handleField input "content" valueStr rest = do
  value <- case parseJson valueStr of
    Left err -> Left err
    Right (Paragraph p) -> Right p
    Right _ -> Left "AAAAA"
  updated <- case parseJsonInlineLink' input rest of
    Left err -> Left err
    Right (Link _ p) -> Right p
    Right _ -> Left "FATAL: parseJsonInlineLink' returned not a Link"
  Right (Link value updated)
parseJsonInlineLink'handleField _ key _ _ = Left $ "Unknown key: " ++ key

parseJsonInlineImage' :: Inline -> String -> Either String Inline
parseJsonInlineImage' input "" = Right input
parseJsonInlineImage' input inner =
  getNextObjectPair inner >>= \((key, valueStr), rest) ->
    parseJsonInlineImage'handleField input key valueStr rest

parseJsonInlineImage'handleField :: Inline -> String -> String -> String 
  -> Either String Inline
parseJsonInlineImage'handleField input "url" valueStr rest = do
  value <- parseJson valueStr
  updated <- case parseJsonInlineImage' input rest of
    Left err -> Left err
    Right (Image p _) -> Right p
    Right _ -> Left "FATAL: parseJsonInlineImage' returned not a Image"
  Right (Image updated (getJsonString value))
parseJsonInlineImage'handleField input "alt" valueStr rest = do
  value <- case parseJson valueStr of
    Left err -> Left err
    Right (Paragraph p) -> Right p
    Right _ -> Left "AAAAA"
  updated <- case parseJsonInlineImage' input rest of
    Left err -> Left err
    Right (Image _ p) -> Right p
    Right _ -> Left "FATAL: parseJsonInlineImage' returned not a Image"
  Right (Image value updated)
parseJsonInlineImage'handleField _ key _ _ = Left $ "Unknown key: " ++ key

parseJsonInlineObject' :: String -> Either String Inline
parseJsonInlineObject' inner = 
  getNextObjectPair inner >>= \((key, valueStr), _) ->
    parseJsonInlineObject'handleField key valueStr

parseJsonInlineObject'handleField :: String -> String -> Either String Inline
parseJsonInlineObject'handleField "bold" valueStr = do
  value <- parseJson valueStr
  Right (Strong (getJsonString value))
parseJsonInlineObject'handleField "italic" valueStr = do
  value <- parseJson valueStr
  Right (Emph (getJsonString value))
parseJsonInlineObject'handleField "code" valueStr = do
  value <- parseJson valueStr
  Right (CodeInline (getJsonString value))
parseJsonInlineObject'handleField "link" valueStr =
  parseJsonInlineLink' (Link [] "") (getObjectInner valueStr)
parseJsonInlineObject'handleField "image" valueStr =
  parseJsonInlineImage' (Image [] "") (getObjectInner valueStr)
parseJsonInlineObject'handleField key _ = Left $ "Unknown key: " ++ key

instance JsonParsable Inline where
  parseJson inner@('"':_) = do
    value <- parseJson inner
    Right $ PlainText $ getJsonString $ value
  parseJson inner@('{':_) = parseJsonInlineObject' (getObjectInner inner)
  parseJson _ = Left "Inline::parseJson: Neither a string nor an object"

trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse
