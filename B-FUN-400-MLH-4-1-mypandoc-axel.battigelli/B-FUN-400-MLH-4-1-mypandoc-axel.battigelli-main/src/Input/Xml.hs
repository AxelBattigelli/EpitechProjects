{-
-- EPITECH PROJECT, 2025
-- myPandoc
-- File description:
-- Xml.hs
-}

module Input.Xml (parseXmlDocument, parseXmlImage'parsePlainTextImage, parseXmlImage') where

import Document (Header(..),Document(..),Body(..),Block(..),Inline(..))
import Data.List (isPrefixOf, dropWhileEnd)

import Data.Char (isSpace)
import Text.ParserCombinators.ReadP

class XmlParsable a where
  parseXml :: String -> [(String, String)] -> String -> Either String a

newtype XmlString = XmlString { getXmlString :: [Char] }

instance XmlParsable XmlString where
  parseXml _ [] inner = Right (XmlString inner)
  parseXml t _ _ = Left ("ZZZZZ: "++t)

parseXmlDocument :: String -> Either String Document
parseXmlDocument str = 
  case extractTag "document" str of
    Left err -> Left err
    Right (doc, "") -> Right doc {- (traceShowId doc) -}
    _ -> Left "Bad Document"

parseAttributes :: String -> [(String, String)]
parseAttributes s = case readP_to_S attrsParser s of
  [] -> []
  results -> fst (last results)

attrsParser :: ReadP [(String, String)]
attrsParser = many $ do
  skipSpaces
  key <- munch1 (/= '=')
  _ <- char '='
  _ <- char '"'
  val <- munch (/= '"')
  _ <- char '"'
  return (key, val)

extractTag :: XmlParsable t => String -> String -> Either String (t, String)
extractTag tagName input = 
  let input' = trim input
      openTag = "<" ++ tagName
      closeTag = "</" ++ tagName ++ ">"
  in case findTag input' openTag closeTag of
       Nothing -> Left $ "Tag <" ++ tagName ++ "> not found."
       Just (attrsStr, inner, before, after) ->
         case parseXml tagName (parseAttributes attrsStr) inner of
           Left err -> Left err
           Right parsed -> Right (parsed, before ++ after)

findTag :: String -> String -> String -> Maybe (String, String, String, String)
findTag input openTag _ = do
  (before, rest) <- breakOn openTag input
  (tagContent, after) <- spanUntilTagClosed rest tagName
  let (attrs, inner) = parseTagContent tagContent
    in Just (attrs, inner, before, after)
  where
    tagName = drop 1 openTag

spanUntilTagClosed :: String -> String -> Maybe (String, String)
spanUntilTagClosed input tagName = 
  case go 0 (drop (length open) input) of
    Just (l, r) -> Just (l, r)
    Nothing -> Nothing
  where
    open = "<" ++ tagName
    close = "</" ++ tagName ++ ">"
    go _ "" = Nothing
    go 0 s
      | close `isPrefixOf` s =
          let (_, rest) = splitAt (length close) s
          in Just ("", rest)
    go n s
      | open `isPrefixOf` s = 
          case go (n + 1) (drop (length open) s) of
            Just (inner, rest) -> Just (open ++ inner, rest)
            Nothing -> Nothing
    go n s
      | close `isPrefixOf` s =
          case go (n - 1) (drop (length close) s) of
            Just (inner, rest) -> Just (close ++ inner, rest)
            Nothing -> Nothing
    go n s =
      case go n (tail s) of
        Just (inner, rest) -> Just (head s : inner, rest)
        Nothing -> Nothing

parseTagContent :: String -> (String, String)
parseTagContent s =
  let inside = dropWhile (/= '>') s
      attrs = takeWhile (/= '>') (s)
  in (trim attrs, trim (drop 1 inside))

breakOn :: String -> String -> Maybe (String, String)
breakOn pat str = go "" str
  where
    go _ [] = Nothing
    go acc s@(x:xs)
      | pat `isPrefixOf` s = Just (reverse acc, s)
      | otherwise = go (x:acc) xs

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace

extractTagType :: String -> Maybe String
extractTagType input = 
  case dropWhile isSpace input of
    '<':rest -> Just $ takeWhile (\c -> not (isSpace c) && c /= '>') rest
    _ -> Nothing

parseXmlDocument' :: Document -> String -> Either String Document
parseXmlDocument' input "" = Right input
parseXmlDocument' input inner =
  case extractTagType (id $ inner) of
    Just "header" ->
      extractTag "header" inner >>= \parsed ->
        parseXmlDocument' input{header = (fst parsed)} (snd parsed)
    Just "body" ->
      extractTag "body" inner >>= \parsed ->
        parseXmlDocument' input{body = (fst parsed)} (snd parsed)
    Just tagType -> Left ("Bad Document: Unknown tag type " ++ tagType)
    Nothing -> Left "Bad Document"

instance XmlParsable Document where
  parseXml "document" [] inner = parseXmlDocument' (Document (Header Nothing Nothing Nothing) (Body [])) inner
  parseXml "document" _ _ = Left "Bad Document: Bad attributes"
  parseXml tag _ _ = Left $ "Bad Document: Bad tag " ++ tag

parseXmlHeader' :: Header -> String -> Either String Header
parseXmlHeader' input "" = Right input
parseXmlHeader' input inner =
  case extractTagType inner of
    Just "author" ->
      extractTag "author" inner >>= \(val, rest) ->
        parseXmlHeader' input {author = Just (getXmlString val)} rest
    Just "date" ->
      extractTag "date" inner >>= \(val, rest) ->
        parseXmlHeader' input {date = Just (getXmlString val)} rest
    Just tagType -> Left ("Bad Header: Unknown tag type " ++ tagType)
    Nothing -> Left "Bad Header"

instance XmlParsable Header where
  parseXml "header" [("title", titleInput)] str = 
    parseXmlHeader' (Header {title = Just (titleInput), author = Nothing, date = Nothing}) str 
  parseXml "header" [] str = 
    parseXmlHeader' (Header Nothing Nothing Nothing) str 
  parseXml _ _ _ = Left "Bad Header: Bad attributes"

parseXmlBody' :: Body -> String -> Either String Body
parseXmlBody' input "" = Right input
parseXmlBody' input inner =
  case extractTagType inner of
    Just "paragraph" -> extractTag "paragraph" inner >>= \(val, rest) ->
      parseXmlBody' input rest >>= \inp ->
        Right inp{blocks=(val:(blocks inp))}
    Just "section" -> extractTag "section" inner >>= \(val, rest) ->
      parseXmlBody' input rest >>= \inp ->
        Right inp{blocks=(val:(blocks inp))}
    Just tagType -> Left ("Bad Body: Unknown tag type " ++ tagType)
    Nothing -> Left "Bad Body"

instance XmlParsable Body where
  parseXml "body" [] str =
    parseXmlBody' (Body []) str
  parseXml _ _ _ = Left "Bad Body: Bad attributes"

parseXmlParagraph' :: Block -> String -> Either String Block
parseXmlParagraph' input "" = Right input
parseXmlParagraph' input inner = case extractTagType inner of
    Just tag | tag `elem` ["bold", "italic", "code", "link", "image"] ->
      parseXmlParagraph'handleTag input tag inner
    Just tagType -> Left ("Bad Paragraph: Unknown tag type " ++ tagType)
    Nothing ->
      let (text, rest) = span (/= '<') inner
        in parseXmlParagraph' input rest >>= \a ->
          case a of
            Paragraph inp -> Right (Paragraph (PlainText text : inp))
            _ -> Left "parseXmlParagraph': did not return a Paragraph"
  
parseXmlParagraph'handleTag :: Block -> String -> String -> Either String Block
parseXmlParagraph'handleTag input tagName str = do
  (val, rest) <- extractTag tagName str
  result <- parseXmlParagraph' input rest
  case result of
    Paragraph inp -> Right (Paragraph (val : inp))
    _ -> Left "parseXmlParagraph': did not return a Paragraph"

parseXmlSection' :: Block -> String -> Either String Block
parseXmlSection' input "" = Right input
parseXmlSection' input inner =
  case extractTagType inner of
    Just tag | tag `elem` ["paragraph", "section", "codeblock", "list"] ->
      parseXmlSection'handleSectionTag input tag inner
    Just tagType ->
      Left ("Bad Section: Unknown tag type " ++ tagType)
    Nothing ->
      Left "Bad Section"

parseXmlSection'handleSectionTag :: Block -> String -> String -> Either String Block
parseXmlSection'handleSectionTag input tagName str = do
  (val, rest) <- extractTag tagName str
  result <- parseXmlSection' input rest
  case result of
    Section sectionTitle inp -> Right (Section sectionTitle (val : inp))
    _ -> Left "parseXmlSection': parseXmlSection' did not return a Section"

parseXmlCodeBlock' :: Block -> String -> Either String Block
parseXmlCodeBlock' input "" = Right input
parseXmlCodeBlock' input inner =
  case extractTagType inner of
    Just "paragraph" -> case extractTag "paragraph" inner of
      Left err -> Left err
      Right (val, rest) -> case parseXmlCodeBlock' input rest of
        Left err -> Left err
        Right (CodeBlock lang inp) -> Right (CodeBlock lang (val:inp))
        Right _ -> Left "parseXmlCodeBlock': did not return a CodeBlock"
    Just tagType -> Left ("Bad CodeBlock: Unknown tag type " ++ tagType)
    Nothing -> Left "Bad CodeBlock"

parseXmlList' :: Block -> String -> Either String Block
parseXmlList' input "" = Right input
parseXmlList' input inner =
  case extractTagType inner of
    Just "paragraph" -> case extractTag "paragraph" inner of
      Left err -> Left err
      Right (val, rest) -> case parseXmlList' input rest of
        Left err -> Left err
        Right (List inp) -> Right (List (val:inp))
        Right _ -> Left "parseXmlList': parseXmlList' did not return a List"
    Just tagType -> Left ("Bad List: Unknown tag type " ++ tagType)
    Nothing -> Left "Bad List"

instance XmlParsable Block where
  parseXml "paragraph" [] str =
    parseXmlParagraph' (Paragraph []) str
  parseXml "section" [("title", "")] str =
    parseXmlSection' (Section Nothing []) str
  parseXml "section" [("title", titleInput)] str =
    parseXmlSection' (Section (Just titleInput) []) str
  parseXml "section" [] str =
    parseXmlSection' (Section Nothing []) str
  parseXml "codeblock" [] str =
    parseXmlCodeBlock' (CodeBlock Nothing []) str
  parseXml "codeblock" [("lang", langInput)] str =
    parseXmlCodeBlock' (CodeBlock (Just langInput) []) str
  parseXml "list" [] str =
    parseXmlList' (List []) str
  parseXml tag _ _ = Left $ "Block::parseXml: Unknown tag " ++ tag

parseXmlLink' :: Inline -> String -> Either String Inline
parseXmlLink' input "" = Right input
parseXmlLink' input inner =
  case extractTagType inner of
    Just tag -> parseXmlLink'handleInlineTag input tag inner
    Nothing  -> parseXmlLink'parsePlainTextLink input inner

parseXmlLink'handleInlineTag :: Inline -> String -> String -> Either String Inline
parseXmlLink'handleInlineTag input tag str
  | tag `elem` ["bold", "italic", "code", "link"] = do
      (val, rest) <- extractTag tag str
      result <- parseXmlLink' input rest
      case result of
        Link inp href -> Right (Link (val : inp) href)
        _ -> Left "parseXmlLink': parseXmlLink' did not return a Link"
  | otherwise = Left ("Bad Link: Unknown tag type " ++ tag)

parseXmlLink'parsePlainTextLink :: Inline -> [Char] -> Either String Inline
parseXmlLink'parsePlainTextLink input str = do
  let (text, rest) = span (/= '<') str
  result <- parseXmlLink' input rest
  case result of
    Link inp href -> Right (Link (PlainText text : inp) href)
    _ -> Left "parseXmlLink': parseXmlLink' did not return a Link"

parseXmlImage' :: Inline -> String -> Either String Inline
parseXmlImage' input "" = Right input
parseXmlImage' input inner =
  case extractTagType inner of
    Just tag -> parseXmlImage'handleInlineTag input tag inner
    Nothing  -> parseXmlImage'parsePlainTextImage input inner

parseXmlImage'handleInlineTag :: Inline -> String -> String -> Either String Inline
parseXmlImage'handleInlineTag input tag str
  | tag `elem` ["bold", "italic", "code", "link"] = do
      (val, rest) <- extractTag tag str
      result <- parseXmlImage' input rest
      case result of
        Image inp src -> Right (Image (val : inp) src)
        _ -> Left "parseXmlImage': parseXmlImage' did not return an Image"
  | otherwise = Left ("Bad Image: Unknown tag type " ++ tag)

parseXmlImage'parsePlainTextImage :: Inline -> [Char] -> Either String Inline
parseXmlImage'parsePlainTextImage input str = do
  let (text, rest) = span (/= '<') str
  result <- parseXmlImage' input rest
  case result of
    Image inp src -> Right (Image (PlainText text : inp) src)
    _ -> Left "parseXmlImage': parseXmlImage' did not return an Image"

instance XmlParsable Inline where
  parseXml "bold" [] str = Right $ Strong str
  parseXml "italic" [] str = Right $ Emph str
  parseXml "code" [] str = Right $ CodeInline str
  parseXml "link" [("url", inputUrl)] str =
    parseXmlLink' (Link [] inputUrl) str
  parseXml "image" [("url", inputUrl)] str =
    parseXmlImage' (Image [] inputUrl) str
  parseXml other _ _ = Left ("Bad Inline: Unknown type " ++ other)