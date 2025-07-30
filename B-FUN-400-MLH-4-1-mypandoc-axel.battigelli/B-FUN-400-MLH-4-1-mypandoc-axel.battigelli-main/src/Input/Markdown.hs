{-
-- EPITECH PROJECT, 2025
-- myPandoc
-- File description:
-- Markdown.hs
-}

module Input.Markdown (parseMarkdownDocument) where

import Document (Header(..), Document(..), Body(..), Block(..), Inline(..))
import Data.Char (isSpace)
import Data.List (isPrefixOf, stripPrefix, tails)
import Debug.Trace (trace, traceShow, traceShowId)

class MarkdownParsable a where
  parseMarkdown :: String -> Either String a

newtype MarkdownString = MarkdownString { getMarkdownString :: String }

instance MarkdownParsable MarkdownString where
  parseMarkdown inner = Right (MarkdownString (inner))

parseMarkdownDocument :: String -> Either String Document
parseMarkdownDocument str = case parseMarkdown str of
  Left err -> Left err
  Right docMarkdown -> Right docMarkdown

instance MarkdownParsable Document where
  parseMarkdown inner = parseMarkdownDocument' (Document{header = (Header Nothing Nothing Nothing), body = (Body [])}) (inner)

parseHeader :: [String] -> (Header, [String])
parseHeader ("---":rest) =
  let (headerLines, remaining) = break (== "---") rest
      hdr = 
        foldl parseHeaderParseLine (Header Nothing Nothing Nothing) headerLines
  in (hdr, drop 1 remaining)
parseHeader ls = (Header Nothing Nothing Nothing, ls)

parseHeaderParseLine :: Header -> String -> Header
parseHeaderParseLine h line
  | Just v <- stripPrefix "title: " line = h { title = Just (trim v) }
  | Just v <- stripPrefix "author: " line = h { author = Just (trim v) }
  | Just v <- stripPrefix "date: " line = h { date = Just (trim v) }
  | otherwise = h

parseBody :: [String] -> (Body, [String])
parseBody [] = (Body [], [])
parseBody ls = 
  let 
      (parsedBlock, remaining) = parseBlock 0 ls
      (Body otherBlocks, finalRest) = (parseBody (dropWhile null (remaining)))
  in case parsedBlock of
     Just block -> (Body (block : otherBlocks), finalRest)
     Nothing -> (Body otherBlocks, finalRest)

parseBlock :: Int -> [String] -> (Maybe Block, [String])
parseBlock _ [] = (Nothing, [])
parseBlock sectionIdx (line:rest)
  | "#" `isPrefixOf` line = parseSection sectionIdx (line:rest)
  | "-" `isPrefixOf` line = parseList (line:rest)
  | "```" `isPrefixOf` line = parseCodeBlock (line:rest)
  | otherwise = parseParagraph (line:rest)

parseList :: [String] -> (Maybe Block, [String])
parseList [] = (Nothing, [])
parseList ls =
  let (listLines, rest) = span isListItem ls
      items = map (Paragraph . parseInlines . drop 2) listLines
  in (Just (List items), rest)

isListItem :: String -> Bool
isListItem line = "- " `isPrefixOf` line

parseCodeBlock :: [String] -> (Maybe Block, [String])
parseCodeBlock [] = (Nothing, [])
parseCodeBlock (line:rest)
  | "```" `isPrefixOf` line =
      let lang = case stripPrefix "```" line of
                   Just l | not (null l) -> Just (trim l)
                   _ -> Nothing
      in parseCodeBlock' lang rest
  | otherwise = (Nothing, line : rest)

parseCodeBlock' :: Maybe String -> [String] -> (Maybe Block, [String])
parseCodeBlock' lang lines =
  let (codeLines, remaining) = break (== "```") lines
      codeBlocks = map (Paragraph . (:[]) . PlainText) codeLines
  in case remaining of
       (_:afterEnd) -> (Just (CodeBlock lang codeBlocks), afterEnd)
       [] -> (Just (CodeBlock lang codeBlocks), [])

parseSection :: Int -> [String] -> (Maybe Block, [String])
parseSection _ [] = (Nothing, [])
parseSection sectionIdx (line:rest) =
  case getSectionDepth line of
    Nothing -> (Nothing, line:rest)
    Just depth
      | depth <= sectionIdx -> (Just $ emptySection, line:rest)
      | sectionIdx + 1 < depth -> parseNestedSection sectionIdx (line:rest)
      | otherwise -> parseCurrentSection sectionIdx line rest

getSectionDepth :: String -> Maybe Int
getSectionDepth line =
  let prefix = takeWhile (== '#') line
  in if null prefix then Nothing else Just (length prefix)

trimTitle :: String -> String
trimTitle = trim . dropWhile (== '#')

emptySection :: Block
emptySection = Section (Just " ") []

parseNestedSection :: Int -> [String] -> (Maybe Block, [String])
parseNestedSection sectionIdx input =
  let (inner, rest) = parseBlock (sectionIdx + 1) input
  in case inner of
       Just block -> (Just $ Section (Just "") [block], rest)
       Nothing    -> (Nothing, rest)

parseCurrentSection :: Int -> String -> [String] -> (Maybe Block, [String])
parseCurrentSection sectionIdx line rest =
  let title = trimTitle line
  in parseSection' (sectionIdx + 1) (Section (Just title) []) rest

parseSection' :: Int -> Block -> [String] -> (Maybe Block, [String])
parseSection' _ input [] = (Just input, [])

parseSection' sectionIdx input@(Section title' contents') ls =
  let
    blockLines = dropWhile null ls
    (parsedBlock, rest') = parseBlock sectionIdx blockLines
    (updatedSection, remainingLines) = parseSection' sectionIdx input rest'
  in
    mergeParsedBlock 
      parsedBlock updatedSection title' contents' rest' remainingLines

mergeParsedBlock :: Maybe Block -> Maybe Block -> Maybe String -> [Block] -> [String] -> [String] -> (Maybe Block, [String])
mergeParsedBlock (Just (Section (Just " ") [])) _ title contents rest' _ =
  (Just $ Section title contents, rest')
mergeParsedBlock (Just blk) (Just (Section title contents)) _ _ _ remaining =
  (Just $ Section title (blk : contents), remaining)
mergeParsedBlock Nothing (Just (Section title contents)) _ _ _ remaining =
  (Just $ Section title contents, remaining)
mergeParsedBlock _ _ title contents rest' _ =
  (Just $ Section title contents, rest')

parseParagraph :: [String] -> (Maybe Block, [String])
parseParagraph ls@(x:xs) =
  let
      (blockLines, rest) = break null ls
      text = unwords blockLines
      inlines = parseInlines text
  in case blockLines of
    [] -> (Nothing, rest)
    _ -> (Just $ Paragraph inlines, rest)

parseInlines :: String -> [Inline]
parseInlines "" = []
parseInlines s
  | Just rest <- stripPrefix "![" s = parseImage rest
  | Just rest <- stripPrefix "[" s = parseLink rest
  | Just rest <- stripPrefix "**" s = parseStrong rest
  | Just rest <- stripPrefix "*" s = parseEmph rest
  | Just rest <- stripPrefix "`" s = parseCode rest
  | otherwise = parsePlain s

parseImage :: String -> [Inline]
parseImage s =
  let (alt, rest') = breakOn "]" s
  in case stripPrefix "(" rest' of
       Just rest'' -> let (url, after) = breakOn ")" rest'' in
                      Image (parseInlines alt) url : parseInlines after
       Nothing -> [PlainText "!["] ++ parseInlines s

parseLink :: String -> [Inline]
parseLink s =
  let (content, rest') = breakOn "]" s
  in case stripPrefix "(" rest' of
       Just rest'' -> let (url, after) = breakOn ")" rest'' in
                      Link (parseInlines content) url : parseInlines after
       Nothing -> [PlainText "["] ++ parseInlines s

parseStrong :: String -> [Inline]
parseStrong s =
  let (boldText, after) = breakOn "**" s
  in Strong boldText : parseInlines after

parseEmph :: String -> [Inline]
parseEmph s =
  let (emphText, after) = breakOn "*" s
  in Emph emphText : parseInlines after

parseCode :: String -> [Inline]
parseCode s =
  let (codeText, after) = breakOn "`" s
  in CodeInline codeText : parseInlines after

parsePlain :: String -> [Inline]
parsePlain s =
  let (plain, rest) =
        break (\c -> c == '*' || c == '`' || c == '[' || c == '!') s
  in PlainText plain : parseInlines rest

breakOn :: String -> String -> (String, String)
breakOn _ "" = ("", "")
breakOn pat str@(x:xs)
  | Just rest <- stripPrefix pat xs =
    ([x], rest)
  | otherwise = let (l, r) = breakOn pat xs in
    ((x:l), r)

parseMarkdownDocument' :: Document -> String -> Either String Document
parseMarkdownDocument' input "" = Right input
parseMarkdownDocument' input inner =
  let
    ls = lines inner
    (parsedHeader, restAfterHeader) = parseHeader ls
    (parsedBody, remainingLines) = parseBody restAfterHeader
    newDoc = input { header = parsedHeader, body = parsedBody }
  -- in trace ("Parsed body: " ++ show parsedBody) $
    in Right newDoc

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace
