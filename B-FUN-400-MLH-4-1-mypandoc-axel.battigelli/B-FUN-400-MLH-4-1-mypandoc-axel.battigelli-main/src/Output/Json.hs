{-
-- EPITECH PROJECT, 2025
-- myPandoc
-- File description:
-- Json.hs
-}

module Output.Json (documentToJson, stringOrEmpty, inlineListJson, 
  inlineToJson, blockListJson, blockToJson, headerToJson) where

import Document (Document(..), Block(..), Body(..), Header(..), Inline(..))

documentToJson :: Document -> String
documentToJson doc =
  let
    hdr = header doc
    meta = headerToJson hdr
    bodyStr = indent 1 ++ "\"body\": [\n" ++ 
      blockListJson 2 (blocks (body doc)) ++ "\n" ++ indent 1 ++ "]\n"
  in "{\n" ++ meta ++ bodyStr ++ "}"

headerToJson :: Header -> String
headerToJson hdr = 
  let 
  titleLine = maybe "" 
    (\t -> indent 2 ++ "\"title\": " ++ show t ++ ",\n") (title hdr)
  authorLine = maybe ""
    (\a -> indent 2 ++ "\"author\": " ++ show a ++ ",\n") (author hdr)
  dateLine = maybe ""
    (\d -> indent 2 ++ "\"date\": " ++ show d ++ "\n") (date hdr)
  in indent 1 ++ "\"header\": {\n" ++ titleLine ++ authorLine ++ dateLine ++
    indent 1 ++ "},\n"

blockListJson :: Int -> [Block] -> String
blockListJson _ [] = ""
blockListJson level [b] = blockToJson level b
blockListJson level (b:bs) =
  blockToJson level b ++ ",\n" ++ blockListJson level bs

blockToJson :: Int -> Block -> String
blockToJson lvl (Paragraph inlines) =
  indent lvl ++ "[\n" ++ inlineListJson (lvl + 1) inlines ++ "\n" ++
  indent lvl ++ "]"
blockToJson lvl (Section sectionTitle content) =
  indent lvl ++ "{\n" ++ 
  indent (lvl + 1) ++ "\"section\": {\n" ++
  indent (lvl + 2) ++ "\"title\": " ++ stringOrEmpty sectionTitle ++ ",\n" ++
  indent (lvl + 2) ++ "\"content\": [\n" ++
  blockListJson (lvl + 3) content ++ "\n" ++
  indent (lvl + 2) ++ "]\n" ++
  indent (lvl + 1) ++ "}\n" ++
  indent lvl ++ "}"
blockToJson lvl (CodeBlock _ content) =
  indent lvl ++ "{\n" ++
  indent (lvl + 1) ++ "\"codeblock\": [\n" ++
  blockListJson (lvl + 2) content ++ "\n" ++
  indent (lvl + 1) ++ "]\n" ++
  indent lvl ++ "}"
blockToJson lvl (List items) =
  indent lvl ++ "{\n" ++
  indent (lvl + 1) ++ "\"list\": [\n" ++
  blockListJson (lvl + 2) items ++ "\n" ++
  indent (lvl + 1) ++ "]\n" ++
  indent lvl ++ "}"

inlineListJson :: Int -> [Inline] -> String
inlineListJson _ [] = ""
inlineListJson lvl [x] = inlineToJson lvl x
inlineListJson lvl (x:xs) = inlineToJson lvl x ++ ",\n" ++
  inlineListJson lvl xs

inlineToJson :: Int -> Inline -> String
inlineToJson lvl (PlainText text) = indent lvl ++ show text
inlineToJson lvl (Emph text) =
  indent lvl ++ "{\n" ++
  indent (lvl + 1) ++ "\"italic\": " ++ show text ++ "\n" ++
  indent lvl ++ "}"
inlineToJson lvl (Strong text) =
  indent lvl ++ "{\n" ++
  indent (lvl + 1) ++ "\"bold\": " ++ show text ++ "\n" ++
  indent lvl ++ "}"
inlineToJson lvl (CodeInline text) =
  indent lvl ++ "{\n" ++
  indent (lvl + 1) ++ "\"code\": " ++ show text ++ "\n" ++
  indent lvl ++ "}"
inlineToJson lvl (Image alt src) =
  indent lvl ++ "{\n" ++
  indent (lvl + 1) ++ "\"image\": {\n" ++
  indent (lvl + 2) ++ "\"url\": " ++ show src ++ ",\n" ++
  indent (lvl + 2) ++ "\"alt\": [\n" ++ 
  inlineListJson (lvl + 3) alt ++ "\n" ++
  indent (lvl + 2) ++ "]\n" ++
  indent (lvl + 1) ++ "}\n" ++
  indent lvl ++ "}"
inlineToJson lvl (Link alt href) =
  indent lvl ++ "{\n" ++
  indent (lvl + 1) ++ "\"link\": {\n" ++
  indent (lvl + 2) ++ "\"url\": " ++ show href ++ ",\n" ++
  indent (lvl + 2) ++ "\"content\": [\n" ++ 
  inlineListJson (lvl + 3) alt ++ "\n" ++
  indent (lvl + 2) ++ "]\n" ++
  indent (lvl + 1) ++ "}\n" ++
  indent lvl ++ "}"

stringOrEmpty :: Maybe String -> String
stringOrEmpty = maybe "\"\"" show

indent :: Int -> String
indent lvl = replicate (lvl * 4) ' '
