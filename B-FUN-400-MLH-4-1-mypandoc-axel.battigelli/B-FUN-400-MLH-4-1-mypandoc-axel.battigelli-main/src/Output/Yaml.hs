{-
-- EPITECH PROJECT, 2025
-- myPandoc
-- File description:
-- Yaml.hs
-}

module Output.Yaml (documentToYaml, inlineListYaml, inlineToYaml, 
  blockListYaml, blockToYaml, headerToYaml) where

import Document (Document(..), Block(..), Body(..), Header(..), Inline(..))

documentToYaml :: Document -> String
documentToYaml doc =
  let
    hdr = header doc
    meta = headerToYaml hdr
    bodyStr = indent 1 ++ "body:\n" ++ 
      blockListYaml 2 (blocks (body doc))
  in "document:\n" ++ meta ++ bodyStr

headerToYaml :: Header -> String
headerToYaml hdr = 
  let 
  titleLine = maybe "" 
    (\t -> indent 2 ++ "title: " ++ show t ++ "\n") (title hdr)
  authorLine = maybe ""
    (\a -> indent 2 ++ "author: " ++ show a ++ "\n") (author hdr)
  dateLine = maybe ""
    (\d -> indent 2 ++ "date: " ++ show d ++ "\n") (date hdr)
  in indent 1 ++ "header:\n" ++ titleLine ++ authorLine ++ dateLine

blockListYaml :: Int -> [Block] -> String
blockListYaml _ [] = ""
blockListYaml level [b] = blockToYaml level b
blockListYaml level (b:bs) =
  blockToYaml level b ++ "\n" ++ blockListYaml level bs

blockToYaml :: Int -> Block -> String
blockToYaml lvl (Paragraph inlines) =
  indent lvl ++ "- paragraph:\n" ++ 
  inlineListYaml (lvl + 1) inlines
blockToYaml lvl (Section sectionTitle content) =
  indent lvl ++ "- section:\n" ++
  indent (lvl + 2) ++ "title: " ++ stringOrEmpty sectionTitle ++ "\n" ++
  indent (lvl + 2) ++ "content:\n" ++
  blockListYaml (lvl + 3) content
blockToYaml lvl (CodeBlock _ content) =
  indent lvl ++ "- codeblock:\n" ++
  blockListYaml (lvl + 1) content
blockToYaml lvl (List items) =
  indent lvl ++ "- list:\n" ++
  blockListYaml (lvl + 1) items

inlineListYaml :: Int -> [Inline] -> String
inlineListYaml _ [] = ""
inlineListYaml lvl [x] = inlineToYaml lvl x
inlineListYaml lvl (x:xs) = inlineToYaml lvl x ++ "\n" ++
  inlineListYaml lvl xs

inlineToYaml :: Int -> Inline -> String
inlineToYaml lvl (PlainText text) = indent lvl ++ "- " ++ show text
inlineToYaml lvl (Emph text) =
  indent lvl ++ "- italic: " ++ show text
inlineToYaml lvl (Strong text) =
  indent lvl ++ "- bold: " ++ show text
inlineToYaml lvl (CodeInline text) =
  indent lvl ++ "- code: " ++ show text
inlineToYaml lvl (Image alt src) =
  indent lvl ++ "- image:\n" ++
  indent (lvl + 1) ++ "url: " ++ show src ++ "\n" ++
  indent (lvl + 1) ++ "alt:\n" ++  inlineListYaml (lvl + 2) alt
inlineToYaml lvl (Link alt href) =
  indent lvl ++ "- link:\n" ++
  indent (lvl + 1) ++ "url: " ++ show href ++ "\n" ++
  indent (lvl + 1) ++ "content:\n" ++  inlineListYaml (lvl + 2) alt

stringOrEmpty :: Maybe String -> String
stringOrEmpty = maybe "\"\"" show

indent :: Int -> String
indent lvl = replicate (lvl * 2) ' '
