{-
-- EPITECH PROJECT, 2025
-- myPandoc
-- File description:
-- Xml.hs
-}

module Output.Xml (documentToXml, indent, inlineToXml, blockToXml, bodyXml, headerXml) where

import Document (Document(..), Block(..), Body(..), Header(..), Inline(..))

documentToXml :: Document -> String
documentToXml doc =
    "<document>\n" ++ headerXml (header doc) ++ bodyXml (body doc) ++
        "</document>"

headerXml :: Header -> String
headerXml hdr =
  let titleLine = maybe "" id (title hdr)
      authorLine = maybe "" (\a -> indent 2 ++ "<author>" ++ a ++
        "</author>\n") (author hdr)
      dateLine = maybe "" (\d -> indent 2 ++ "<date>" ++ d ++
        "</date>\n") (date hdr)
  in concat
     [ indent 1 ++ "<header title=\"" ++ titleLine ++ "\">\n"
     , authorLine , dateLine, indent 1 ++ "</header>\n" ]

bodyXml :: Body -> String
bodyXml b =
  indent 1 ++ "<body>\n"
  ++ blockToXml 2 (blocks b)
  ++ indent 1 ++ "</body>\n"

blockToXml :: Int -> [Block] -> String
blockToXml _ [] = ""
blockToXml level (Paragraph inlines : xs) =
    indent level ++ "<paragraph>" ++ inlineToXml inlines ++ "</paragraph>\n" ++
    blockToXml level xs
blockToXml level (Section (Just sectionTitle) contents : xs) =
    indent level ++ "<section title=\"" ++ sectionTitle ++ "\">\n" ++
    blockToXml (level + 1) contents ++ indent level ++ "</section>\n" ++
    blockToXml level xs
blockToXml level (Section Nothing contents : xs) =
    indent level ++ "<section title=\"\">\n" ++
    blockToXml (level + 1) contents ++ indent level ++ "</section>\n" ++
    blockToXml level xs
blockToXml level (CodeBlock (Just lang) contents : xs) =
    indent level ++ "<codeblock language=\"" ++ lang ++ "\">\n" ++
    blockToXml (level + 1) contents ++ indent level ++ "</codeblock>\n" ++
    blockToXml level xs
blockToXml level (CodeBlock Nothing contents : xs) =
    indent level ++ "<codeblock>\n" ++ blockToXml (level + 1) contents ++
    indent level ++ "</codeblock>\n" ++ blockToXml level xs
blockToXml level (List items : xs) =
    indent level ++ "<list>\n" ++ concatMap (\item -> (blockToXml (level + 1)
    [item])) items ++ indent level ++ "</list>\n" ++ blockToXml level xs

inlineToXml :: [Inline] -> String
inlineToXml [] = ""
inlineToXml (PlainText text : xs) = text ++ inlineToXml xs
inlineToXml (Emph text : xs) =
    "<italic>" ++ text ++ "</italic>" ++ inlineToXml xs
inlineToXml (Strong text : xs) =
    "<bold>" ++ text ++ "</bold>" ++ inlineToXml xs
inlineToXml (CodeInline text : xs) =
    "<code>" ++ text ++ "</code>" ++ inlineToXml xs
inlineToXml (Image alt src : xs) =
    "<image url=\"" ++ src ++ "\">" ++ inlineToXml alt ++ "</image>" ++
    inlineToXml xs
inlineToXml (Link alt href : xs) =
    "<link url=\"" ++ href ++ "\">" ++ inlineToXml alt ++ "</link>" ++
    inlineToXml xs

indent :: Int -> String
indent lvl = replicate (lvl * 4) ' '
