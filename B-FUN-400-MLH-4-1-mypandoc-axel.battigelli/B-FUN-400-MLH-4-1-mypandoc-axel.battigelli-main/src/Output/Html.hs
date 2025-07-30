{-
-- EPITECH PROJECT, 2025
-- myPandoc
-- File description:
-- Html.hs
-}

module Output.Html (documentToHtml, inlineToHtml, blockToHtml, bodyHtml,
    headerHtml) where

import Document (Document(..), Block(..), Body(..), Header(..), Inline(..))

documentToHtml :: Document -> String
documentToHtml doc =
    "<!DOCTYPE html>\n<html>\n" ++ headerHtml (header doc) ++
        bodyHtml (body doc) ++ "</html>"

headerHtml :: Header -> String
headerHtml hdr =
  let c = (indent 2) ++ "<meta charset='utf-8' />\n"
      t = maybe "" (\a -> indent 2 ++ "<title>" ++a++ "</title>\n")(author hdr)
      a = maybe "" (\a -> indent 2 ++
        "<meta name=\"author\" content=\"" ++ a ++ "\" />\n") (author hdr)
      d = maybe "" (\d -> indent 2 ++
        "<meta name=\"date\" content=\"" ++ d ++ "\" />\n") (date hdr)
      v = (indent 2) ++ "<meta name='viewport' content='width=device-width," ++
        " initial-scale=1' />\n"
  in concat [ indent 1 ++ "<head>\n", c, t, a , d, v, indent 1 ++ "</head>\n" ]

bodyHtml :: Body -> String
bodyHtml b =
  indent 1 ++ "<body>\n"
  ++ blockToHtml 1 2 (blocks b)
  ++ indent 1 ++ "</body>\n"

blockToHtml :: Int -> Int -> [Block] -> String
blockToHtml _ _ [] = ""
blockToHtml headingLevel level (Paragraph inlines : xs) =
    indent level ++ "<p>" ++ inlineToHtml inlines ++ "</p>\n" ++
    blockToHtml headingLevel level xs
blockToHtml headingLevel level (Section Nothing contents : xs) =
    blockToHtml (headingLevel + 1) level contents ++
    blockToHtml headingLevel level xs

blockToHtml headingLevel level (Section maybeTitle contents : xs) =
  let titlePart = case maybeTitle of
        Just "" -> ""
        Just title -> indent level ++ "<h" ++ show headingLevel ++ ">" ++
                      title ++ "</h" ++ show headingLevel ++ ">\n"
        Nothing -> ""
      contentsPart = blockToHtml (headingLevel + 1) level contents
      rest = blockToHtml headingLevel level xs
  in titlePart ++ contentsPart ++ rest
blockToHtml headingLevel level (CodeBlock (Just lang) contents : xs) =
    indent level ++ "<pre><code class=\"" ++ lang ++ "\">\n" ++
    blockToHtml headingLevel level contents ++ indent level ++
    "</code></pre>\n" ++ blockToHtml headingLevel level xs
blockToHtml headingLevel level (CodeBlock Nothing contents : xs) =
    indent level ++ "<pre><code>\n" ++
    blockToHtml headingLevel (level + 1) contents ++ indent level ++
    "</code></pre>\n" ++ blockToHtml headingLevel level xs
blockToHtml headingLevel level (List items : xs) =
    indent level ++ "<ul>\n" ++
    concatMap (\item -> (blockToHtml headingLevel (level + 1)
    [item])) items ++ indent level ++ "</ul>\n" ++
    blockToHtml headingLevel level xs

inlineToHtml :: [Inline] -> String
inlineToHtml [] = ""
inlineToHtml (PlainText text : xs) = text ++ inlineToHtml xs
inlineToHtml (Emph text : xs) =
    "<i>" ++ text ++ "</i>" ++ inlineToHtml xs
inlineToHtml (Strong text : xs) =
    "<strong>" ++ text ++ "</strong>" ++ inlineToHtml xs
inlineToHtml (CodeInline text : xs) =
    "<code>" ++ text ++ "</code>" ++ inlineToHtml xs
inlineToHtml (Image alt src : xs) =
    "<img src=\"" ++ src ++ "\" alt=\""++ inlineToHtml alt ++ "\"></img>" ++
    inlineToHtml xs
inlineToHtml (Link alt href : xs) =
    "<a href=\"" ++ href ++ "\">" ++ inlineToHtml alt ++ "</a>" ++
    inlineToHtml xs

indent :: Int -> String
indent lvl = replicate (lvl * 4) ' '
