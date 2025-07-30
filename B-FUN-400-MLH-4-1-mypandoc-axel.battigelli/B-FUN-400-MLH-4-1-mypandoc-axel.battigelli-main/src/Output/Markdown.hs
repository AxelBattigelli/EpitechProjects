{-
-- EPITECH PROJECT, 2025
-- myPandoc
-- File description:
-- Markdown.hs
-}

module Output.Markdown (documentToMarkdown, inlineToMd, blockToMd) where

import Data.List (intercalate)
import Document(Document(..),Block(..),Body(..),Header(..), Inline(..))

documentToMarkdown :: Document -> String
documentToMarkdown doc =
    let hdr = header doc
        bodyStr = blockToMd "\n\n" 0 (blocks (body doc))
        titleLine = maybe "" (\a -> "title: " ++ a) (title hdr)
        authorLine = maybe "" (\a -> "author: " ++ a) (author hdr)
        dateLine = maybe "" (\d -> "date: " ++ d) (date hdr)
        meta = unlines $ filter (not . null) 
            ["---", titleLine, authorLine, dateLine, "---\n"]
    in meta ++ bodyStr

-- blockToMd :: String -> Int -> [Block] -> String
-- blockToMd _ _ [] = ""
-- -- blockToMd separator level ((Paragraph inline):xs) =
-- --     inlineToMd inline ++ separator ++ (blockToMd separator level xs)
-- blockToMd separator level ((Paragraph inline):xs) =
--     inlineToMd inline ++
--     (if null xs then "" else separator ++ blockToMd separator level xs)
-- blockToMd _ level ((Section (Just "") contents):xs) =
--     (blockToMd "\n\n" (level + 1) contents) ++ (blockToMd "\n\n" level xs)
-- blockToMd _ level ((Section (Just sectionTitle) contents):xs) =
--     replicate (level + 1) '#' ++ " " ++ sectionTitle ++ "\n\n" ++
--     (blockToMd "\n\n" (level + 1) contents) ++ (blockToMd "\n\n" level xs)
-- blockToMd _ level ((Section (Nothing) contents):xs) =
--     (blockToMd "\n\n" (level + 1) contents) ++ (blockToMd "\n\n" level xs)
-- blockToMd _ level ((CodeBlock (Just lang) contents):xs) =
--      "```" ++ lang ++ "\n" ++ (blockToMd "\n" (level) contents) ++
--      "\n\n```\n" ++ (blockToMd "\n\n" level xs)
-- blockToMd _ level ((CodeBlock (Nothing) contents):xs) =
--     "```\n" ++ (blockToMd "\n" (level) contents) ++ "```\n" ++
--     (blockToMd "\n\n" level xs)
-- blockToMd _ level ((List items):xs) = 
--     concat (map (\v -> "- " ++ (blockToMd "\n" level [v])) items) ++ "\n" ++
--     blockToMd "\n\n" level xs
blockToMd :: String -> Int -> [Block] -> String
blockToMd _ _ [] = ""
blockToMd separator level [b] = blockToMdSingle separator level b
blockToMd separator level (b:bs) =
    blockToMdSingle separator level b ++ separator ++
    blockToMd separator level bs

blockToMdSingle :: String -> Int -> Block -> String
blockToMdSingle separator level (Paragraph inline) =
    inlineToMd inline
blockToMdSingle separator level (Section (Just "") contents) =
    blockToMd separator (level + 1) contents
blockToMdSingle separator level (Section (Just sectionTitle) contents) =
    replicate (level + 1) '#' ++ " " ++ sectionTitle ++ "\n\n" ++
    blockToMd separator (level + 1) contents
blockToMdSingle separator level (Section Nothing contents) =
    blockToMd separator (level + 1) contents
blockToMdSingle separator level (CodeBlock (Just lang) contents) =
    "```" ++ lang ++ "\n" ++ blockToMd "\n" level contents ++ "\n```"
blockToMdSingle separator level (CodeBlock Nothing contents) =
    "```\n" ++ blockToMd "\n" level contents ++ "\n```"
blockToMdSingle separator level (List items) =
    intercalate "\n" (map (\v -> "- " ++ blockToMd " " level [v]) items)

inlineToMd :: [Inline] -> String
inlineToMd [] = ""
inlineToMd ((PlainText text):xs) = text ++ inlineToMd xs
inlineToMd ((Emph text):xs) = "*" ++ text ++ "*" ++ inlineToMd xs
inlineToMd ((Strong text):xs) = "**" ++ text ++ "**" ++ inlineToMd xs
inlineToMd ((CodeInline text):xs) = "`" ++ text ++ "`" ++ inlineToMd xs
inlineToMd ((Image alt src):xs) =
    "![" ++ (inlineToMd alt) ++ "](" ++ src ++ ")" ++ inlineToMd xs
inlineToMd ((Link alt href):xs) =
    "[" ++ (inlineToMd alt) ++ "](" ++ href ++ ")" ++ inlineToMd xs
