{-
-- EPITECH PROJECT, 2025
-- myPandoc
-- File description:
-- Document.hs
-}

module Document (Document(..),Header(..),Body(..),Block(..),Inline(..)) where

-- Document

data Document = Document
  { header :: Header
  , body   :: Body
  } deriving (Show, Eq)

-- Header

data Header = Header
  { title  :: Maybe String
  , author :: Maybe String
  , date   :: Maybe String
  } deriving (Show, Eq)

-- Body

data Body = Body
  { blocks :: [Block]
  } deriving (Show, Eq)

-- Main content blocks

data Block
  = Paragraph [Inline]                -- A paragraph made of inline elements
  | Section (Maybe String) [Block]    -- Section with optional title and nested blocks
  | CodeBlock (Maybe String) [Block]  -- Optional language, list of code lines
  | List [Block]                      -- A list made of items, each item is a block
  deriving (Show, Eq)

-- Inline content

data Inline
  = PlainText String
  | Emph String                       -- Italic
  | Strong String                     -- Bold
  | CodeInline String                 -- Inline code
  | Link [Inline] String              -- Render text + URL
  | Image [Inline] String             -- Alt text + Src
  deriving (Show, Eq)
