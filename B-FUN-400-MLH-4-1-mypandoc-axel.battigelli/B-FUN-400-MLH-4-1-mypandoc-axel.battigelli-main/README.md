# myPandoc

## Module Functional

## Language Haskell

## Overview

myPandoc is a lightweight, functional document converter written in Haskell. Inspired by Pandoc, it provides a clean and minimal architecture for converting documents between multiple formats: JSON, Markdown, and XML.

The tool works by translating all supported formats into a common intermediate data structure, then exporting them into the desired output format.

## Conversion Flow

Parse CLI arguments using Arguments.Core

Read and parse the source document using the appropriate module under Input/

Transform content into the Document structure

Generate output using the corresponding module under Output/

## Architecture

The core is the link between an input and a output format. The treatment is complitely released in the submodule. Each element is declare in the Document structure and must be use.

```text
app/
  - Main.hs
src/
  Arguments/
    - Core.hs
  Input/
    - Json.hs
    - Markdown.hs
    - Xml.hs
  Output/
    - Html.hs
    - Json.hs
    - Markdown.hs
    - Xml.hs
    - Yaml.hs
  - Document.hs      # data structure for interchages
```

## Intermediate Structure: Document

The Document module defines a unified, format-agnostic representation of documents.
This allows:
- Safe and consistent transformations
- Easy addition of new formats
- Clear separation between parsing and rendering

It typically includes elements like:
- Headings
- Paragraphs
- Lists
- Code blocks
- Inline formatting
- ...

For a submodule, you must use and implemente all this functionnality :
```
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

```

## Extends and add more input and output

### Input

Your code must start with this type of declaration. The rest depend of your parsing but all the functionnality must be handle. You can't parse tag which haven't a corresponding in the structure.

```
import Document (Document)

parseLangDocument :: String -> Either String Document
parseLangDocument content =
  case Lang.parse content of
    Left err -> Left ("LANG parsing failed: " ++ show err)
    Right langData -> Right (convertToDocument langData)

convertToDocument :: string -> Document
convertToDocument lang = -- Implement the conversion from language to Document
```

You also add in the core, a call to your new file :
```
import Input.Lang (parseLangDocument)

parseDocument :: String -> String -> IO Document
...
parseDocument "lang" content =
  case parseLangDocument content of
    Right d  -> return d
    Left err -> handleError ("LANG parsing error: " ++ err)
```

**Advice:**
It's recommend to having a block and an inline function separatly.
It will easier to read, update and maintain

### Output

Your code must start with this type of declaration. Your have any obligation to treat all the elements from the structure but if it is your choice, ignore it with a `_`.

```
module Output.Lang (documentToLang) where

import Document (Document)

documentToLang :: Document -> String
documentToLang doc = convertToLangString doc

convertToLangString :: Document -> String
convertToLangString doc = -- Implement the conversion from Document to Language string
```

You also add in the core, a call to your new file :
```
import Output.Lang (documentToLang)

renderOutput :: String -> Document -> String
renderOutput fmt doc = case fmt of
...
  "lang"   -> documentToLang doc
  _          -> "Unsupported output format"
```

**Advice:**
It's recommend to having a block and an inline function separatly.
It will easier to read, update and maintain

## Usage

```
./mypandoc -i ifile -f oformat [-o ofile] [-e iformat]
```
