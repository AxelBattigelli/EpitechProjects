{-
-- EPITECH PROJECT, 2025
-- B-FUN-400-MLH-4-1-mypandoc-axel.battigelli
-- File description:
-- Spec
-}

import Arguments.Core
import Document
import Output.Xml
import Output.Markdown
import Output.Json
import Output.Html
import Output.Yaml
import Input.Xml
import Input.Markdown
import Input.Json

import Test.HUnit
import System.Exit
import System.Console.GetOpt
import Data.List

test_usage :: Test
test_usage = TestCase $
  assertEqual "should return text of usage"
    "USAGE: ./mypandoc -i ifile -f oformat [-o ofile] [-e iformat]\n\n  ifile   path to the file to convert\n  oformat output format (xml, json, markdown, html, yaml)\n  ofile   path to the output file\n  iformat input format (xml, json, markdown)\n"
    usage

test_defaultOptions :: Test
test_defaultOptions = TestCase $
  assertEqual "should return deflault options"
    Options {inputFile = "", inputFmt = Nothing, outputFile = Nothing, outputFmt = "", showHelp = False}
    defaultOptions

test_parseArgs_success :: Test
test_parseArgs_success = TestCase $
  assertEqual "should parse valid -i and -f"
    (Right defaultOptions { inputFile  = "in.txt"
                          , outputFmt = "json"
                          }
    )
    (parseArgs ["-i","in.txt","-f","json"])

test_parseArgs_missingRequired :: Test
test_parseArgs_missingRequired = TestCase $
  assertEqual "should error on missing required options"
    (Left "Missing required options: -i and -f")
    (parseArgs [])

test_parseArgs_missingOneOption :: Test
test_parseArgs_missingOneOption = TestCase $
  assertEqual "should error when only one required option provided"
    (Left "Missing required options: -i and -f")
    (parseArgs ["-i","in.txt"])

test_parseArgs_showHelp :: Test
test_parseArgs_showHelp = TestCase $
  assertEqual "should set showHelp when -h is provided"
    (Right defaultOptions { showHelp = True })
    (parseArgs ["-h"])

test_parseArgs_invalidOption :: Test
test_parseArgs_invalidOption = TestCase $
  assertBool "should return Left on invalid options"
    (case parseArgs ["--unknown"] of
       Left err -> "--unknown" `isInfixOf` err
       Right _  -> False)


test_nbOptions :: Test
test_nbOptions = TestCase $
  assertEqual "should return 5 options" 5 (length optionsDescr)

test_option_i :: Test
test_option_i = TestCase $
  assertEqual "should contain -i option"
    (Just True)
    (lookupOption 'i')

test_option_f :: Test
test_option_f = TestCase $
  assertEqual "should contain -f option"
    (Just True)
    (lookupOption 'f')

test_option_o :: Test
test_option_o = TestCase $
  assertEqual "should contain -o option"
    (Just True)
    (lookupOption 'o')

test_option_e :: Test
test_option_e = TestCase $
  assertEqual "should contain -e option"
    (Just True)
    (lookupOption 'e')

test_option_h :: Test
test_option_h = TestCase $
  assertEqual "should contain -h option"
    (Just True)
    (lookupOption 'h')

lookupOption :: Char -> Maybe Bool
lookupOption c =
  let found = any (\(Option shorts _ _ _) -> c `elem` shorts) optionsDescr
  in if found then Just True else Nothing


test_detectFormat_xml :: Test
test_detectFormat_xml = TestCase $
  assertEqual "should detect xml extension"
    (Just "xml")
    (detectFormat "document.xml")

test_detectFormat_json :: Test
test_detectFormat_json = TestCase $
  assertEqual "should detect json extension"
    (Just "json")
    (detectFormat "data.user.export.json")

test_detectFormat_no_ext :: Test
test_detectFormat_no_ext = TestCase $
  assertEqual "should return Nothing on file without extension"
    Nothing
    (detectFormat "filewithoutextension")

test_detectFormat_dot_at_end :: Test
test_detectFormat_dot_at_end = TestCase $
  assertEqual "should return Nothing on filename ending with dot"
    (Just "")
    (detectFormat "filename.")

test_detectFormat_empty :: Test
test_detectFormat_empty = TestCase $
  assertEqual "should return Nothing on empty input"
    Nothing
    (detectFormat "")

test_inputFmt_defined :: Test
test_inputFmt_defined = TestCase $
  assertEqual "should return explicitly set inputFmt"
    "xml"
    (determineInputFormat (Options "file.json" (Just "xml") Nothing "markdown" False) "")

test_inputFmt_detected :: Test
test_inputFmt_detected = TestCase $
  assertEqual "should return detected format from file extension"
    "json"
    (determineInputFormat (Options "data.json" Nothing Nothing "markdown" False) "")

test_inputFmt_unknown :: Test
test_inputFmt_unknown = TestCase $
  assertEqual "should return 'unknown'"
    "unknown"
    (determineInputFormat (Options "nofile" Nothing Nothing "markdown" False) "")

test_options_eq :: Test
test_options_eq = TestCase $
  let opt1 = Options "input.txt" (Just "json") (Just "output.txt") "markdown" False
      opt2 = Options "input.txt" (Just "json") (Just "output.txt") "markdown" False
  in assertEqual "Options should be equal" opt1 opt2

test_options_not_eq :: Test
test_options_not_eq = TestCase $
  let opt1 = Options "input1.txt" (Just "json") Nothing "xml" False
      opt2 = Options "input2.txt" (Just "json") Nothing "xml" False
  in assertBool "Options should not be equal" (opt1 /= opt2)

test_options_show :: Test
test_options_show = TestCase $
  let shown = show (Options "in.txt" (Just "xml") (Just "out.txt") "json" True)
  in assertBool "Show should include field values"
       ("in.txt" `isInfixOf` shown &&
        "xml"    `isInfixOf` shown &&
        "out.txt"`isInfixOf` shown &&
        "json"   `isInfixOf` shown &&
        "True"   `isInfixOf` shown)

test_document_eq :: Test
test_document_eq = TestCase $
  let d1 = Document (Header (Just "T") (Just "A") (Just "2024"))
                    (Body [Paragraph [PlainText "Hello"]])
      d2 = Document (Header (Just "T") (Just "A") (Just "2024"))
                    (Body [Paragraph [PlainText "Hello"]])
  in assertEqual "Equal documents" d1 d2

test_document_not_eq :: Test
test_document_not_eq = TestCase $
  let d1 = Document (Header (Just "T") Nothing Nothing)
                    (Body [Paragraph [PlainText "Hi"]])
      d2 = Document (Header (Just "T") Nothing Nothing)
                    (Body [Paragraph [PlainText "Different"]])
  in assertBool "Different documents should not be equal" (d1 /= d2)

test_document_show :: Test
test_document_show = TestCase $
  let doc = Document (Header (Just "Title") (Just "Author") (Just "2024"))
                     (Body [Section (Just "Intro") [Paragraph [Strong "Bold"]]])
      s = show doc
  in assertBool "Show should contain key content"
       ("Title" `isInfixOf` s &&
        "Author" `isInfixOf` s &&
        "Intro" `isInfixOf` s &&
        "Bold" `isInfixOf` s)

test_inline_eq :: Test
test_inline_eq = TestCase $
  assertEqual "Equal PlainText" (PlainText "hello") (PlainText "hello")

test_inline_not_eq :: Test
test_inline_not_eq = TestCase $
  assertBool "Different Emph and Strong"
    (Emph "text" /= Strong "text")

test_block_eq :: Test
test_block_eq = TestCase $
  assertEqual "Same Paragraphs"
    (Paragraph [PlainText "Hello"])
    (Paragraph [PlainText "Hello"])

test_block_not_eq :: Test
test_block_not_eq = TestCase $
  assertBool "Different types should not be equal"
    (Paragraph [PlainText "Hi"] /= CodeBlock Nothing [])

test_body_eq :: Test
test_body_eq = TestCase $
  let b1 = Body [Paragraph [PlainText "Hello"]]
      b2 = Body [Paragraph [PlainText "Hello"]]
  in assertEqual "Bodies should be equal" b1 b2

test_body_not_eq :: Test
test_body_not_eq = TestCase $
  let b1 = Body [Paragraph [PlainText "Hello"]]
      b2 = Body [Paragraph [PlainText "World"]]
  in assertBool "Bodies should not be equal" (b1 /= b2)

test_body_show :: Test
test_body_show = TestCase $
  let body1 = Body [Paragraph [Strong "Bold"]]
      s = show body1
  in assertBool "Show should contain content"
       ("Bold" `isInfixOf` s)

test_header_eq :: Test
test_header_eq = TestCase $
  let h1 = Header (Just "My Title") (Just "Valentin") (Just "2024-04-29")
      h2 = Header (Just "My Title") (Just "Valentin") (Just "2024-04-29")
  in assertEqual "Headers should be equal" h1 h2

test_header_not_eq :: Test
test_header_not_eq = TestCase $
  let h1 = Header (Just "Title") (Just "Valentin") Nothing
      h2 = Header (Just "Title") (Just "Axel") Nothing
  in assertBool "Headers should not be equal" (h1 /= h2)

test_header_show :: Test
test_header_show = TestCase $
  let h = Header (Just "Doc") (Just "Author") (Just "2025")
      s = show h
  in assertBool "Show should contain all fields"
       ("Doc" `isInfixOf` s &&
        "Author" `isInfixOf` s &&
        "2025" `isInfixOf` s)

test_indent_0 :: Test
test_indent_0 = TestCase $
  assertEqual "indent 0 should be empty" "" (indent 0)

test_indent_1 :: Test
test_indent_1 = TestCase $
  assertEqual "indent 1 should be 4 spaces" "    " (indent 1)

test_indent_2 :: Test
test_indent_2 = TestCase $
  assertEqual "indent 2 should be 8 spaces" "        " (indent 2)

test_inline_plain :: Test
test_inline_plain = TestCase $
  assertEqual "PlainText only"
    "hello world"
    (inlineToXml [PlainText "hello", PlainText " world"])

test_inline_emph :: Test
test_inline_emph = TestCase $
  assertEqual "Emph text"
    "<italic>important</italic>"
    (inlineToXml [Emph "important"])

test_inline_mixed :: Test
test_inline_mixed = TestCase $
  assertEqual "Mixed inline"
    "A <bold>test</bold> <italic>case</italic>."
    (inlineToXml [PlainText "A ", Strong "test", PlainText " ", Emph "case", PlainText "."])

test_inline_link :: Test
test_inline_link = TestCase $
  assertEqual "Link with text"
    "<link url=\"https://x.com\">click</link>"
    (inlineToXml [Link [PlainText "click"] "https://x.com"])

test_inline_image :: Test
test_inline_image = TestCase $
  assertEqual "Image with alt text"
    "<image url=\"img.png\">icon</image>"
    (inlineToXml [Image [PlainText "icon"] "img.png"])

test_inline_code :: Test
test_inline_code = TestCase $
  assertEqual "Inline code"
    "<code>print()</code>"
    (inlineToXml [CodeInline "print()"])


test_block_paragraph :: Test
test_block_paragraph = TestCase $
  assertEqual "Simple paragraph"
    "    <paragraph>Hello</paragraph>\n"
    (blockToXml 1 [Paragraph [PlainText "Hello"]])

test_block_section :: Test
test_block_section = TestCase $
  assertEqual "Section with title"
    "    <section title=\"Intro\">\n        <paragraph>Hi</paragraph>\n    </section>\n"
    (blockToXml 1 [Section (Just "Intro") [Paragraph [PlainText "Hi"]]])

test_block_codeblock :: Test
test_block_codeblock = TestCase $
  assertEqual "CodeBlock with language"
    "    <codeblock language=\"haskell\">\n        <paragraph>print()</paragraph>\n    </codeblock>\n"
    (blockToXml 1 [CodeBlock (Just "haskell") [Paragraph [PlainText "print()"]]])

test_block_list :: Test
test_block_list = TestCase $
  assertEqual "List of paragraphs"
    "    <list>\n        <paragraph>One</paragraph>\n        <paragraph>Two</paragraph>\n    </list>\n"
    (blockToXml 1 [List [Paragraph [PlainText "One"], Paragraph [PlainText "Two"]]])

test_block_section_empty_title :: Test
test_block_section_empty_title = TestCase $
  assertEqual "Section with no title"
    "    <section title=\"\">\n        <paragraph>Empty</paragraph>\n    </section>\n"
    (blockToXml 1 [Section Nothing [Paragraph [PlainText "Empty"]]])

test_block_codeblock_nolang :: Test
test_block_codeblock_nolang = TestCase $
  assertEqual "CodeBlock without language"
    "    <codeblock>\n        <paragraph>no lang</paragraph>\n    </codeblock>\n"
    (blockToXml 1 [CodeBlock Nothing [Paragraph [PlainText "no lang"]]])

test_bodyXml_paragraph :: Test
test_bodyXml_paragraph = TestCase $
  let b = Body [Paragraph [PlainText "Hello world"]]
      expected =
          "    <body>\n" ++
          "        <paragraph>Hello world</paragraph>\n" ++
          "    </body>\n"
  in assertEqual "bodyXml should generate XML with body and paragraph" expected (bodyXml b)

test_headerXml :: Test
test_headerXml = TestCase $
  let h = Header (Just "My Doc") (Just "Valentin") (Just "2025")
      expected =
          "    <header title=\"My Doc\">\n" ++
          "        <author>Valentin</author>\n" ++
          "        <date>2025</date>\n" ++
          "    </header>\n"
  in assertEqual "Full header" expected (headerXml h)

test_headerXml_only_date :: Test
test_headerXml_only_date = TestCase $
  let h = Header Nothing Nothing (Just "2024")
      expected =
          "    <header title=\"\">\n" ++
          "        <date>2024</date>\n" ++
          "    </header>\n"
  in assertEqual "Header with only date" expected (headerXml h)

test_headerXml_only_title :: Test
test_headerXml_only_title = TestCase $
  let h = Header (Just "Title Only") Nothing Nothing
      expected =
          "    <header title=\"Title Only\">\n" ++
          "    </header>\n"
  in assertEqual "Header with only title" expected (headerXml h)

test_documentToXml :: Test
test_documentToXml = TestCase $
  let doc = Document
              (Header (Just "T") (Just "A") (Just "2025"))
              (Body [Paragraph [PlainText "Hello"]])
      expected =
        "<document>\n" ++
        "    <header title=\"T\">\n" ++
        "        <author>A</author>\n" ++
        "        <date>2025</date>\n" ++
        "    </header>\n" ++
        "    <body>\n" ++
        "        <paragraph>Hello</paragraph>\n" ++
        "    </body>\n" ++
        "</document>"
  in assertEqual "documentToXml test all function" expected (documentToXml doc)

test_md_plain :: Test
test_md_plain = TestCase $
  assertEqual "PlainText only"
    "hello world"
    (inlineToMd [PlainText "hello", PlainText " world"])

test_md_emph :: Test
test_md_emph = TestCase $
  assertEqual "Emphasized"
    "*important*"
    (inlineToMd [Emph "important"])

test_md_strong :: Test
test_md_strong = TestCase $
  assertEqual "Strong text"
    "**bold**"
    (inlineToMd [Strong "bold"])

test_md_code :: Test
test_md_code = TestCase $
  assertEqual "code"
    "`print()`"
    (inlineToMd [CodeInline "print()"])

test_md_link :: Test
test_md_link = TestCase $
  assertEqual "Link"
    "[click](https://x.com)"
    (inlineToMd [Link [PlainText "click"] "https://x.com"])

test_md_image :: Test
test_md_image = TestCase $
  assertEqual "Image with alt"
    "![logo](logo.png)"
    (inlineToMd [Image [PlainText "logo"] "logo.png"])

test_md_mixed :: Test
test_md_mixed = TestCase $
  assertEqual "Mixed inline content"
    "Say *hi* to **everyone** at `[Home](/home)`."
    (inlineToMd [PlainText "Say "
                , Emph "hi"
                , PlainText " to "
                , Strong "everyone"
                , PlainText " at `"
                , Link [PlainText "Home"] "/home"
                , PlainText "`."
                ])

test_md_paragraph :: Test
test_md_paragraph = TestCase $
  assertEqual "Simple paragraph"
    "Hello World"
    (blockToMd "\n" 0 [Paragraph [PlainText "Hello", PlainText " World"]])

test_md_section :: Test
test_md_section = TestCase $
  assertEqual "Section with title"
    "# Intro\n\nContent"
    (blockToMd "\n" 0 [Section (Just "Intro") [Paragraph [PlainText "Content"]]])

test_md_nested_section :: Test
test_md_nested_section = TestCase $
  assertEqual "Nested section"
    "# Title\n\n## Sub\n\nSub content"
    (blockToMd "\n" 0 [Section (Just "Title") [Section (Just "Sub") [Paragraph [PlainText "Sub content"]]]])

test_md_codeblock_lang :: Test
test_md_codeblock_lang = TestCase $
  assertEqual "Code block with language"
    "```haskell\nprint()\n```"
    (blockToMd "\n" 0 [CodeBlock (Just "haskell") [Paragraph [PlainText "print()"]]])

test_md_codeblock_nolang :: Test
test_md_codeblock_nolang = TestCase $
  assertEqual "Code block no language"
    "```\necho hi\n```"
    (blockToMd "\n" 0 [CodeBlock Nothing [Paragraph [PlainText "echo hi"]]])

test_md_list :: Test
test_md_list = TestCase $
  assertEqual "List of 2 items"
    "- Item 1\n- Item 2"
    (blockToMd "\n" 0 [List [Paragraph [PlainText "Item 1"], Paragraph [PlainText "Item 2"]]])

test_md_section_empty_title :: Test
test_md_section_empty_title = TestCase $
  assertEqual "Section with empty title"
    "hello content"
    (blockToMd "\n" 0 [Section (Just "") [Paragraph [PlainText "hello content"]]])

test_md_section_nothing :: Test
test_md_section_nothing = TestCase $
  assertEqual "Section Nothing"
    "hello world"
    (blockToMd "\n" 0 [Section Nothing [Paragraph [PlainText "hello world"]]])

test_documentToMarkdown :: Test
test_documentToMarkdown = TestCase $
  let doc = Document
        (Header (Just "Doc") (Just "Paul") (Just "2025"))
        (Body [Paragraph [PlainText "Hello"], Section (Just "Intro") [Paragraph [Strong "Yes"]]])
      expected = "---\ntitle: Doc\nauthor: Paul\ndate: 2025\n---\n\nHello\n\n# Intro\n\n**Yes**"
  in assertEqual "documentToMarkdown full output" expected (documentToMarkdown doc)

test_stringOrEmpty_nothing :: Test
test_stringOrEmpty_nothing = TestCase $
  assertEqual "Nothing should give \"\""
    "\"\""
    (stringOrEmpty Nothing)

test_stringOrEmpty_just :: Test
test_stringOrEmpty_just = TestCase $
  assertEqual "Just hello should give hello with \""
    "\"hello\""
    (stringOrEmpty (Just "hello"))

test_stringOrEmpty_emptyString :: Test
test_stringOrEmpty_emptyString = TestCase $
  assertEqual "Just \"\" should give hello with \""
    "\"\""
    (stringOrEmpty (Just ""))

test_list_empty :: Test
test_list_empty = TestCase $
  assertEqual "Empty list"
    ""
    (inlineListJson 1 [])

test_list_single :: Test
test_list_single = TestCase $
  assertEqual "Single element"
    "    \"hello\""
    (inlineListJson 1 [PlainText "hello"])

test_list_multiple :: Test
test_list_multiple = TestCase $
  let expected = "    \"x\",\n    {\n        \"italic\": \"y\"\n    }"
  in assertEqual "Two elements"
       expected
       (inlineListJson 1 [PlainText "x", Emph "y"])

test_inlineToJson_plainText :: Test
test_inlineToJson_plainText = TestCase $
  assertEqual "PlainText Json ouput"
    "\"hello\""
    (inlineToJson 0 (PlainText "hello"))

test_inlineToJson_emph :: Test
test_inlineToJson_emph = TestCase $
  let expected = "{\n    \"italic\": \"italic text\"\n}"
  in assertEqual "Emph Json ouput" expected (inlineToJson 0 (Emph "italic text"))

test_inlineToJson_strong :: Test
test_inlineToJson_strong = TestCase $
  let expected = "{\n    \"bold\": \"bold text\"\n}"
  in assertEqual "Strong Json ouput" expected (inlineToJson 0 (Strong "bold text"))

test_inlineToJson_code :: Test
test_inlineToJson_code = TestCase $
  let expected = "{\n    \"code\": \"x = 42\"\n}"
  in assertEqual "CodeInline Json ouput" expected (inlineToJson 0 (CodeInline "x = 42"))

test_inlineToJson_image :: Test
test_inlineToJson_image = TestCase $
  let val = Image [PlainText "logo"] "img.png"
      expected = "{\n" ++
                 "    \"image\": {\n" ++
                 "        \"url\": \"img.png\",\n" ++
                 "        \"alt\": [\n" ++
                 "            \"logo\"\n" ++
                 "        ]\n" ++
                 "    }\n" ++
                 "}"
  in assertEqual "Image Json ouput" expected (inlineToJson 0 val)

test_inlineToJson_link :: Test
test_inlineToJson_link = TestCase $
  let val = Link [PlainText "Trello"] "https://trello.com/"
      expected = "{\n" ++
                 "    \"link\": {\n" ++
                 "        \"url\": \"https://trello.com/\",\n" ++
                 "        \"content\": [\n" ++
                 "            \"Trello\"\n" ++
                 "        ]\n" ++
                 "    }\n" ++
                 "}"
  in assertEqual "Link Json ouput" expected (inlineToJson 0 val)

test_blockList_empty :: Test
test_blockList_empty = TestCase $
  assertEqual "Empty block list" "" (blockListJson 1 [])

test_blockList_single :: Test
test_blockList_single = TestCase $
  let expected = "    [\n        \"hello\"\n    ]"
  in assertEqual "Single block" expected (blockListJson 1 [Paragraph [PlainText "hello"]])

test_blockList_multiple :: Test
test_blockList_multiple = TestCase $
  let bs = [Paragraph [PlainText "a"], Paragraph [PlainText "b"]]
      expected = "    [\n        \"a\"\n    ],\n    [\n        \"b\"\n    ]"
  in assertEqual "Two blocks" expected (blockListJson 1 bs)

test_blockToJson_section :: Test
test_blockToJson_section = TestCase $
  let b = Section (Just "Intro") [Paragraph [PlainText "Text"]]
      expected = "{\n    \"section\": {\n        \"title\": \"Intro\",\n        \"content\": [\n            [\n                \"Text\"\n            ]\n        ]\n    }\n}"
  in assertEqual "Section to JSON" expected (blockToJson 0 b)

test_blockToJson_codeblock :: Test
test_blockToJson_codeblock = TestCase $
  let b = CodeBlock Nothing [Paragraph [PlainText "code"]]
      expected = "{\n    \"codeblock\": [\n        [\n            \"code\"\n        ]\n    ]\n}"
  in assertEqual "CodeBlock to JSON" expected (blockToJson 0 b)

test_blockToJson_list :: Test
test_blockToJson_list = TestCase $
  let b = List [Paragraph [PlainText "Item 1"], Paragraph [PlainText "Item 2"]]
      expected = "{\n    \"list\": [\n        [\n            \"Item 1\"\n        ],\n        [\n            \"Item 2\"\n        ]\n    ]\n}"
  in assertEqual "List to JSON" expected (blockToJson 0 b)

test_header_full :: Test
test_header_full = TestCase $
  let h = Header (Just "My Title") (Just "Alice") (Just "2025-04-29")
      expected = unlines
        [ "    \"header\": {"
        , "        \"title\": \"My Title\","
        , "        \"author\": \"Alice\","
        , "        \"date\": \"2025-04-29\""
        , "    },"
        ]
  in assertEqual "Header full" expected (headerToJson h)

test_header_partial :: Test
test_header_partial = TestCase $
  let h = Header Nothing (Just "Bob") Nothing
      expected = unlines
        [ "    \"header\": {"
        , "        \"author\": \"Bob\","
        , "    },"
        ]
  in assertEqual "Header partial" expected (headerToJson h)

test_header_empty :: Test
test_header_empty = TestCase $
  let h = Header Nothing Nothing Nothing
      expected = "    \"header\": {\n    },\n"
  in assertEqual "Header empty" expected (headerToJson h)


test_documentToJson :: Test
test_documentToJson = TestCase $
  let doc = Document
              (Header (Just "Title") (Just "Valentin") (Just "2025"))
              (Body [Paragraph [PlainText "Bonjour"]])
      expected = "{\n    \"header\": {\n        \"title\": \"Title\",\n        \"author\": \"Valentin\",\n        \"date\": \"2025\"\n    },\n    \"body\": [\n        [\n            \"Bonjour\"\n        ]\n    ]\n}"
  in assertEqual "Full document JSON" expected (documentToJson doc)

test_inlineToHtml_plain :: Test
test_inlineToHtml_plain = TestCase $
  assertEqual "Plain text html ouptu"
    "Hello world"
    (inlineToHtml [PlainText "Hello", PlainText " world"])

test_inlineToHtml_emph :: Test
test_inlineToHtml_emph = TestCase $
  assertEqual "Emphasized text html ouptu"
    "<i>important</i>!"
    (inlineToHtml [Emph "important", PlainText "!"])

test_inlineToHtml_strong :: Test
test_inlineToHtml_strong = TestCase $
  assertEqual "Bold text html ouptu"
    "This is <strong>bold</strong>."
    (inlineToHtml [PlainText "This is ", Strong "bold", PlainText "."])

test_inlineToHtml_code :: Test
test_inlineToHtml_code = TestCase $
  assertEqual "Code inline html ouptu"
    "Run <code>npm start</code>"
    (inlineToHtml [PlainText "Run ", CodeInline "npm start"])

test_inlineToHtml_link :: Test
test_inlineToHtml_link = TestCase $
  assertEqual "Link html ouptu"
    "<a href=\"https://example.com\">Example</a>"
    (inlineToHtml [Link [PlainText "Example"] "https://example.com"])

test_inlineToHtml_image :: Test
test_inlineToHtml_image = TestCase $
  assertEqual "Image html ouptu"
    "<img src=\"logo.png\" alt=\"Logo\"></img>"
    (inlineToHtml [Image [PlainText "Logo"] "logo.png"])

test_blockToHtml_paragraph :: Test
test_blockToHtml_paragraph = TestCase $
  let input = [Paragraph [PlainText "Hello"]]
      expected = "    <p>Hello</p>\n"
  in assertEqual "Paragraph html ouput" expected (blockToHtml 1 1 input)

test_blockToHtml_section :: Test
test_blockToHtml_section = TestCase $
  let input = [Section (Just "Intro") [Paragraph [PlainText "Text"]]]
      expected = "    <h1>Intro</h1>\n    <p>Text</p>\n"
  in assertEqual "Section html ouput" expected (blockToHtml 1 1 input)

test_blockToHtml_code :: Test
test_blockToHtml_code = TestCase $
  let input = [CodeBlock (Just "hs") [Paragraph [PlainText "putStrLn \"Hello\""]]]
      expected =
        "    <pre><code class=\"hs\">\n" ++
        "    <p>putStrLn \"Hello\"</p>\n" ++
        "    </code></pre>\n"
  in assertEqual "Code block html ouput" expected (blockToHtml 1 1 input)

test_blockToHtml_list :: Test
test_blockToHtml_list = TestCase $
  let input = [List [Paragraph [PlainText "Item 1"], Paragraph [PlainText "Item 2"]]]
      expected =
        "    <ul>\n" ++
        "        <p>Item 1</p>\n" ++
        "        <p>Item 2</p>\n" ++
        "    </ul>\n"
  in assertEqual "List html ouput" expected (blockToHtml 1 1 input)

test_blockToHtml_section_nothing :: Test
test_blockToHtml_section_nothing = TestCase $
  let input =
        [ Section Nothing
            [ Paragraph [PlainText "Hello"]
            , Section (Just "World") [Paragraph [PlainText "all"]]
            ]
        ]
      expected = "    <p>Hello</p>\n    <h2>World</h2>\n    <p>all</p>\n"
  in assertEqual "Empty section html ouput" expected (blockToHtml 1 1 input)

test_html_codeblock_no_lang :: Test
test_html_codeblock_no_lang = TestCase $
  let input = [CodeBlock Nothing [Paragraph [PlainText "raw code"]]]
      expected =
        "    <pre><code>\n" ++
        "        <p>raw code</p>\n" ++
        "    </code></pre>\n"
  in assertEqual "CodeBlock without language html ouput" expected (blockToHtml 1 1 input)


test_bodyHtml :: Test
test_bodyHtml = TestCase $
  let b = Body [Paragraph [PlainText "Hello world"]]
      expected =
        "    <body>\n" ++
        "        <p>Hello world</p>\n" ++
        "    </body>\n"
  in assertEqual "bodyHtml basic" expected (bodyHtml b)

test_full_headerHtml :: Test
test_full_headerHtml = TestCase $
  let hdr = Header (Just "My Doc") (Just "Valentin") (Just "2025-04-29")
      expected = "    <head>\n        <meta charset='utf-8' />\n        <title>Valentin</title>\n        <meta name=\"author\" content=\"Valentin\" />\n        <meta name=\"date\" content=\"2025-04-29\" />\n        <meta name='viewport' content='width=device-width, initial-scale=1' />\n    </head>\n"
  in assertEqual "Full header" expected (headerHtml hdr)

test_partial_headerHtml :: Test
test_partial_headerHtml = TestCase $
  let hdr = Header Nothing (Just "Valentin") Nothing
      expected = "    <head>\n        <meta charset='utf-8' />\n        <title>Valentin</title>\n        <meta name=\"author\" content=\"Valentin\" />\n        <meta name='viewport' content='width=device-width, initial-scale=1' />\n    </head>\n"
  in assertEqual "Partial header" expected (headerHtml hdr)

test_empty_headerHtml :: Test
test_empty_headerHtml = TestCase $
  let hdr = Header Nothing Nothing Nothing
      expected =
        "    <head>\n" ++
        "        <meta charset='utf-8' />\n" ++
        "        <meta name='viewport' content='width=device-width, initial-scale=1' />\n" ++
        "    </head>\n"
  in assertEqual "Empty header" expected (headerHtml hdr)

test_documentToHtml :: Test
test_documentToHtml = TestCase $
  let doc = Document
              (Header (Just "Titre") (Just "Valentin") (Just "2025-04-29"))
              (Body [Paragraph [PlainText "Bonjour le monde"]])
      expected = "<!DOCTYPE html>\n<html>\n    <head>\n        <meta charset='utf-8' />\n        <title>Valentin</title>\n        <meta name=\"author\" content=\"Valentin\" />\n        <meta name=\"date\" content=\"2025-04-29\" />\n        <meta name='viewport' content='width=device-width, initial-scale=1' />\n    </head>\n    <body>\n        <p>Bonjour le monde</p>\n    </body>\n</html>"
  in assertEqual "Full HTML document" expected (documentToHtml doc)

test_inlineListYaml :: Test
test_inlineListYaml = TestCase $
  let inlines =
        [ PlainText "hello", Emph "italic", Strong "bold", CodeInline "code"]
      expected = "- \"hello\"\n- italic: \"italic\"\n- bold: \"bold\"\n- code: \"code\""
  in assertEqual "YAML inline list" expected (inlineListYaml 0 inlines)

test_inlineToYaml_linkYaml :: Test
test_inlineToYaml_linkYaml = TestCase $
  let val = Link [PlainText "Example"] "https://example.com"
      expected = "- link:\n  url: \"https://example.com\"\n  content:\n    - \"Example\""
  in assertEqual "YAML Link" expected (inlineToYaml 0 val)

test_inlineToYaml_imageYaml :: Test
test_inlineToYaml_imageYaml = TestCase $
  let val = Image [PlainText "Logo"] "logo.png"
      expected = "- image:\n  url: \"logo.png\"\n  alt:\n    - \"Logo\""
  in assertEqual "YAML Image" expected (inlineToYaml 0 val)

test_blockListYaml_paragraphs :: Test
test_blockListYaml_paragraphs = TestCase $
  let blocks = [Paragraph [PlainText "Hello"], Paragraph [PlainText "World"]]
      expected = "- paragraph:\n  - \"Hello\"\n- paragraph:\n  - \"World\""
  in assertEqual "Two paragraphs yaml" expected (blockListYaml 0 blocks)

test_blockListYaml_empty :: Test
test_blockListYaml_empty = TestCase $
  assertEqual "Empty list" "" (blockListYaml 0 [])

test_block_section_yaml :: Test
test_block_section_yaml = TestCase $
  let block = Section (Just "Intro") [Paragraph [PlainText "Text"]]
      expected = "- section:\n    title: \"Intro\"\n    content:\n      - paragraph:\n        - \"Text\""
  in assertEqual "Section YAML" expected (blockToYaml 0 block)

test_block_section_none :: Test
test_block_section_none = TestCase $
  let block = Section Nothing [Paragraph [PlainText "Anonymous"]]
      expected = "- section:\n    title: \"\"\n    content:\n      - paragraph:\n        - \"Anonymous\""
  in assertEqual "Section with no title yaml" expected (blockToYaml 0 block)

test_block_code :: Test
test_block_code = TestCase $
  let block = CodeBlock Nothing [Paragraph [PlainText "code"]]
      expected = "- codeblock:\n  - paragraph:\n    - \"code\""
  in assertEqual "CodeBlock YAML" expected (blockToYaml 0 block)

test_block_list_yaml :: Test
test_block_list_yaml = TestCase $
  let block = List [Paragraph [PlainText "Item 1"], Paragraph [PlainText "Item 2"]]
      expected = "- list:\n  - paragraph:\n    - \"Item 1\"\n  - paragraph:\n    - \"Item 2\""
  in assertEqual "List YAML" expected (blockToYaml 0 block)

test_header_full_yaml :: Test
test_header_full_yaml = TestCase $
  let hdr = Header (Just "Title") (Just "Valentin") (Just "2025-04-29")
      expected = unlines
        [ "  header:", "    title: \"Title\""
        , "    author: \"Valentin\"", "    date: \"2025-04-29\""]
  in assertEqual "Full header YAML" expected (headerToYaml hdr)

test_header_partial_yaml :: Test
test_header_partial_yaml = TestCase $
  let hdr = Header Nothing (Just "Valentin") Nothing
      expected = unlines
        [ "  header:" , "    author: \"Valentin\""]
  in assertEqual "Partial header YAML" expected (headerToYaml hdr)

test_header_empty_yaml :: Test
test_header_empty_yaml = TestCase $
  let hdr = Header Nothing Nothing Nothing
      expected = "  header:\n"
  in assertEqual "Empty header YAML" expected (headerToYaml hdr)

test_documentToYaml :: Test
test_documentToYaml = TestCase $
  let doc = Document
              (Header (Just "Titre") (Just "Valentin") (Just "2025-04-30"))
              (Body [Paragraph [PlainText "Bonjour"]])
      expected = "document:\n  header:\n    title: \"Titre\"\n    author: \"Valentin\"\n    date: \"2025-04-30\"\n  body:\n    - paragraph:\n      - \"Bonjour\""
  in assertEqual "Full document YAML" expected (documentToYaml doc)

test_inlineListYaml_empty :: Test
test_inlineListYaml_empty = TestCase $
  assertEqual "Empty inline list yaml" "" (inlineListYaml 0 [])

test_parse_valid_document :: Test
test_parse_valid_document = TestCase $
  assertEqual "should parse minimal valid document"
    (Right (Document {header = Header {title = Just "Syntaxe XML", author = Nothing, date = Nothing}, body = Body {blocks = []}}))
    (parseXmlDocument "<document><header title=\"Syntaxe XML\"></header><body></body></document>")

test_parse_invalid_document :: Test
test_parse_invalid_document = TestCase $
  assertEqual "should fail with missing <document>"
    (Left "Tag <document> not found.")
    (parseXmlDocument "<invalid></invalid>")

test_parse_bad_document :: Test
test_parse_bad_document = TestCase $
  assertEqual "should missing <document>"
    (Left "Tag <document> not found.")
    (parseXmlDocument "")

test_parse_valid_header :: Test
test_parse_valid_header = TestCase $
  assertEqual "should parse header document valid"
    (Right (Document {header = Header {title = Just "Syntaxe XML", author = Just "Fornes Leo", date = Just "2024-01-01"}, body = Body {blocks = []}}))
    (parseXmlDocument "<document><header title=\"Syntaxe XML\"><author>Fornes Leo</author><date>2024-01-01</date></header><body></body></document>")

test_parse_bad_header :: Test
test_parse_bad_header = TestCase $
  assertEqual "should parse header document bad "
    (Left "Tag <author> not found.")
    (parseXmlDocument "<document><header title=\"Syntaxe XML\"><author>Fornes Leo<date>2024-01-01</date></header><body></body></document>")

test_parse_body_paragphe :: Test
test_parse_body_paragphe = TestCase $
  assertEqual "should parse body paragphe"
    (Right (Document {header = Header {title = Nothing, author = Nothing, date = Nothing}, body = Body {blocks = [Paragraph [PlainText "This is a paragraph with ",Strong "bold",PlainText ", ",Emph "italic",PlainText " and ",CodeInline "code",PlainText " text."]]}}))
    (parseXmlDocument "<document><header></header><body><paragraph>This is a paragraph with <bold>bold</bold>, <italic>italic</italic> and <code>code</code> text.</paragraph></body></document>")

test_parse_body_paragphe_link :: Test
test_parse_body_paragphe_link = TestCase $
  assertEqual "should parse body paragphe link"
    (Right (Document {header = Header {title = Nothing, author = Nothing, date = Nothing}, body = Body {blocks = [Paragraph [PlainText "This is a paragraph with a ",Link [PlainText "link"] "https://www.youtube.com/watch?v=dQw4w9WgXcQ&ab_channel=RickAstley",PlainText "."]]}}))
    (parseXmlDocument "<document><header></header><body><paragraph>This is a paragraph with a <link url=\"https://www.youtube.com/watch?v=dQw4w9WgXcQ&ab_channel=RickAstley\">link</link>.</paragraph></body></document>")

test_parse_body_paragphe_image :: Test
test_parse_body_paragphe_image = TestCase $
  assertEqual "should parse body paragphe image"
    (Right (Document {header = Header {title = Nothing, author = Nothing, date = Nothing}, body = Body {blocks = [Paragraph [PlainText "This is a paragraph with an image",Image [PlainText "Text to replace image"] "https://cdn-images-1.medium.com/max/697/1*tsHrUKwQXG1YZX0l957ISw.png",PlainText "."]]}}))
    (parseXmlDocument "<document><header></header><body><paragraph>This is a paragraph with an image<image url=\"https://cdn-images-1.medium.com/max/697/1*tsHrUKwQXG1YZX0l957ISw.png\">Text to replace image</image>.</paragraph></body></document>")

test_parse_body_section :: Test
test_parse_body_section = TestCase $
  assertEqual "should parse body section"
    (Right (Document {header = Header {title = Nothing, author = Nothing, date = Nothing}, body = Body {blocks = [Section (Just "header 1") [CodeBlock Nothing [Paragraph [PlainText "This is a code block."]],List [Paragraph [PlainText "list item 1"],Paragraph [PlainText "list item 2"],Paragraph [PlainText "list item 3"]]]]}}))
    (parseXmlDocument ("<document><header></header><body>" ++
    "<section title=\"header 1\">" ++ "<codeblock>"
    ++ "<paragraph>This is a code block.</paragraph>" ++
    "</codeblock>" ++
    "<list>" ++
    "<paragraph>list item 1</paragraph>" ++
    "<paragraph>list item 2</paragraph>" ++
    "<paragraph>list item 3</paragraph>" ++
    "</list></section>" ++ "</body></document>"))

test_parse_valid_header_md :: Test
test_parse_valid_header_md = TestCase $
  assertEqual "should parse valid header md"
    (Right (Document {header = Header {title = Just "Syntaxe MARKDOWN", author = Just "Fornes Leo", date = Just "2024-01-01"}, body = Body {blocks = []}}))
    (parseMarkdownDocument "---\ntitle: Syntaxe MARKDOWN\nauthor: Fornes Leo\ndate: 2024-01-01\n---")

test_parse_bad_header_md :: Test
test_parse_bad_header_md = TestCase $
  assertEqual "should parse header document bad "
    (Right (Document {header = Header {title = Just "Syntaxe MARKDOWN", author = Nothing, date = Just "2024-01-01"}, body = Body {blocks = []}}))
    (parseMarkdownDocument "---\ntitle: Syntaxe MARKDOWN\nautr: Fornes Leo\ndate: 2024-01-01\n---")

test_parse_body_paragphe_md :: Test
test_parse_body_paragphe_md = TestCase $
  assertEqual "should parse body paragphe md"
    (Right (Document {header = Header {title = Just "Syntaxe MARKDOWN", author = Nothing, date = Nothing}, body = Body {blocks = [Paragraph [PlainText "This is a paragraph with ",Strong "bold",PlainText ", ",Emph "italic",PlainText " and ",CodeInline "code",PlainText " text."]]}}))
    (parseMarkdownDocument "---\ntitle: Syntaxe MARKDOWN\n---\n\nThis is a paragraph with **bold**, *italic* and `code` text.")

test_parse_body_paragphe_link_md :: Test
test_parse_body_paragphe_link_md = TestCase $
  assertEqual "should parse body paragphe link md"
    (Right (Document {header = Header {title = Just "Syntaxe MARKDOWN", author = Nothing, date = Nothing}, body = Body {blocks = [Paragraph [PlainText "This is a paragraph with a ",Link [PlainText "link"] "https://www.youtube.com/watch?v=dQw4w9WgXcQ&ab_channel=RickAstley",PlainText "."]]}}))
    (parseMarkdownDocument "---\ntitle: Syntaxe MARKDOWN\n---\n\nThis is a paragraph with a [link](https://www.youtube.com/watch?v=dQw4w9WgXcQ&ab_channel=RickAstley).")

test_parse_body_paragphe_image_md :: Test
test_parse_body_paragphe_image_md = TestCase $
  assertEqual "should parse body paragphe image md"
    (Right (Document {header = Header {title = Just "Syntaxe MARKDOWN", author = Nothing, date = Nothing}, body = Body {blocks = [Paragraph [PlainText "This is a paragraph with an image ",Image [PlainText "Text to replace image"] "https://cdn-images-1.medium.com/max/697/1*tsHrUKwQXG1YZX0l957ISw.png",PlainText "."]]}}))
    (parseMarkdownDocument "---\ntitle: Syntaxe MARKDOWN\n---\n\nThis is a paragraph with an image ![Text to replace image](https://cdn-images-1.medium.com/max/697/1*tsHrUKwQXG1YZX0l957ISw.png).")

test_parse_body_section_md :: Test
test_parse_body_section_md = TestCase $
  assertEqual "should parse body section md"
    (Right (Document {header = Header {title = Just "Syntaxe MARKDOWN", author = Nothing, date = Nothing}, body = Body {blocks = [Section (Just "") [Section (Just "header 1") [CodeBlock Nothing [Paragraph [PlainText "This is a code block."]],List [Paragraph [PlainText "list item 1"],Paragraph [PlainText "list item 2"],Paragraph [PlainText "list item 3"]]]]]}}))
    (parseMarkdownDocument ("---\ntitle: Syntaxe MARKDOWN\n---\n\n" ++
    "## header 1\n" ++ "```\n"
    ++ "This is a code block.\n" ++
    "```\n" ++
    "- list item 1\n" ++
    "- list item 2\n" ++
    "- list item 3\n"))


test_parse_valid_header_json :: Test
test_parse_valid_header_json = TestCase $
  assertEqual "should parse valid header md"
    (Right (Document {header = Header {title = Just "Syntaxe JSON", author = Just "Fornes Leo", date = Just "2024-01-01"}, body = Body {blocks = []}}))
    (parseJsonDocument ("{\n" ++
    "\"header\": {\n" ++
        "\"title\": \"Syntaxe JSON\",\n" ++
        "\"author\": \"Fornes Leo\",\n" ++
        "\"date\": \"2024-01-01\"\n" ++
    "},\n" ++
    "\"body\": []\n}\n"))

test_parse_bad_header_json :: Test
test_parse_bad_header_json = TestCase $
  assertEqual "should parse header document bad "
    (Left "Unknown key: aur")
    (parseJsonDocument ("{\n" ++
    "\"header\": {\n" ++
        "\"title\": \"Syntaxe JSON\",\n" ++
        "\"aur\": \"Fornes Leo\",\n" ++
        "\"date\": \"2024-01-01\"\n" ++
    "},\n" ++
    "\"body\": []\n}\n"))

test_parse_body_paragphe_json :: Test
test_parse_body_paragphe_json = TestCase $
  assertEqual "should parse body paragphe json"
    (Right (Document {header = Header {title = Just "Syntaxe JSON", author = Nothing, date = Nothing}, body = Body {blocks = [Paragraph [PlainText "This is a paragraph with ",Strong "bold",PlainText ", ",Emph "italic",PlainText " and ",CodeInline "code ",PlainText " text."]]}}))
    (parseJsonDocument ("{\n" ++
    "  \"header\": {\n" ++
    "    \"title\": \"Syntaxe JSON\"\n" ++
    "  },\n" ++
    "  \"body\": [\n" ++
    "    [\n" ++
    "      \"This is a paragraph with \",\n" ++
    "      {\n" ++
    "        \"bold\": \"bold\"\n" ++
    "      },\n" ++
    "      \", \",\n" ++
    "      {\n" ++
    "        \"italic\": \"italic\"\n" ++
    "      },\n" ++
    "      \" and \",\n" ++
    "      {\n" ++
    "        \"code\": \"code \"\n" ++
    "      },\n" ++
    "      \" text.\"\n" ++
    "    ]\n" ++
    "  ]\n" ++
    "}\n"))

test_parse_body_paragphe_link_json :: Test
test_parse_body_paragphe_link_json = TestCase $
  assertEqual "should parse body paragphe link json"
    (Right (Document {header = Header {title = Just "Syntaxe JSON", author = Nothing, date = Nothing}, body = Body {blocks = [Paragraph [PlainText "This is a paragraph with a, ",Link [PlainText "link"] "https://www.youtube.com/watch?v=dQw4w9WgXcQ&ab_channel=RickAstley",PlainText "."]]}}))
    (parseJsonDocument ("{\n" ++
    "  \"header\": {\n" ++
    "    \"title\": \"Syntaxe JSON\"\n" ++
    "  },\n" ++
    "  \"body\": [\n" ++
    "    [\n" ++
    "      \"This is a paragraph with a, \",\n" ++
    "      {\n" ++
    "        \"link\": {\n" ++
    "          \"url\": \"https://www.youtube.com/watch?v=dQw4w9WgXcQ&ab_channel=RickAstley\",\n" ++
    "          \"content\": [\n" ++
    "            \"link\"\n" ++
    "          ]\n" ++
    "        }\n" ++
    "      },\n" ++
    "      \".\"\n" ++
    "    ]\n" ++
    "  ]\n" ++
    "}\n"))

test_parse_body_paragphe_image_json :: Test
test_parse_body_paragphe_image_json = TestCase $
  assertEqual "should parse body paragphe image json"
    (Right (Document {header = Header {title = Just "Syntaxe JSON", author = Nothing, date = Nothing}, body = Body {blocks = [Paragraph [PlainText "This is a paragraph with an image, ",Image [PlainText "Text to replace image"] "https://cdn-images-1.medium.com/max/697/1*tsHrUKwQXG1YZX0l957ISw.png",PlainText "."]]}}))
    (parseJsonDocument ("{\n" ++
    "  \"header\": {\n" ++
    "    \"title\": \"Syntaxe JSON\"\n" ++
    "  },\n" ++
    "  \"body\": [\n" ++
    "    [\n" ++
    "      \"This is a paragraph with an image, \",\n" ++
    "      {\n" ++
    "        \"image\": {\n" ++
    "          \"url\": \"https://cdn-images-1.medium.com/max/697/1*tsHrUKwQXG1YZX0l957ISw.png\",\n" ++
    "          \"alt\": [\n" ++
    "            \"Text to replace image\"\n" ++
    "          ]\n" ++
    "        }\n" ++
    "      },\n" ++
    "      \".\"\n" ++
    "    ]\n" ++
    "  ]\n" ++
    "}\n"))

test_parse_body_section_json :: Test
test_parse_body_section_json = TestCase $
  assertEqual "should parse body section json"
    (Right (Document {header = Header {title = Just "Syntaxe MARKDOWN", author = Nothing, date = Nothing}, body = Body {blocks = [Section (Just "") [Section (Just "header 1") [CodeBlock Nothing [Paragraph [PlainText "This is a code block."]],List [Paragraph [PlainText "list item 1"],Paragraph [PlainText "list item 2"],Paragraph [PlainText "list item 3"]]]]]}}))
    (parseJsonDocument ("{\n" ++
    "  \"header\": {\n" ++
    "    \"title\": \"Syntaxe JSON\"\n" ++
    "  },\n" ++
    "  \"body\": [\n" ++
    "    {\n" ++
    "      \"section\": {\n" ++
    "        \"title\": \"header 2\",\n" ++
    "        \"content\": [\n" ++
    "          {\n" ++
    "            \"codeblock\": [\n" ++
    "              \"This is a code block.\"\n" ++
    "            ]\n" ++
    "          },\n" ++
    "          {\n" ++
    "            \"list\": [\n" ++
    "              [\n" ++
    "                \"list item 1\"\n" ++
    "              ],\n" ++
    "              [\n" ++
    "                \"list item 2\"\n" ++
    "              ],\n" ++
    "              [\n" ++ 
    "                \"list item 3\"\n" ++
    "              ]\n" ++
    "            ]\n" ++
    "          }\n" ++
    "        ]\n" ++
    "      }\n" ++
    "    }\n" ++
    "  ]\n" ++
    "}\n"))

test1 :: Test
test1 = TestList [TestLabel "test1" test_usage, 
    TestLabel "test2" test_defaultOptions,
    TestLabel "test3" test_parseArgs_success,
    TestLabel "test4" test_parseArgs_missingRequired,
    TestLabel "test5" test_parseArgs_missingOneOption,
    TestLabel "test6" test_parseArgs_showHelp,
    TestLabel "test7" test_parseArgs_invalidOption, 
    TestLabel "test8" test_nbOptions]

test2 :: Test
test2 = TestList[TestLabel "test9" test_option_i,
    TestLabel "test10" test_option_f,
    TestLabel "test11" test_option_o,
    TestLabel "test12" test_option_e,
    TestLabel "test13" test_option_h,
    TestLabel "test14" test_detectFormat_xml,
    TestLabel "test15" test_detectFormat_json,
    TestLabel "test16" test_detectFormat_no_ext,
    TestLabel "test17" test_detectFormat_dot_at_end,
    TestLabel "test18" test_detectFormat_empty]

test3 :: Test
test3 = TestList[TestLabel "test19" test_inputFmt_defined,
    TestLabel "test20" test_inputFmt_detected, 
    TestLabel "test21" test_inputFmt_unknown,
    TestLabel "test22" test_options_eq,
    TestLabel "test23" test_options_not_eq,
    TestLabel "test24" test_options_show, 
    TestLabel "test25" test_document_eq, 
    TestLabel "test26" test_document_not_eq, 
    TestLabel "test27" test_document_show]

test4 :: Test 
test4 = TestList[TestLabel "test28" test_inline_eq,
    TestLabel "test29" test_inline_not_eq,
    TestLabel "test30" test_block_eq,
    TestLabel "test31" test_block_not_eq,
    TestLabel "test32" test_body_eq,
    TestLabel "test33" test_body_not_eq,
    TestLabel "test34" test_body_show,
    TestLabel "test35" test_header_eq,
    TestLabel "test36" test_header_not_eq,
    TestLabel "test37" test_header_show]

test_xml_ouput :: Test
test_xml_ouput = TestList[TestLabel "test38" test_indent_0,
    TestLabel "test39" test_indent_1,
    TestLabel "test40" test_indent_2,
    TestLabel "test41" test_inline_plain,
    TestLabel "test42" test_inline_emph,
    TestLabel "test43" test_inline_mixed,
    TestLabel "test44" test_inline_link,
    TestLabel "test45" test_inline_image,
    TestLabel "test46" test_inline_code, 
    TestLabel "test47" test_block_paragraph,
    TestLabel "test48" test_block_section,
    TestLabel "test49" test_block_codeblock,
    TestLabel "test50" test_block_list,
    TestLabel "test51" test_block_section_empty_title,
    TestLabel "test52" test_block_codeblock_nolang,
    TestLabel "test53" test_bodyXml_paragraph,
    TestLabel "test54" test_headerXml,
    TestLabel "test55" test_headerXml_only_date,
    TestLabel "test56" test_headerXml_only_title,
    TestLabel "test57" test_documentToXml]

test_md_ouput :: Test
test_md_ouput = TestList[TestLabel "test58" test_md_plain,
    TestLabel "test59" test_md_emph,
    TestLabel "test60" test_md_strong,
    TestLabel "test61" test_md_code,
    TestLabel "test62" test_md_link,
    TestLabel "test63" test_md_image,
    TestLabel "test64" test_md_mixed, 
    TestLabel "test65" test_md_paragraph,
    TestLabel "test66" test_md_section,
    TestLabel "test67" test_md_nested_section,
    TestLabel "test68" test_md_codeblock_lang,
    TestLabel "test69" test_md_codeblock_nolang,
    TestLabel "test70" test_md_list, 
    TestLabel "test71" test_md_section_empty_title,
    TestLabel "test72" test_md_section_nothing,
    TestLabel "test73" test_documentToMarkdown]

test_json_ouput :: Test
test_json_ouput = TestList[TestLabel "test74" test_stringOrEmpty_nothing,
  TestLabel "test75" test_stringOrEmpty_just,
  TestLabel "test76" test_stringOrEmpty_emptyString,
  TestLabel "test77" test_list_empty,
  TestLabel "test78" test_list_single,
  TestLabel "test79" test_list_multiple,
  TestLabel "test80" test_inlineToJson_plainText,
  TestLabel "test81" test_inlineToJson_emph,
  TestLabel "test82" test_inlineToJson_strong,
  TestLabel "test83" test_inlineToJson_code,
  TestLabel "test84" test_inlineToJson_image,
  TestLabel "test85" test_inlineToJson_link,
  TestLabel "test86" test_blockList_empty,
  TestLabel "test87" test_blockList_single,
  TestLabel "test88" test_blockList_multiple, 
  TestLabel "test89" test_blockToJson_section, 
  TestLabel "test90" test_blockToJson_codeblock, 
  TestLabel "test91" test_blockToJson_list,
  TestLabel "test92" test_header_full,
  TestLabel "test93" test_header_partial,
  TestLabel "test94" test_header_empty,
  TestLabel "test95" test_documentToJson]

test_html_ouput :: Test
test_html_ouput = TestList[TestLabel "test96" test_inlineToHtml_plain,
  TestLabel "test97" test_inlineToHtml_emph,
  TestLabel "test98" test_inlineToHtml_strong,
  TestLabel "test99" test_inlineToHtml_code,
  TestLabel "test100" test_inlineToHtml_link,
  TestLabel "test101" test_inlineToHtml_image,
  TestLabel "test102" test_blockToHtml_paragraph,
  TestLabel "test103" test_blockToHtml_section,
  TestLabel "test104" test_blockToHtml_code,
  TestLabel "test105" test_blockToHtml_list,
  TestLabel "test106" test_blockToHtml_section_nothing,
  TestLabel "test107" test_html_codeblock_no_lang,
  TestLabel "test108" test_bodyHtml,
  TestLabel "test109" test_full_headerHtml,
  TestLabel "test110" test_partial_headerHtml,
  TestLabel "test111" test_empty_headerHtml,
  TestLabel "test112" test_documentToHtml]

test_yaml_ouput :: Test
test_yaml_ouput = TestList[TestLabel "test113" test_inlineListYaml,
  TestLabel "test114" test_inlineToYaml_linkYaml,
  TestLabel "test115" test_inlineToYaml_imageYaml,
  TestLabel "test116" test_blockListYaml_paragraphs,
  TestLabel "test117" test_blockListYaml_empty,
  TestLabel "test118" test_block_section_yaml,
  TestLabel "test119" test_block_section_none,
  TestLabel "test120" test_block_code,
  TestLabel "test121" test_block_list_yaml,
  TestLabel "test122" test_header_full_yaml,
  TestLabel "test123" test_header_partial_yaml,
  TestLabel "test124" test_header_empty_yaml,
  TestLabel "test125" test_documentToYaml,
  TestLabel "test126" test_inlineListYaml_empty]

test_xml_input :: Test
test_xml_input = TestList[TestLabel "test127" test_parse_valid_document,
  TestLabel "test128" test_parse_invalid_document,
  TestLabel "test129" test_parse_bad_document,
  TestLabel "test130" test_parse_valid_header,
  TestLabel "test131" test_parse_bad_header,
  TestLabel "test132" test_parse_body_paragphe,
  TestLabel "test133" test_parse_body_paragphe_link,
  TestLabel "test134" test_parse_body_paragphe_image,
  TestLabel "test135" test_parse_body_section]

test_markdown_input :: Test
test_markdown_input = TestList[TestLabel "test136" test_parse_valid_header_md,
  TestLabel "test137" test_parse_bad_header_md,
  TestLabel "test138" test_parse_body_paragphe_md,
  TestLabel "test139" test_parse_body_paragphe_link_md,
  TestLabel "test140" test_parse_body_paragphe_image_md,
  TestLabel "test141" test_parse_body_section_md]

test_json_input :: Test
test_json_input = TestList[TestLabel "test142" test_parse_valid_header_json,
  TestLabel "test143" test_parse_bad_header_json,
  TestLabel "test144" test_parse_body_paragphe_json,
  TestLabel "test145" test_parse_body_paragphe_link_json,
  TestLabel "test146" test_parse_body_paragphe_image_json,
  TestLabel "test147" test_parse_body_section_json]

tests :: Test
tests = TestList [test1, test2, test3, test4, test_xml_ouput,
    test_md_ouput, test_json_ouput, test_html_ouput,
    test_yaml_ouput, test_xml_input, test_markdown_input,
    test_json_input]

main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then return () else exitWith ExitSuccess
