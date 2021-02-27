module Lib (stdinTransform) where

import Text.Pandoc
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO

readerOptions :: ReaderOptions
readerOptions = def { readerExtensions = pandocExtensions }

writerOptions :: WriterOptions
writerOptions = def { writerHTMLMathMethod = KaTeX (Text.pack "") }
-- NB: URL for KaTeX is not needed because it's going to be loaded by wrapper

cardRenderer :: Block -> Either PandocError Block
cardRenderer block = case block of
    CodeBlock attrs@(_, classes, _) text | Text.pack "card" `elem` classes ->
        do (Pandoc _ body) <- runPure (readMarkdown readerOptions text)
           pure (Div attrs body)
    x -> pure x

stdinTransform :: IO ()
stdinTransform = do
    source <- TextIO.getContents
    let result =
            do Pandoc meta blocks <- runPure (readMarkdown readerOptions source)
               doc <- Pandoc meta <$> mapM cardRenderer blocks
               runPure (writeHtml5String writerOptions doc)
    case result of
        Left err -> print err
        Right doc -> TextIO.putStr doc
