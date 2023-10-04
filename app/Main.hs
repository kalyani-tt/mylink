module Main where

import Macro
import Compile
import System.Environment
import System.Directory
import System.FilePath
import Control.Monad
import Data.Map qualified as M

template :: String -> String
template s = unlines
    [ "<!DOCTYPE html>"
    , "<html lang=\"en\">"
    , "<head>"
    , "  <title>Kali's Notes</title>"
    , "  <meta charset=\"UTF-8\">"
    , "  <link rel=\"stylesheet\" href=\"../css/style.css\">"
    , "</head>"
    , "<body>"
    , s
    , "</body>" ]

clearDir name = do
    files <- listDirectory name
    forM_ files \file -> removeFile (name </> file)

build :: String -> String -> String -> IO ()
build macrosFolder pagesFolder outFolder = do
    macroFilenames <- listDirectory macrosFolder
    macroDefs <- M.fromList <$> forM macroFilenames \filename ->
        (takeBaseName filename ,) <$> parseDefsFile (macrosFolder </> filename)
    
    pageFilenames <- listDirectory pagesFolder
    pageDocs <- M.fromList <$> forM pageFilenames \filename ->
        (takeBaseName filename ,) <$> parseDocFile (pagesFolder </> filename)
    
    let qMacroDefs = M.mapWithKey (\name defs -> qualifyDefsNames name macroDefs defs) macroDefs
    let qPageDocs = fmap (qualifyDocNames macroDefs) pageDocs

    let renderedPages = renderDocs template qMacroDefs qPageDocs

    clearDir outFolder

    forM_ (M.toList renderedPages) \(name, content) -> do
        writeFile (outFolder </> name <.> "html") content

main = do
    [macros, pages, out] <- getArgs
    build macros pages out