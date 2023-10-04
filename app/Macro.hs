module Macro where

import Data.Map qualified as M
import Data.Maybe
import Control.Monad.Reader
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.Char
import System.Exit
import Debug.Pretty.Simple

data Term
    = Text String
    | Lam String Term
    | App Term Term
    | Var String
    | Concat Term Term
    | Include Term
    deriving Show

type Env = (M.Map String Term, M.Map String String)

eval :: Term -> Reader Env Term
eval (Text s) = pure (Text s)
eval (Var x) = do
    env <- asks fst
    pure case M.lookup x env of
        Just a -> a
        Nothing -> error $ x
eval (Lam x f) = do
    env <- asks fst
    pure (Lam x (foldr (\(n, a) acc -> subst n acc a) f (M.toList . M.delete x $ env)))
eval (App f a) = do
    f <- eval f
    a <- eval a
    case f of
        Lam x e -> local (\(env, pages) -> (M.insert x a env, pages)) (eval e)
        _ -> error $ show f
eval (Concat a b) = do
    a <- eval a
    b <- eval b
    pure case (a, b) of
        (Text a, Text b) -> Text (a ++ b)
eval (Include name) = do
    name <- eval name
    case name of
        Text name -> do
            env <- asks snd
            pure (Text (fromJust . M.lookup name $ env))

subst :: String -> Term -> Term -> Term
subst x (Text s) a = Text s
subst x (Var y) a =
    if x == y then
        a
    else
        Var y
subst x (Lam y f) a =
    if x == y then
        Lam y f
    else
        Lam y (subst x f a)
subst x (App f e) a = App (subst x f a) (subst x e a)
subst x (Concat y z) a = Concat (subst x y a) (subst x z a)
subst _ (Include name) _ = Include name

type Parser = Parsec Void String

name :: Parser String
name = some (try alphaNumChar <|> oneOf ['-', '_', '.'])

ws' = many (satisfy isSpace)

parseAtomicTerm :: Parser Term
parseAtomicTerm =
    try (do
        t <- some (noneOf ['\n', '\\', '{', '}', '[', ']'])
        pure (Text t)) <|>
    try (do
        string "\\"
        ws'
        string "include"
        ws'
        single '{'
        ws'
        n <- parseTerm
        ws'
        single '}'
        pure (Include n)) <|>
    try (do
        single '['
        ps <- some do
            ws'
            n <- name
            ws'
            pure n
        string "=>"
        ws'
        e <- parseTerm
        ws'
        single ']'
        pure (foldr Lam e ps)) <|>
    (do
        single '\\'
        ws'
        f <- Var <$> name
        args <- many do
            single '{'
            ws'
            a <- parseTerm
            ws'
            single '}'
            pure a
        pure (foldl App f args))

parseTerm :: Parser Term
parseTerm =
    try (do
        a <- parseAtomicTerm
        t <- ws'
        b <- parseTerm
        pure (Concat a (if t /= [] then Concat (Text t) b else b))) <|>
    parseAtomicTerm

data Defs = Defs [String] [(String, Term)]
    deriving Show

parseImport :: Parser String
parseImport = do
    ws'
    string "\\import"
    ws'
    single '{'
    ws'
    n <- name
    ws'
    single '}'
    ws'
    pure n

parseDefs :: Parser Defs
parseDefs = do
    imps <- many parseImport
    defs <- many do
        ws'
        string "\\def"
        ws'
        single '{'
        ws'
        n <- name
        ws'
        single '}'
        ws'
        single '{'
        ws'
        ps <- some do
            n <- name
            ws'
            pure n
        single '}'
        ws'
        single '{'
        ws'
        a <- parseTerm
        ws'
        single '}'
        pure (n, foldr Lam a ps)
    pure (Defs imps defs)

parseFile :: Parser a -> String -> IO a
parseFile p name = do
    s <- readFile name
    case parse (p >>= \x -> eof >> pure x) "" s of
        Right x -> pure x
        Left e -> do
            putStr $ errorBundlePretty e
            exitFailure

parseDefsFile = parseFile parseDefs

data Doc = Doc [String] Term
    deriving Show

parseDoc :: Parser Doc
parseDoc = do
    ws'
    imps <- many parseImport
    ws'
    content <- parseTerm;
    ws'
    pure (Doc imps content)

parseDocFile = parseFile parseDoc