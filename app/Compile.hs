module Compile where

import Control.Monad.Reader
import Data.Map qualified as M
import Macro

defNames :: Defs -> [String]
defNames (Defs _ defs) = map fst defs

qualifyTerm :: M.Map String Defs -> [String] -> Term -> Term
qualifyTerm allDefs imps def =
    foldr
        (\imp acc -> foldr
            (\defName acc -> subst defName acc (Var (imp ++ "." ++ defName)))
            acc
            (defNames (allDefs M.! imp)))
        def
        imps

qualifyTermLocally :: String -> [String] -> Term -> Term
qualifyTermLocally modName defNames def =
    foldr
        (\defName acc -> subst defName acc (Var (modName ++ "." ++ defName)))
        def
        defNames

qualifyDefsNames :: String -> M.Map String Defs -> Defs -> Defs
qualifyDefsNames modName allDefs (Defs imps defs) =
    Defs imps (map
        (\(name, def) -> (name, qualifyTerm allDefs imps
            (qualifyTermLocally modName (map fst defs) def)))
        defs)

qualifyDocNames :: M.Map String Defs -> Doc -> Doc
qualifyDocNames allDefs (Doc imps content) =
    Doc imps (qualifyTerm allDefs imps content)

createEnv :: M.Map String Defs -> M.Map String Term
createEnv allDefs = M.fromList do
    (impName, Defs imps defs) <- M.toList allDefs
    map (\(name, def) -> (impName ++ "." ++ name, def)) defs

renderDocs :: (String -> String) -> M.Map String Defs -> M.Map String Doc -> M.Map String String
renderDocs template allDefs docs =
    let
        allFiles = foldr (\name acc -> Cons (Text name) acc) Nil (map fst (M.toList docs))
        rends = M.mapWithKey
            (\name (Doc _ content) ->
                let
                    env =
                        M.fromList
                            [ ("cat", Lam "x" (Lam "y" (Concat (Var "x") (Var "y"))))
                            , ("title", Text name)
                            , ("cons", Lam "x" (Lam "y" (Cons (Var "x") (Var "y"))))
                            , ("nil", Nil)
                            , ("head", Lam "xs" (Head (Var "xs")))
                            , ("tail", Lam "xs" (Tail (Var "xs")))
                            , ("isnil", Lam "xs" (IsNil (Var "xs")))
                            , ("if", Lam "t" (Lam "a" (Lam "b" (If (Var "t") (Var "a") (Var "b")))))
                            , ("allfiles", allFiles) ] <>
                        (createEnv allDefs)
                in case runReader (eval content) (env, rends) of
                    Text s -> s)
            docs
    in fmap template rends