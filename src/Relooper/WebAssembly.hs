module WebAssembly where

type Index = Int

type Expression = String

makeLoop i expr = "Loop " ++ show i ++ "\n" ++ expr ++ "\nend loop " ++ show i

genUseableLabel l = l + 1

concatExpr a b = if null b then a else if null a then b else a ++ "\n" ++ b

emptyExpr = ""

makeBlock i expr = "Block " ++ show i ++ "\n" ++ expr ++ "\nend block " ++ show i

