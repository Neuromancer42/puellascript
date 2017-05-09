{-# LANGUAGE DuplicateRecordFields #-}

module Relooper where

import qualified Data.Map.Lazy as M
import qualified Data.Set as S

import qualified Data.List as L

-- import qualified Language.WebAssemly as Wasm
import qualified WebAssembly as Wasm -- a fake library for demo

-- import qualified Language.CFG as Cfg
data Block = Block
  { getInBranches :: BlockSet
  , getOutBranches :: BlockSet
  , getContent :: Wasm.Expression
  , getCondition :: Wasm.Expression
  , getId :: BlockId
  } deriving (Show)

--  , getParent :: Shape
instance Eq Block where
  a == b = getId a == getId b

-- type BlockSet = S.Set Branch
type BlockSet = [BlockId]

type BlockId = Int

{-
data Branch = Branch
  { getFlowType :: FlowType
  , getAncestor :: Shape
  , getContent :: Wasm.Expression
  , getCondition :: Wasm.Expression
  , getSwitchValues :: [Wasm.Index]
  }

data FlowType
  = Direct
  | Break
  | Continue

data Shape = Shape
  { getId :: Int
  , getNext :: Shape
  , getNatural :: Shape
  , getType :: ShapeType
  , getContent :: ShapeContent
  }

data ShapeContent
  = SimpleShape Block
  | MultipleShape (M.Map Int Shape)
  | LoopShape Block
              BlockSet
-}
-- | close a block by making all forward blocks
--   pointing to it processed
closeBlock
  :: Wasm.Index
  -> [BlockId]
  -> [Block]
  -> M.Map BlockId Wasm.Index
  -> (Wasm.Expression, ([BlockId], [Block]))
closeBlock useableLabel initialIds unprocessedBlocks mapIdLabel =
  case L.intersect forwardIds unprocessedIds of
    [] -> makeNonLoop useableLabel initialBlocks unprocessedBlocks mapIdLabel
    unprocessedForwards ->
      mapfst (Wasm.makeLoop useableLabel) $
      foldForward
        (Wasm.genUseableLabel useableLabel)
        unprocessedForwards
        initialBlocks
        unprocessedBlocks
        newMap
  where
    initialBlocks = filter (\b -> getId b `elem` initialIds) unprocessedBlocks
    forwardIds = L.nub . L.concat . (map getInBranches) $ initialBlocks
    unprocessedIds = map getId unprocessedBlocks
    newMap = foldr (\blockId m -> M.insert blockId useableLabel m) mapIdLabel initialIds

-- | make a structure without foward goto
makeNonLoop
  :: Wasm.Index
  -> [Block]
  -> [Block]
  -> M.Map BlockId Wasm.Index
  -> (Wasm.Expression, ([BlockId], [Block]))
makeNonLoop useableLabel entries unprocessed mapIdLabel = (expr, (newEntriesId, restBlocks))
  where
    expr =
      foldl Wasm.concatExpr Wasm.emptyExpr $ map (renderContent useableLabel mapIdLabel) entries
    restBlocks = unprocessed L.\\ entries
    newEntriesId =
      L.intersect (L.nub $ L.concat $ map getOutBranches entries) $ map getId restBlocks

-- | render following blocks until all forward blocks
--   (see `closeBlock`) are processed
foldForward
  :: Wasm.Index
  -> [BlockId]
  -> [Block]
  -> [Block]
  -> M.Map BlockId Wasm.Index
  -> (Wasm.Expression, ([BlockId], [Block]))
foldForward useableLabel forwardIds initialBlocks unprocessedBlocks mapIdLabel =
  let result@(firstExpr, (newEntriesId, restBlocks)) =
        makeNonLoop useableLabel initialBlocks unprocessedBlocks mapIdLabel
  in case L.intersect forwardIds $ map getId restBlocks of
       [] -> result
       restIds ->
         mapfst (Wasm.concatExpr firstExpr) $
         appendForward useableLabel restIds newEntriesId restBlocks mapIdLabel

appendForward
  :: Wasm.Index
  -> [BlockId]
  -> [BlockId]
  -> [Block]
  -> M.Map BlockId Wasm.Index
  -> (Wasm.Expression, ([BlockId], [Block]))
appendForward useableLabel forwardIds initialIds unprocessedBlocks mapIdLabel =
  if null initialIds
    then (Wasm.emptyExpr, ([], unprocessedBlocks))
    else let result@(firstExpr, (newEntriesId, restBlocks)) =
               closeBlock useableLabel initialIds unprocessedBlocks mapIdLabel
         in case L.intersect forwardIds $ map getId restBlocks of
              [] -> result
              restIds ->
                mapfst (Wasm.concatExpr firstExpr) $
                appendForward useableLabel restIds newEntriesId restBlocks mapIdLabel

-- | given a CFG and one or more entries, generate a WebAssemly Expression
foldEntries :: Wasm.Index -> [BlockId] -> [Block] -> M.Map BlockId Wasm.Index -> Wasm.Expression
foldEntries useableLabel initialIds unprocessedBlocks mapIdLabel =
  let res@(firstExpr, (newEntriesId, restBlocks)) =
        closeBlock useableLabel initialIds unprocessedBlocks mapIdLabel
  in if null newEntriesId
       then firstExpr
       else Wasm.concatExpr firstExpr $ foldEntries useableLabel newEntriesId restBlocks mapIdLabel

-- | translate a block into WebAssemly Expression
renderContent :: Wasm.Index -> M.Map BlockId Wasm.Index -> Block -> Wasm.Expression
renderContent i m b = checkLabel `Wasm.concatExpr` bodyExpr
  where
    idTarget = getOutBranches b
    (branchTarget, needBlock) =
      foldr
        (\b r ->
           case M.lookup b m of
             Just l -> mapfst (l :) r
             Nothing -> (i : (fst r), True))
        ([], False)
        idTarget
    tailbranchExpr = setLabel `Wasm.concatExpr` (getCondition b)
    internalExpr = getContent b `Wasm.concatExpr` tailbranchExpr
    bodyExpr =
      if needBlock
        then Wasm.makeBlock i internalExpr
        else internalExpr
    setLabel = "TODO: setLabel here"
    checkLabel = "TODO: checkLabel here"

-- | some helper functions
mapfst :: (a -> b) -> (a, c) -> (b, c)
mapfst f (x, y) = (f x, y)
