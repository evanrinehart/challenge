module Editor where

import Data.Monoid ((<>))
import Data.Text
import qualified Data.Text as T

import Data.Typeable
import Control.Exception

import Types

type EditorState = (Text, [Text]) -- cheapo non-empty list

runEditor :: [Op] -> EditorState -> ([Char], EditorState)
runEditor ops s0 = go ops s0 where
  go []       s = ([], s)
  go (op:ops) s = case edit op s of
    (Nothing, s') -> go ops s'
    (Just x,  s') ->
      let (outs', s'') = go ops s' in
      (x : outs', s'')

edit :: Op -> EditorState -> (Maybe Char, EditorState)
edit (Append x) (s,ss)   = (Nothing,          (s <> x, s : ss))
edit (Delete k) (s,ss)   = (Nothing,          (dropEnd k s, s : ss))
edit (Print k)  (s,ss)   = (Just (index s (k-1)), (s, ss))
edit Undo       (_,s:ss) = (Nothing,          (s, ss))
edit Undo       (_,[])   = error "edit: unable to undo past the beginning!"
--edit Undo       (s,[])   = (s,[]) -- option to hide the error


isOpOk :: Op -> EditorState -> Bool
isOpOk (Append x) (s,ss) = True
isOpOk (Delete k) (s,ss) = k >= 1 && k <= T.length s
isOpOk (Print k)  (s,ss) = k >= 1 && k <= T.length s
isOpOk Undo       (s,[]) = False
isOpOk Undo       (s,ss) = True

areOpsOk :: [Op] -> EditorState -> Bool
areOpsOk []       _ = True
areOpsOk (op:ops) s =
  if isOpOk op s
    then let (_,s') = edit op s in areOpsOk ops s'
    else False


data BadOperationException = BadOperationException
  deriving (Show, Typeable)

instance Exception BadOperationException

