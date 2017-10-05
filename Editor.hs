module Editor where

import Data.Monoid ((<>))
import Data.Text

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
edit (Print i)  (s,ss)   = (Just (index s (i-1)), (s, ss))
edit Undo       (_,s:ss) = (Nothing,          (s, ss))
edit Undo       (_,[])   = error "edit: unable to undo past the beginning!"
--edit Undo       (s,[])   = (s,[]) -- option to hide the error
