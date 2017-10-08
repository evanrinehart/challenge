module Editor where

import Data.Monoid ((<>))
import Data.Text
import qualified Data.Text as T

import Types
import Validator

-- | Run operations on an editor starting state. Returns the sequence of
-- | characters printed and the final state. Assumes the sequence of operations
-- | is valid for the given starting state, so the operations should be
-- | verified against the starting state first. If the sequence if invalid,
-- | then runEditor will error badly.
runEditor :: ValidEditorSession -> ([Char], EditorState)
runEditor (VES (VO ops) s0) = go ops s0 where
  go []       s = ([], s)
  go (op:ops) s = case edit op s of
    (Nothing, s') -> go ops s'
    (Just x,  s') ->
      let (outs', s'') = go ops s' in
      (x : outs', s'')

-- | Execute one operation on an editor state. Returns the new state
-- | and potentially an output character. If the operation is invalid
-- | for the given state, then one of the components will be a ⊥.
-- | The operation should have been verified against the given state already.
edit :: Op -> EditorState -> (Maybe Char, EditorState)
edit (Append x) (s,ss)   = (Nothing,          (s <> x, s : ss))
edit (Delete k) (s,ss)   = (Nothing,          (dropEnd k s, s : ss))
edit (Print k)  (s,ss)   = (Just (index s (k-1)), (s, ss))
edit Undo       (_,s:ss) = (Nothing,          (s, ss))
edit Undo       (_,[])   = error "edit: unable to undo past the beginning!"
--edit Undo       (s,[])   = (s,[]) -- option to hide the error

