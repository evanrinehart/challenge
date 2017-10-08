module Types where

import Data.Text (Text)

-- | Raw editing operations. Not necessarily valid.
data Op =
  Append Text |
  Delete Int |
  Print Int |
  Undo
    deriving (Eq, Ord, Show, Read)

appendPayload (Append x) = Just x
appendPayload _ = Nothing

deleteCount (Delete x) = Just x
deleteCount _ = Nothing

-- | A list of Ops which has been verified for append and delete limits.
newtype ValidOps = VO { unValidOps :: [Op] }
  deriving (Show, Eq)

-- | A non-empty undo stack. Top string is the current buffer.
type EditorState = (Text, [Text]) -- cheapo non-empty list

-- | A valid ops sequence paired with an editor state. The ops have been
-- | verified to be valid when used on this starting state.
data ValidEditorSession = VES ValidOps EditorState
