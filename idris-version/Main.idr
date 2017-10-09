||| a manifest of things we care about during an editing session
record Metrics where
  constructor MkES
  currentSize : Nat
  sizeHistory : List Nat
  appendTotal : Nat
  deleteTotal : Nat

blankMetrics : Metrics
blankMetrics = MkES 0 [] 0 0

million : Nat
million = pow 10 2 -- 10^6 is really slow

strDropEnd : Nat -> String -> String
strDropEnd n = 
  pack .
  reverse .
  drop n .
  reverse .
  unpack

||| Appending a string str is only valid if str is non-empty and total
||| appends doesn't exceed one million characters.
data AppendOk : String -> Metrics -> Type where
  MkAppendOk :
    (LTE 1 (length str)) ->
    (LTE (appendTotal met + length str) Main.million) ->
    AppendOk str met

||| Deleting n characters is only valid is n > 0, total characters deleted
||| doesn't exceed one million, and there are at least n characters in
||| the current buffer.
data DeleteOk : Nat -> Metrics -> Type where
  MkDeleteOk :
    (1 `LTE` n) ->
    (n `LTE` currentSize met) ->
    ((deleteTotal + n) `LTE` Main.million) ->
    DeleteOk n met

||| Printing character n (1-based) is only valid if there are at least
||| n characters in the current buffer.
data PrintOk : Nat -> Metrics -> Type where
  MkPrintOk :
    (1 `LTE` n) ->
    (n `LTE` currentSize met) ->
    PrintOk n met

||| Undo is only valid if there is something in the undo stack to undo to.
data UndoOk : Metrics -> Type where
  MkUndoOk : NonEmpty (sizeHistory met) -> UndoOk met

append : (str : String) -> Metrics -> Metrics
append str met =
  let n = length str in
  record {
    currentSize $= (+ n),
    sizeHistory $= ((currentSize met) ::),
    appendTotal $= (+ n)
  } met

delete : (n : Nat) -> Metrics -> Metrics
delete n met =
  record {
    currentSize $= (\x => x `minus` n),
    sizeHistory $= ((currentSize met) ::),
    deleteTotal $= (+ n)
  } met

undo : (met : Metrics) -> { auto ok : UndoOk met } -> Metrics
undo met {ok=MkUndoOk p} =
  record {
    currentSize = head (sizeHistory met),
    sizeHistory = tail (sizeHistory met)
  } met

||| A chain of edits indexed by the accumulated metrics.
||| You can only extend the chain if the operation obeys the metric-based
||| restrictions.
data Edit : Metrics -> Type where
  Start  : Edit (Main.blankMetrics)
  Append : Edit met -> (str : String) -> AppendOk str met -> Edit (append str met)
  Delete : Edit met -> (n : Nat) -> DeleteOk n met -> Edit (Main.delete n met)
  Print  : Edit met -> (n : Nat) -> PrintOk n met -> Edit met
  Undo   : Edit met -> (p : UndoOk met) -> Edit (undo met)

||| Read an edit session from a string, if possible. The index (metrics) is
||| determined dynamically so it must be returned in a dependent pair.
||| The edit chain can then be used in metric-agnostic functions.
parse : String -> Maybe (met : Metrics ** Edit met)
parse input = Nothing -- Not yet implemented

-- To implement parse, we will probably need an auxilliary type for Strings
-- paired with their length. Since Strings are primitive, nothing can really
-- be proven about them. So we would need a kernel of trust implemented with
-- believe_me's.
