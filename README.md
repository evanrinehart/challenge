# challenge

https://www.hackerrank.com/challenges/simple-text-editor

In summary, the challenge is to implement a text editor which has 4 operations
and accepts its input as a text format. In the simplest possible Haskell
solution, we would have the operations modeled with a sum type:

```
data Op = Append Text | Delete Int | Print Int | Undo
```

The text editor state necessary to support the operations would be a non-empty
list of buffers, essentially an undo stack:

```
type EditorState = (Text, [Text])
```

And these functions between them to implement the functionality:

```
parseInput :: Text -> Maybe [Op]
runEditor :: [Op] -> EditorState -> ([Char], EditorState)
```

runEditor, basically an interpreter for the Op language, would return the
sequence of characters (output by the print command) and the final editor
state.

The challenge guarantees the input data is well formed and obeys certain
numeric limits. By using attoparsec for the parser, and a few lines for the
interpreter, the solution would be pretty simple. But there are two issues.

- The simple interpreter would technically be a partial function, since
many combinations of Op and EditorState would be nonsense. Without actually
verifying the input somehow, the program would crash badly.
- If the input is not well formed after all, the program could crash badly
even before getting to the parse stage. For instance the input data may not
be nice UTF-8.

To get better behavior in the face of any input, more care is needed at each
stage of processing.

## Input reader stage

## Text parser stage

## Operation validator stage

## Finally run the editor operations


## Commands to run

```
git clone https://github.com/evanrinehart/challenge challenge-evan
```

```
cd challenge-evan
stack build
```

```
stack test
```

```
stack exec challenge < extra/sample
```

There are other files in the extra directory which contains various problems.
They can't be fully interpreted but can be loaded into the program anyway
to see what the issue is.
