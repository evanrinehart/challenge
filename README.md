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
printed sequence of characters (via the print op) and the final editor
state.

The challenge guarantees the input data is well formed and obeys certain
numeric limits. By using attoparsec for the parser, and a few lines for the
interpreter, the solution would be pretty simple. But there are two issues.

- The simple interpreter would technically be a partial function, since
many combinations of Op and EditorState would be nonsense. Without actually
verifying the input somehow, the program could crash badly.
- If the input is not well formed after all, the program could crash badly
even before getting to the parse stage. For instance the input data may not
be nice UTF-8.

To get better behavior in the face of any input, more care is needed at each
stage of processing.

## Input reader stage

Still trying to keep things managable I decided to load all of the input
into memory, then verify and decode it. This way the bulk of the work is
done in a "pure" function:

```
validateByteString :: ByteString -> Either Word8 Text
```

which gives the input data as text as long as it only contains the specified
characters. Namely lower case english letters, digits, space, and newlines.
If anything else is detected, such as punctuation, capitals, or bytes from
a more sophisticated text encoding, the first offending byte is returned
instead.

## Text parser stage

The parser attempts to extract a list of Op from flat Text. But [Op] by itself
doesn't indicate in its type that we have verified any of the constraints
the input is supposed to have. There are 2 properties to be guaranteed for
a op sequence in isolation: total characters appended doesn't exceed 10^6, and
total characters deleted doesn't exceed 2 * 10^6. We can use a newtype wrapper
and "smart constructor" to flag the fact that we checked this. The parser
checks these limits before returning the (wrapped) Op list, and serves as the
"kernel of trust" for these properties. Also we can return the crude error
message from attoparsec if the parsing fails for some reason.

```
newtype ValidOps = ValidOps [Op]

parseInput :: Text -> Either String ValidOps
```

## Operation validator stage

Even if you have a ValidOps, you may not be able to use it on a given
editor state, not even the initial empty state `("",[])`. So we can prescreen
a given combination of ValidOps and EditorState and determine the operations
can hypothetically be carried out. I made another data type to indicate this
check has been performed for the paired ops and state:

```
data ValidEditorSession = VES ValidOps EditorState

data BadOperation = BO
  { boLineNo :: Int
  , boMsg    :: String }

validateSession :: ValidOps -> EditorState -> Either BadOperation ValidEditorSession

```

Outside the Validator module, we should trust that if we have a `VES ops
state`, then it is valid to run ops on the state. Otherwise there is a bug in
that module!

```
runEditor :: ValidEditorSession -> ([Char], EditorSession)
```

This approach can be contrasted with putting Maybe on the runEditor return
type. I.e. if any invalid situation occurs during interpretation, return
Nothing.  This would (heavy-handedly) fix the partiality of runEditor, but has
some disadvantages:

- The implementation of the interpreter would be complicated by the munging
of Maybe values and by doing the checks better done earlier in the process.
- We would only be able to get feedback on invalid operations after carrying
out the expensive editing operations. To validate a program, it sucks to have
to actually run it.

Aside: splitting up the validation from the interpretation seems to be a
promising pattern in the world of dependent types. The smart constructor could
require hard evidence of the properties it is responsible for, removing the
need for a kernel of trust. Although this puts more of a burden on the parser.

## Put it all together and run the editor

All the IO is consolidated in the app/Main.hs source file. This includes
getting the input from stdin, throwing IO exceptions when problems are
detected, and printing out the final answer. The main IO action essentially
just runs all the above functions, and throws an exception in response to Left
results.

## Testing

Since each stage consists of pure functions, testing should be easy. To use
QuickCheck, it's necessary to have Arbitrary instances for data to be tested.
To generate arbitrary (valid) input data, I also made a simple encoder which
returns a set of valid ops to Text form.

```
encode :: ValidOps -> Text

law:
  parseInput (encode ops) = Right ops
```

Laws like this can be tested easily in QuickCheck, as long as you defined a
proper Arbitrary instance.

```
instance Arbitrary ValidOps where
  arbitrary = ...

prop_parsingUndoesEncoding ops = parseInput (encode ops) == Right ops
```

The test/Spec.hs program contains many other tests of the functions mentioned
earlier, and of certain critical functions not mentioned.

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
