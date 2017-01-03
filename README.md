# Cat party

Currently, Cat party is a C parser implemented in Clojure/ClojureScript.

## Wait...what?

The eventual goal is to implement a C compiler, implemented in JavaScript (via compilation with ClojureScript), targeting JavaScript.  I.e., a C compiler that can operate entirely within a web browser.

## Why?

I have a larger goal in mind, but it's too early to say much about it.

## How to run it

To run the ClojureScript version, run the command

```bash
./scripts/brepl.sh
```

Then, navigate your web browser to `localhost:9000`.  Then, run the following commands in the REPL:

```clojure
(require [catparty.cparser :as cp])
(require [catparty.prettyprint :as pp])
(pp/pretty-print cp/t)
```

This prints the parse tree of the test code at the bottom of `cparser.cljc`.

You can run the same code in a Clojure REPL by running `lein repl`, and then

```clojure
(use 'catparty.cparser)
(pp/pretty-print t)
```

## What works

Supported features in the C parser are approximately:

* declarations (mostly)
* expressions (mostly)
* statements (mostly)

## What doesn't work

* typedef names
* struct and union type specifiers
* preprocessor (completely nonexistent)
* probably lots of other stuff

## License

The code is distributed under the [GPL version 3](https://www.gnu.org/licenses/gpl-3.0.en.html).

## Contact

David Hovemeyer, &lt;[david.hovemeyer@gmail.com](mailto:david.hovemeyer@gmail.com)&gt;
