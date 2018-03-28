# Parser Combinator Project

The goal of this project is to implement a parser combinator
library. See [here](https://github.com/nyu-oop-sp18/class07) for a
detailed discussion of the basic design of the library.

You can work on this project in groups of up to 3 students. It is
possible to do the project on your own, though I advise you to
join a group.

The deadline is Thursday, April 26. 

## Minimum Requirements

Your final version should satisfy the following minimum requirements:

* The parser combinator library should be fully implemented with
  support for parsing from a `String` input.

* Use your library to implement a JSON parser
  (see [json.JSON](src/main/scala/json/JSON.scala)) that
  converts a string in the JSON data exchange format into an abstract
  syntax tree representation of that
  data. See [here](https://www.json.org/) for a description of the
  JSON syntax. Your parser should provide detailed error reporting for
  parse errors.
  
* Your code should be well-documented and include extensive unit
  tests.
  
**Important:**

* Do *not* modify the signatures of the public methods provided by
  `Parser`. Though, it is OK to extend the interface with additional
  functionality.

* Avoid side effects such as mutable state, throwing exceptions,
  etc. in the library implementation (unless these side effects are 
  transparent to the library clients).

**Things to consider:**

* Add appropriate variance annotations to `Parser[A]`. Is it covariant
  or contravariant in `A`?
  
* While efficiency is not a primary concern, you should at least
  ensure that your implementation is sufficiently robust so that it
  works reliably on large input sequences. In particular, make sure
  that your implementations of `repeat` and `repeatN` run in constant
  stack space.

## Stretch Goals

* Extend your library with functionality for parsing from other input
  sources such as files. Extend your JSON parser appropriately to take
  advantage of the new functionality.

* Implement a simple command line calculator that parses arithmetic
  expressions over floating point numbers and evaluates them. The
  calculator should be implemented as a `Parser[Double]` that
  evaluates the expression on-the-fly during parsing.

* Generalize your parser to work on inputs other than character
  sequences. That is, your parser should work with abstract tokens
  rather than characters. These tokens can represent sequences of
  characters in a preprocessed input sequence such as keywords,
  numbers, identifiers, string literals, etc. This way, you can split
  a parser for a complex language into a *tokenizer* (aka lexer) that
  parses the input character sequence to produce a token sequence as
  output, and the actual parser that parses the token sequence to
  produce, e.g., an abstract syntax tree. This is how complex parsers
  are typically organized.

* ...

## Grading

What follows are the criteria by which I will evaluate your
project. Typically all team members will get the same grade for the
project. Though, we will conduct peer reviews to inform the individual
grades of the team members.

Code:
* Does your project fulfill the expectations listed above.
* Is the implementation of `Parser` conceptually correct?
* Are there unit tests?
* Is the code well organized?
* Is the code well documented?
* Am I able to clone and test the project easily? 

Team:
* How proactive was the team?
* Did the team distribute the work well?
* If the team struggled, did they come to me for help?

Individual:
* What contributions did the student make to the Github repo?
* How did the other team members evaluate the student?
* If the student struggled, did they come to me for help?
