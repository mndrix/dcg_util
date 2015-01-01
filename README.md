# Synopsis

```prolog
:- use_module(library(dcg_util)).
ho(ho) --> "ho".
comma --> ", ".

?- phrase(exactly(3,ho,Matches),`hohoho`).
Matches = [ho, ho, ho].

?- phrase(list(ho,comma,Santa),`ho, ho, ho`).
Santa = [ho, ho, ho].

?- phrase(list(ho,comma,[ho,ho,ho]),Text).
Text = `ho, ho, ho`.
```

# Description

This module is a collection of predicates and combinators for working with
Prolog's definite clause grammars (DCG).  As much as possible, I've tried to
make these rules symmetric so that you can use them for both parsing and
generating.

Most of these predicates factor out patterns that I've noticed in my DCGs.  Some
of them provide regex-like quantifiers similar to `*`, `*?`, `{2}`, etc.

## Example

To match a single, lowercase word one might write a DCG like this.  It consumes
as many lowercase letters as possible.  On backtracking, it gives back one letter at a time:

```prolog
lowercase(C) -->
    [C],
    { between(0'a,0'z,C) }.

word([X|Xs]) -->
    lowercase(X),
    word(Xs).
word([]) -->
    [].
```

Unfortunately, that mixes list operations, lowercase matching and search order.
It's more difficult to read than we'd like.  Because of the complexity, it's
also easy to make a mistake.

By using this library's predicate `greedy//2` we end up with something cleaner:

```prolog
word(Word) -->
    greedy(lowercase, Word).
```


# Installation

Using SWI-Prolog 7.1 or later:

    ?- pack_install(dcg_util).

This module uses [semantic versioning](http://semver.org/).

Source code available and pull requests accepted at
http://github.com/mndrix/dcg_util

@author Michael Hendricks <michael@ndrix.org>
@license unlicense
