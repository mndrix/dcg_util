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

# Changes in this Version

  * ...

# Installation

Using SWI-Prolog 6.3 or later:

    ?- pack_install(dcg_util).

This module uses [semantic versioning](http://semver.org/).

Source code available and pull requests accepted at
http://github.com/mndrix/dcg_util

@author Michael Hendricks <michael@ndrix.org>
@license unlicense
