:- use_module(library(dcg_util)).

hello(english) --> "hello".
hello(texan) --> "howdy".

greeting(Lang) -->
    hello(Lang),
    eos.

quote -->
    "I said, ",
    followed_by(hello(_)).

:- use_module(library(tap)).

'the end' :-
    Text = `howdy`,
    phrase(greeting(Lang),Text,Rest),
    Lang == texan,
    Rest == [].

'more to come'(fail) :-
    Text = `howdy partner`,
    phrase(greeting(_),Text,_).


'follow up' :-
    Text = `I said, hello`,
    once(phrase(quote,Text,Rest)),
    Rest == `hello`.

'follow up absent'(fail) :-
    Text = `I said, goodbye`,
    phrase(quote,Text,_).
