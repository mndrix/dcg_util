:- use_module(library(dcg_util)).

ho(ho) --> "ho".
comma --> ", ".

:- use_module(library(tap)).

'synopsis: exactly' :-
    Text = `hohoho`,
    phrase(exactly(3,ho),Text).


'synopsis: list parsing' :-
    Text = `ho, ho, ho`,
    once(phrase(list(ho,comma,Santa),Text)),
    Santa == [ho, ho, ho].

'synopsis: list generating' :-
    once(phrase(list(ho,comma,[ho,ho,ho]),Text)),
    Text == `ho, ho, ho`.
