:- use_module(library(dcg_util)).

lowercase(C) -->
    [C],
    { between(0'a, 0'z, C) }.

:- use_module(library(tap)).

% make sure our auxiliary predicate behaves as expected
'lowercase is behaving' :-
    phrase(lowercase(C), `hello`, Rest),
    C == 0'h,
    Rest == `ello`.

nothing :-
    Text = `some text`,
    once(phrase(generous(lowercase,Matches),Text,Rest)),
    Matches == [],
    Rest == Text.

backtrack :-
    Text = `abc`,
    findall(X,phrase(generous(lowercase,X),Text,_),Matches),
    Matches == [``, `a`, `ab`, `abc`].
