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

'username: parse, whole text' :-
    Text = `mndrix`,
    once(phrase(greedy(lowercase,Name),Text)),
    Name == Text.

'username: parse, partial text' :-
    Text = `mndrix wrote this`,
    once(phrase(greedy(lowercase,Name),Text,Rest)),
    Name == `mndrix`,
    Rest == ` wrote this`.


backtrack :-
    Text = `abc`,
    findall(X,phrase(greedy(lowercase,X),Text,_), Xs),
    Xs == [`abc`,`ab`,`a`,``].
