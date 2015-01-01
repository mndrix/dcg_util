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

username :-
    Text = `mndrix`,
    once(phrase(at_least(3,lowercase,Name),Text)),
    Name == `mndrix`.

'too short'(fail) :-
    Text = `joe`,
    phrase(at_least(4,lowercase,_),Text).
