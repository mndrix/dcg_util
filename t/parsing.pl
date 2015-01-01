:- use_module(library(dcg_util)).

:- use_module(library(tap)).

'parse this' :-
    phrase(parsing, `content`, `content`).

'generate this'(fail) :-
    phrase(parsing, _, _).
