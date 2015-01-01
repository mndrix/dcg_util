:- use_module(library(dcg_util)).

ho(ho) -->
    "ho".

:- use_module(library(tap)).

synopsis :-
    Text = `hohoho`,
    phrase(exactly(3,ho),Text).
