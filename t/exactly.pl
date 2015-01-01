:- use_module(library(dcg_util)).

ho(ho) -->
    "ho".

:- use_module(library(tap)).

'santa: parse, known N, ignore matches' :-
    Text = `hohoho`,
    phrase(exactly(3,ho),Text).

'santa: parse, find N, ignore matches' :-
    Text = `hohoho`,
    phrase(exactly(N,ho),Text),
    N == 3.


'santa: parse, known N, keep matches' :-
    Text = `hohoho`,
    phrase(exactly(3,ho,Matches),Text),
    Matches == [ho, ho, ho].

'santa: parse, find N, keep matches' :-
    Text = `hohoho`,
    phrase(exactly(N,ho,Matches),Text),
    N == 3,
    Matches == [ho, ho, ho].


'santa: generate, known N' :-
    Matches = [ho,ho,ho],
    phrase(exactly(3,ho,Matches), Text),
    Text == `hohoho`.

'santa: generate, find N' :-
    Matches = [ho,ho,ho],
    phrase(exactly(N,ho,Matches), Text),
    N == 3,
    Text == `hohoho`.


'scrooge: parse, ignore matches' :-
    Text = `bah humbug`,
    phrase(exactly(0,ho),Text,Rest),
    Rest == Text.

'scrooge: parse, with matches' :-
    Text = `bah humbug`,
    phrase(exactly(0,ho,Matches),Text,Rest),
    Matches == [],
    Rest == Text.
