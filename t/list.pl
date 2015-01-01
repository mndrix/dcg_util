:- use_module(library(dcg_util)).

% DCG rules needed for describing a rudimentary CSV syntax:
% "hello","two words"
comma --> ",".
quote --> "\"".
any(C) --> [C].
column(W) --> quote, generous(any,W), quote.

:- use_module(library(tap)).

'csv: parsing' :-
    Text = `"hello","two words"`,
    once(phrase(list(column,comma,Words), Text)),
    Words == [`hello`, `two words`].

'csv: generating' :-
    once(phrase(list(column,comma,[`first`,`2nd one`,`third`]),Text)),
    Text == `"first","2nd one","third"`.
