:- use_module(library(dcg_util)).

:- use_module(library(tap)).

'parse this' :-
    phrase(parsing, `content`, `content`).

'generate this'(fail) :-
    phrase(parsing, _, _).


'when_parsing: parsing' :-
    Text = `something goes here`,
    phrase(when_parsing(X=parsed), Text, _),
    X == parsed.

'when_parsing: generating' :-
    phrase(when_parsing(X=parsed), _, _),
    var(X).


'when_generating: parsing' :-
    Text = `something goes here`,
    phrase(when_generating(X=generated), Text, _),
    var(X).

'when_generating: generating' :-
    phrase(when_generating(X=generated), _, _),
    X == generated.
