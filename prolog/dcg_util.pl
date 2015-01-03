:- module(dcg_util, [
    at_least//2,
    at_least//3,
    eos//0,
    exactly//2,
    exactly//3,
    followed_by//1,
    generous//2,
    greedy//1,
    greedy//2,
    list//3,
    parsing//0,
    when_generating//1,
    when_parsing//1
]).

:- use_module(library(clpfd)).

%% at_least(N:nonneg,:Dcg)//
%
%  Like at_least//3 but discards the matches.
:- meta_predicate at_least(+,3,*,*).
at_least(N,Goal) -->
    at_least(N,Goal,_).


%% at_least(N:nonneg,:Dcg,Matches:list)//
%
%  Consumes at least N matches of Dcg.  Dcg is called
%  with one extra parameter which should be bound to a representation
%  of what Dcg parsed.  After N matches, it consumes as many
%  more matches as possible.
:- meta_predicate at_least(+,3,?,*,*).
at_least(N0,Goal,[X|Xs]) -->
    { N0 > 0 },
    !,
    call(Goal,X),
    { N is N0 - 1 },
    at_least(N,Goal,Xs).
at_least(0,Goal,Xs) -->
    greedy(Goal,Xs).


%% exactly(N:nonneg,:Dcg)//
%
%  Like exactly//3 but discards the matches.
:- meta_predicate exactly(+,3,*,*).
exactly(N,Goal) -->
    exactly(N,Goal,_).


%% exactly(N:nonneg,:Dcg,Matches:list)//
%
%  Consumes exactly N matches of Dcg.  Dcg is called
%  with one extra parameter which should be bound to a
%  representation of what Dcg parsed.
:- meta_predicate exactly(+,3,?,*,*).
exactly(0,Goal,[]) -->
    ( parsing -> \+ call(Goal,_); [] ),
    !.
exactly(N0,Goal,[X|Xs]) -->
    { N0 #> 0 },
    { N #= N0 - 1 },
    call(Goal,X),
    exactly(N,Goal,Xs).


%% generous(:Goal,Matches:list)//
%
%  Consume as few matches of Goal as possible.  Goal is called
%  with one extra argument which should be bound to a
%  representation of what Dcg parsed.
:- meta_predicate generous(3,-,*,*).
generous(_Goal,[]) -->
    [].
generous(Goal,[X|Xs]) -->
    call(Goal,X),
    generous(Goal,Xs).


%% greedy(:Dcg)//
%
%  Like greedy//2 but discards the matches.
:- meta_predicate greedy(3,*,*).
greedy(Goal) -->
    greedy(Goal,_).


%% greedy(:Dcg,Matches:list)//
%
%  Like generous//2 but consumes as many matches as possible.
%  Gives back matches on backtracking.
:- meta_predicate greedy(3,-,*,*).
greedy(Goal,[X|Xs]) -->
    call(Goal,X),
    greedy(Goal,Xs).
greedy(_,[]) -->
    [].


%% list(:ElemDcg, :SeparatorDcg, Elems:list)//
%
%  Describes a list in which the elements match ElemDcg and the
%  separators match SeparatorDcg. Elems is the list of elements found.
%  The set of patterns matched by ElemDcg and SeparatorDcg
%  should be disjoint.  ElemDcg is called with one extra argument.
%  SeparatorDcg is called without any extra arguments.
%
%  On backtracking, gives back elements and their associated separators.
%  Always matches at least one element (without a trailing separator).
:- meta_predicate list(3,2,?,?,?).
list(ElemDCG, SepDCG, [Elem|Tail]) -->
    call(ElemDCG, Elem),
    ( call(SepDCG),
      list(ElemDCG, SepDCG, Tail)
    ; "",
      { Tail = [] }
    ).


%% followed_by(:Dcg)//
%
%  True if Dcg would match. Consumes nothing.
:- meta_predicate followed_by(//,*,*).
followed_by(Goal) -->
    \+ \+ Goal.


%% eos//
%
%  Matches the end of string position.
eos([],[]).


%% when_generating(:Goal)//
%
%  Call Goal when the DCG operates in generator mode.  Otherwise,
%  it's a noop.
:- meta_predicate when_generating(0,?,?).
when_generating(Goal) -->
    ( parsing -> []; { call(Goal) } ).


%% when_parsing(:Goal)//
%
%  Call Goal when the DCG operates in parsing mode.  Otherwise,
%  it's a noop.
:- meta_predicate when_parsing(0,?,?).
when_parsing(Goal) -->
    ( parsing -> { call(Goal) }; [] ).


%% parsing// is semidet.
%
%  True if the DCG is operating as a parser.  Specifically,
%  the DCG list is not a variable.
parsing(H,H) :-
    nonvar(H).
