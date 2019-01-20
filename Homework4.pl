%Homework 5
%Bryan Arnold
%CSE 4705

%Q5.1
%Define a predicate unifiable(List1,Term,List2) where List2 is the list of all members of List1 that unify with Term. The elements of List2 should not be instantiated by the %unification. For example,
%unifiable([X,b,t(Y)],t(a),List).
%List= [X,t(Y)].
%Note that X and Y are still not instantiated. So the tricky part is: how do we check that they unify with t(a) without instantiating them? Write the predicate so that it %terminates immediately after finding the correct answer. (That is, after prolog produces its first solution, it terminates executing the query immediately.)
%Hint: consider using possibilities associated with using tests of the form: \+ term1 = term2.

unifiable([], _, []).


unifiable([X | Y], A, B) :- \+ X = A, !,
 unifiable(Y, A, B).


unifiable([X | Y], A, [X | B]) :- unifiable(Y, A, B).

%Q5.2
%Define the predicate:
%split(Numbers, Positive, Negatives)
%which splits a list of numbers into 2 lists: positive ones (including zero) and negative ones. For example,
%split([3,-1,0,5,-2],[3,0,5],[-1,-2]).
%should return true. Write the predicate so that it terminates immediately after finding the correct answer.

split([], [], []).


split([X | Y], [X | Y1], Z) :- X >= 0, !,
 split(Y, Y1, Z).

split([X | Y], Z, [X | Y1]) :- X < 0, !,
 split(Y, Z, Y1).

%Q5.3
%Define the subtraction predicate: 
%set_difference(Set1,Set2,SetDifference) 
%where all the three sets are represented as lists. For example,
%set_difference([a,b,c,d],[b,d,e,f],[a,c]).
%should return true. Also, the predicate should terminate as soon as the first solution is found when unifying variables for these arguments. That is,
%set_difference([a,b,c,d],[b,d,e,f],X).
%should terminate immediately after finding the first solution,
%X = [a,c].

set_difference([], _, []).

set_difference([X | Y], Z, [X | Y1]) :-
    not(member(X, Z)),
    set_difference(Y, Z, Y1).

set_difference([_ | Y], Z, Y1) :-
    set_difference(Y, Z, Y1).

%Q5.4
%Write a prolog program that defines the predicate groundTerm(Term) which tests whether or not Term is a ground term. Ground terms are those taht don't contain variables. %Here are examples of how the predicate should behave.
%groundTerm(X).
%false.
%groundTerm(french(bic_mac,le_bic_mac)).
%true.
%groundTerm(french(whopper,X)).
%false.
%Also, the predicate should terminate execution immediately after producing its first (and only) solution.

groundTerm(X) :-
  nonvar(X),
  X =.. [_ | Y],
  gTermFound(Y).

gTermFound([]).

gTermFound([X | Y]) :-
  groundTerm(X),
  gTermFound(Y).

%Q5.5
%Sets can be though of as lists that don't contain any repeated elements. For example, [a,4,6] is a set, but [a,4,6,a] is not (as it contains 2 occurrences of a). Write a %Prolog program, powerset/2 that generates the set of all subsets of a list in sorted order. (Note the input list may not, itself, be a set.)
%Example1: powerset([a,b],X).
%should yield:
%X = [[],[a],[a,b],[b],[b,a]].
%Example2: powerset([a,b,a],X).
%should also yield (note input list is not actually a set):
%X = [[],[a],[a,b],[b],[b,a]].
%Example3: powerset([a,b,a,c],X).
%should yield:
%X = [[], [a], [a, b], [a, b, c], [a, c], [a, c, b], [b], [b, a], [b, a, c], [b, c], [b, c, a], [c], [c, a], [c, a, b], [c, b], [c, b, a]].
%Also, the predicate should terminate execution immediately after producing its first (and only) solution.

powerset(X, Y) :- bagof(S, sub(S, X), Y).
 
sub([], []).
sub([], [_ | _]).

sub([X | X1], [X | Y1]) :- sub(X1, Y1).
sub([X | X1], [_ | Y1]) :- append(_, [X | Z], Y1), !, sub(X1, Z).
