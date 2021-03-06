%Homework 3
%Bryan Arnold
%CSE 4705

%Q3.1
%Write a DCG that generates the language anb2n. The language consists of a block of a's followed by a block of b's such that the block of b's is twice the length of the block %of a's, for any value of n = 0. Please use s for your sentence rule.

s --> [].
s --> x, s, y, y.

x --> [a].
y --> [b].

%Q3.2
%Write a DCG that generates the language anb2mc2mdn. This language consists of strings of the form: a block of a's, followed by a block of b's, followed by a block of c's, %followed by a block of d's where the a block and d block have the same length, and the b block and c block have the same length and consist of an even number of b's and c's. %The DCG should work for any value n = 0 and m = 0.  Please use s for your sentence rule.

s --> [].

s --> w, s, z.

s --> s1.



s1 --> [].
s1 --> x, x, s1, y, y.



w --> [a].

x --> [b].

y --> [c].

z --> [d].

%Q3.3
%Write a DCG that generates the following:
%prop -> p
%prop -> q
%prop -> r
%prop -> not prop
%prop -> (prop and prop)
%prop -> (prop or prop)
%prop -> (prop implies prop)
%When creating this DCG make sure to include parentheses and the logical operators. Use not, and, or, and implies for each of the above operators.
%As an example the string "NOT(p IMPLIES q)" would have to be represented in a list of 6 elements like this:
%[not, '(', p, implies, q, ')'].
%Please use prop as the functor for your rules.

prop --> [p].
prop --> [q].
prop --> [r].
prop --> ['not'], prop.
prop --> ['('], prop, conjunc, prop, [')'].

conjunc --> ['or'].
conjunc --> ['implies'].
conjunc --> ['and'].

%Q3.4
%Create a DCG that has the following features:
%1. handle subject/object distinction
%2. singular/plural distinction
%3. capable of producing parse trees
%4. make use of a separate lexicon (as shown in the starter code).
%Note: Do not change the lexicon in any way. This could cause the test cases to fail.
%As an example the following is the Prolog parse tree for the query
%"?- s(X,[he, eats, the, apple],[]).":
%X = s(np(pronoun(he, singular, subject)), vp(v(eats, singular), np(det(the, singular), n(apple, singular, object)))).

lex(the,det,_).
lex(a,det,singular).

lex(man,n,singular).
lex(men,n,plural).
lex(woman,n,singular).
lex(women,n,plural).
lex(apple,n,singular).
lex(apples,n,plural).
lex(pear,n,singular).
lex(pears,n,plural).
lex(table,n,singular).
lex(tables,n,plural).
lex(shower,n,singular).
lex(showers,n,plural).

lex(eat,v,plural).
lex(eats,v,singular).
lex(know,v,plural).
lex(knows,v,singular).

lex(i,pronoun,singular,subject).
lex(we,pronoun,plural,subject).
lex(me,pronoun,singular,object).
lex(us,pronoun,plural,object).
lex(you,pronoun,_,_).
lex(he,pronoun,singular,subject).
lex(she,pronoun,singular,subject).
lex(him,pronoun,singular,object).
lex(her,pronoun,singular,object).
lex(they,pronoun,plural,subject).
lex(them,pronoun,plural,object).
lex(it,pronoun,singular,_).

s(s(NP, VP)) --> np(NP, Q, subject), vp(VP, Q).
np(np(DET, N), _, Y) --> det(DET, Q), n(N, Q, Y).
np(np(PRONOUN), Q, X) --> pronoun(PRONOUN, Q, X).

vp(vp(V, NP), Q) --> v(V, Q), np(NP, _, object).
vp(vp(V), Q) --> v(V, Q).

det(det(W, Q), Q) --> [W], {lex(W, det, Q)}.
pronoun(pronoun(W, Q, X), Q, X) --> [W], {lex(W, pronoun, Q, X)}.
n(n(W, Q, Y), Q, Y) --> [W], {lex(W, n, Q)}.
v(v(W, Q), Q) --> [W], {lex(W, v, Q)}.

%Q3.5
%Use the given lexicon to create a DCG that has the same capabilities as that for problem q.4.5, that also has the following features:
%1.  handle adjectives
%2.  handle prepositional phrases
%For example, it should be able to handle noun phrases such as "the small frightened woman on the table" or "the big fat cow under the shower". Please be careful with your %parentheses. The test cases only have one set of parentheses around each piece of the sentence like in question 4. 
%Example 1:
%?- s(T,[the,small,young,woman,eats,the,pear,on,the,table],[]).
%T = s(np(det(the, singular), ap(adj(small), adj(young)), n(woman, singular, subject)), vp(v(eats, singular), np(det(the, singular), n(pear, singular, object), pp(prep(on), %np(det(the, singular), n(table, singular, object)))))) .
%Example 2:
%?- s(T,[the,big,fat,man,in,the,shower,knows,a,young,woman],[]).
%T = s(np(det(the, singular), ap(adj(big), adj(fat)), n(man, singular, subject), pp(prep(in), np(det(the, singular), n(shower, singular, object)))), vp(v(knows, singular), %np(det(a, singular), ap(adj(young)), n(woman, singular, object)))) .

%%%% lexicon %%%%%%


%% lex/3

%% determiners
lex(the,det,_).

lex(a,det,singular).



%% nouns

lex(man,n,singular).
lex(men,n,plural).

lex(woman,n,singular).

lex(women,n,plural).

lex(apple,n,singular).

lex(apples,n,plural).

lex(pear,n,singular).

lex(pears,n,plural).

lex(table,n,singular).

lex(tables,n,plural).

lex(shower,n,singular).

lex(showers,n,plural).



%% verbs (all present tense)

lex(eat,v,plural).

lex(eats,v,singular).

lex(know,v,plural).

lex(knows,v,singular).



%%lex/4
%% pronouns

lex(i,pronoun,singular,subject).

lex(we,pronoun,plural,subject).

lex(me,pronoun,singular,object).

lex(us,pronoun,plural,object).

lex(you,pronoun,_,_).
lex(he,pronoun,singular,subject).

lex(she,pronoun,singular,subject).

lex(him,pronoun,singular,object).

lex(her,pronoun,singular,object).

lex(they,pronoun,plural,subject).

lex(them,pronoun,plural,object).

lex(it,pronoun,singular,_).



% lex/2
%% adjectives 

lex(old,adj).

lex(young,adj).

lex(low,adj).

lex(high,adj).

lex(big,adj).

lex(fat,adj).

lex(small,adj).

lex(large,adj).

lex(frightened,adj).



% prepositions

lex(on,prep).

lex(in,prep).

lex(over,prep).

lex(under,prep).

lex(of,prep).



s(s(NP, VP)) --> np(NP, Q, subject), vp(VP, Q).


np(np(DET, N), Q, Y) --> det(DET, Q), n(N, Q, Y).

np(np(PRONOUN), Q, X) --> pronoun(PRONOUN, Q, X).

np(np(DET, ADJ, N), Q, Y) --> det(DET, Q), adj(ADJ), n(N, Q, Y).

np(np(DET, N, PP), Q, Y) --> det(DET, Q), n(N, Q, Y), pp(PP).

np(np(PRONOUN, PP), Q, Y) --> pronoun(PRONOUN, Q, Y), pp(PP).

np(np(DET, ADJ, N, PP), Q) --> det(DET, Q), adj(ADJ), n(N, Q, _), pp(PP).



pp(pp(PREP, NP)) --> prep(PREP), np(NP, _, object).

pp(pp(PREP, VP)) --> prep(PREP), vp(VP, _).



vp(vp(V, NP), Q) --> v(V, Q), np(NP, _, object).

vp(vp(V), Q) --> v(V, Q).



det(det(W, Q), Q) --> [W], {lex(W, det, Q)}.

pronoun(pronoun(W, Q, X), Q, X) --> [W], {lex(W, pronoun, Q, X)}.

n(n(W, Q, Y), Q, Y) --> [W], {lex(W, n, Q)}.

v(v(W, Q), Q) --> [W], {lex(W, v, Q)}.


adj(ap(adj(W))) --> [W], {lex(W, adj)}.

adj(ap(adj(W),adj(Wa))) --> [W,Wa], {lex(W, adj), lex(Wa, adj)}.

prep(prep(W)) --> [W], {lex(W, prep)}.

%Q3.6
%Our previous representation of parse trees is a little hard to read. It would be nice if we could display them like this:
%
%s(
%   np(
%      det(a)
%      n(man)
%
%   )
%   vp(
%      v(shoots)
%      np(
%         det(a)
%         n(woman)
%
%      )
%
%   )
%
%)
%
%This representation is much easier to understand. Using indentation we can see that s is the root and it has to children, np and vp, and so on. In order to do this you will %need to understand the write/1 predicate. A detailed explanation of the write/1 predicate is given on page 180 of the LPN textbook, at the beginning of the Practical Session %in Ch. 9. Once you understand the write/1 predicate please create a predicate ppTree/1 that pretty prints a complex term representing a parse tree in the way shown above. %Make sure to use tabs to indicate depth within the tree as shown above.
%Please be aware that as of Prolog Version 7 a list actually has the functor '[|]' instead of . so a list [a,b] is actually represented as '[|]'(a, '[|]'(b, [])). and not .%(a,.(b, [])). Mimir uses Prolog Version 7.2.3 and most of you probably have a newer version so your compilers and Mimir's will not recognize the . as the functor for lists %anymore.

printlist([], Indent).
printlist([X|List], Indent) :-
    tab(Indent), write(X),nl,
    printlist(List, Indent).

termtype(Term, atom) :-
  atom(Term).
termtype(Term, integer) :-
  integer(Term).
termtype(Term, number) :-
  number(Term).
termtype(Term, constant) :-
  atomic(Term).
termtype(Term, variable) :-
  var(Term).

termtype(Term, complex_term) :-
  nonvar(Term),
  functor(Term, _, A),
  A > 0.

termtype(Term, simple_term) :-
  termtype(Term, variable).
termtype(Term, simple_term) :-
  termtype(Term, constant).

termtype(Term, term) :-
  termtype(Term, simple_term).
termtype(Term, term) :-
  termtype(Term, complex_term).

printterm(Term, Indent) :-
  termtype(Term, simple_term),
  tab(Indent), write(Term).

printterm(Term, Indent) :-
  termtype(Term, complex_term),
  Term =.. [TermName | TermArguments],
  checkSimpleTypes(TermArguments),
  tab(Indent), write(TermName),
  write('('), nl, printlist(TermArguments, Indent + 1),
  tab(Indent), write(')').

checkSimpleTypes([]).

checkSimpleTypes([Head | Tail]) :-
  termtype(Head, simple_term),
  checkSimpleTypes(Tail).

printterm(Term, Indent) :-
  termtype(Term, complex_term),
  Term =.. [TermName | TermArguments],
  not(checkSimpleTypes(TermArguments)),
  tab(Indent), write(TermName), write('('), nl,
  NewIndent is Indent + 1,
  iterateArguments(TermArguments, NewIndent),
  nl, tab(Indent), write(')').

iterateArguments([Head], Indent) :-
  printterm(Head, Indent).

iterateArguments([Head | Tail], Indent) :-
  Tail \= [],
  printterm(Head, Indent), nl,
  iterateArguments(Tail, Indent).

ppTree(Term) :-
  printterm(Term, 0).
