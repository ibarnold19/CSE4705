%Homework 6
%Bryan Arnold
%CSE 4705

%Q7.1
%The connectives nand and nor are defined by the following truth tables:
%p         q         p nand q         p nor q
%------------------------------------------------------------------------------------------
%T         T            F                F
%T         F            T                F
%F         T            T                F
%F         F            T                T
%Expand the code for the tableau method implementation, to include implementations for expansion rules for thes two connectives. Your code should give the following results %when querying the predicate tprove/1, with the following formulas:
%**** Your code will be tested against these queries, and other ones as well. ****
%?- tprove(imp(nand(p,q),or(not(p),not(q))),Result). %% should return Result = 'Theorem'.
%?- tprove(imp(nand(p,q),and(not(p),not(q))),Result). %% should return Result = 'Not a Theorem'.
%?- tprove(imp(nor(p,q),or(not(p),not(q))),Result). %% should return Result = 'Theorem'.
%?- tprove(imp(nor(p,q),and(not(p),not(q))),Result). %% should return Result = 'Theorem'.
%?- tprove(imp(not(p),nand(p,q)),Result). %% should return Result = 'Theorem'.
%?- tprove(imp(not(p),nor(p,q)),Result). %% should return Result = 'Not a Theorem'.

%% ********************************* STARTER CODE ************************************
%%
%% contains code for propositional tableau method
%%

tprove(Formula,'Theorem') :-
	closedTableau([[f(Formula)]]),!.
tprove(Formula,'Not a Theorem') :-
	\+ closedTableau([[f(Formula)]]).

%% closedTableau returns true for an empty list, meaning
%% all formerly present branches were removed by the removeClosedBranches/2
%% predicate because they all had opposite-signed formulas.
closedTableau([]) :- !.
closedTableau(OldTableau) :-
    expand(OldTableau,TempTableau),
    removeClosedBranches(TempTableau,NewTableau),
    closedTableau(NewTableau).



%% removeClosedBranches/2 removes those branches in the tableau
%% list that have opposite-signed formulas.

removeClosedBranches([],[]).
removeClosedBranches([Branch|Rest],Tableau) :-
    closedBranch(Branch),
    !,
    removeClosedBranches(Rest,Tableau).
removeClosedBranches([Branch|Rest],[Branch|Tableau]) :-
    \+ closedBranch(Branch),
    !,
    removeClosedBranches(Rest,Tableau).


%% closedBranch/1 returns true if Branch contains two opposite-signed
%% formulas.

closedBranch(Branch) :-
    member(t(X),Branch),
    member(f(X),Branch).


%% expand/2 expands (only) the first branch of the tableau,
%% using any of the three expansion predicates.  If none
%% of those apply, then recursively call into the rest of the
%% list.
%% Note this predicate is *not* recursive.

expand([Branch|Tableau],[NewBranch|Tableau]) :-
    unaryExpansion(Branch,NewBranch),
    !.
expand([Branch|Tableau],[NewBranch|Tableau]) :-
    conjunctiveExpansion(Branch,NewBranch),
    !.
expand([Branch|Tableau],[NewBranch1,NewBranch2|Tableau]) :-
    disjunctiveExpansion(Branch,NewBranch1,NewBranch2),
    !.
expand([Branch|Rest],[Branch|NewRest]) :-
    expand(Rest,NewRest).

%% these are the expansion predicates for each of the unary,
%% conjunctive, and disjunctive expansions in the tableau
%% method.

unaryExpansion(Branch,[Component|Temp]) :-
    unary(SignedFormula,Component),
    removeFirst(SignedFormula,Branch,Temp).

conjunctiveExpansion(Branch,[Comp1,Comp2|Temp]) :-
    conjunctive(SignedFormula,Comp1,Comp2),
    removeFirst(SignedFormula,Branch,Temp).

disjunctiveExpansion(Branch,[Comp1|Temp],[Comp2|Temp]) :-
    disjunctive(SignedFormula,Comp1,Comp2),
    removeFirst(SignedFormula,Branch,Temp).

%% these predicates give the rules for replacement
%% of terms in the tableau method.

unary(t(not(X)),f(X)).
unary(f(not(X)),t(X)).

conjunctive(t(and(X,Y)),t(X),t(Y)).
conjunctive(f(or(X,Y)),f(X),f(Y)).
conjunctive(f(imp(X,Y)),t(X),f(Y)).

%% Added rules to handle nand and nor
conjunctive(t(nand(X,Y)), f(X), t(Y)).
conjunctive(t(nand(X,Y)), t(X), f(Y)).
conjunctive(t(nand(X,Y)), f(X), f(Y)).
conjunctive(f(nand(X,Y)), t(X), t(Y)).
conjunctive(t(nor(X,Y)), f(X), f(Y)).

disjunctive(f(and(X,Y)),f(X),f(Y)).
disjunctive(t(or(X,Y)),t(X),t(Y)).
disjunctive(t(imp(X,Y)),f(X),t(Y)).

%% Added rules to handle nor
disjunctive(f(nor(X,Y)), t(X), t(Y)).
disjunctive(f(nor(X,Y)), t(X), f(Y)).
disjunctive(f(nor(X,Y)), f(X), t(Y)).

%% removeFirst/2 removes the first occurrence of an element,
%% X from a list.
removeFirst(X,[X|Tail],Tail) :- !.
removeFirst(X,[Head|Tail],[Head|NewTail]):-
   removeFirst(X,Tail,NewTail).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% END STARTER CODE ********************************

%Q7.2
%Consider the boolean constants, true and false. In the context of our automated theorem proving methdology, true represents "always true", i.e., true in every model, while %false represents "always false", i.e., false in every model.
%Expand the code for the tableau method implementation, (given below as starting code), to include implementations for expansion rules for these two boolean constants, true %and false.
%Your code should give the following results when querying the predicate tprove/1, with the following formulas:
%**** Your code will be tested against these queries, and other ones as well. *****
%?- tprove(true,Result). %% should return Result = 'Theorem'.
%?- tprove(false,Result). %% should return Result = 'Not a Theorem'.
%?- tprove(or(false,true),Result). %% should return Result = 'Theorem'.
%?- tprove(and(false,true),Result). %% should return Result = 'Not a Theorem'.
%?- tprove(and(true,imp(q,imp(p,q))),Result). %% should return Result = 'Theorem'.
%?- tprove(and(false,imp(q,imp(p,q))),Result). %% should return Result = 'Not a Theorem'.
%?- tprove(or(true,imp(q,imp(p,q))),Result). %% should return Result = 'Theorem'.
%?- tprove(or(false,imp(q,imp(p,q))),Result). %% should return Result = 'Theorem'.

%% ********************************* STARTER CODE ************************************
%%
%% contains code for propositional tableau method
%%

tprove(Formula,'Theorem') :-
	closedTableau([[f(Formula)]]),!.
tprove(Formula,'Not a Theorem') :-
	\+ closedTableau([[f(Formula)]]).

%% closedTableau returns true for an empty list, meaning
%% all formerly present branches were removed by the removeClosedBranches/2
%% predicate because they all had opposite-signed formulas.
closedTableau([]) :- !.
closedTableau(OldTableau) :-
    expand(OldTableau,TempTableau),
    removeClosedBranches(TempTableau,NewTableau),
    closedTableau(NewTableau).



%% removeClosedBranches/2 removes those branches in the tableau
%% list that have opposite-signed formulas.

removeClosedBranches([],[]).
removeClosedBranches([Branch|Rest],Tableau) :-
    closedBranch(Branch),
    !,
    removeClosedBranches(Rest,Tableau).
removeClosedBranches([Branch|Rest],[Branch|Tableau]) :-
    \+ closedBranch(Branch),
    !,
    removeClosedBranches(Rest,Tableau).


%% closedBranch/1 returns true if Branch contains two opposite-signed
%% formulas.

closedBranch(Branch) :-
    member(t(X),Branch),
    member(f(X),Branch).


%% expand/2 expands (only) the first branch of the tableau,
%% using any of the three expansion predicates.  If none
%% of those apply, then recursively call into the rest of the
%% list.

expand([Branch|Tableau],[NewBranch|Tableau]) :-
    unaryExpansion(Branch,NewBranch),
    !.
expand([Branch|Tableau],[NewBranch|Tableau]) :-
    conjunctiveExpansion(Branch,NewBranch),
    !.
expand([Branch|Tableau],[NewBranch1,NewBranch2|Tableau]) :-
    disjunctiveExpansion(Branch,NewBranch1,NewBranch2),
    !.
expand([Branch|Rest],[Branch|NewRest]) :-
    expand(Rest,NewRest).


%% these are the expansion predicates for each of the unary,
%% conjunctive, and disjunctive expansions in the tableau
%% method.

unaryExpansion(Branch,[Component|Temp]) :-
    unary(SignedFormula,Component),
    removeFirst(SignedFormula,Branch,Temp).

conjunctiveExpansion(Branch,[Comp1,Comp2|Temp]) :-
    conjunctive(SignedFormula,Comp1,Comp2),
    removeFirst(SignedFormula,Branch,Temp).

disjunctiveExpansion(Branch,[Comp1|Temp],[Comp2|Temp]) :-
    disjunctive(SignedFormula,Comp1,Comp2),
    removeFirst(SignedFormula,Branch,Temp).

%% these predicates give the rules for replacement
%% of terms in the tableau method.

unary(t(not(X)),f(X)).
unary(f(not(X)),t(X)).

%% Added rules to suppor true/false
conjunctive(f(true), t(X), f(X)).
conjunctive(t(true), t(X), f(X)).

conjunctive(t(and(X,Y)),t(X),t(Y)).
conjunctive(f(or(X,Y)),f(X),f(Y)).
conjunctive(f(imp(X,Y)),t(X),f(Y)).

disjunctive(f(and(X,Y)),f(X),f(Y)).
disjunctive(t(or(X,Y)),t(X),t(Y)).
disjunctive(t(imp(X,Y)),f(X),t(Y)).

%% removeFirst/2 removes the first occurrence of an element,
%% X from a list.
removeFirst(X,[X|Tail],Tail) :- !.
removeFirst(X,[Head|Tail],[Head|NewTail]):-
   removeFirst(X,Tail,NewTail).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% END STARTER CODE ********************************

%Q7.3
%Modify the code so that the propositional tableau prover handles the bi-implication connective <->. Use say, bimp/2 as the Prolog notation for this connective.
%Your code should give the following results when querying the predicate tprove/1, with the following formulas:
%**** Your code will be tested against these queries, and other ones as well. *****
%?- tprove(bimp(not(or(p,q)),and(not(p),not(q))),Result). %% should return Result = 'Theorem'.
%?- tprove(bimp(not(and(p,q)),or(not(p),not(q))),Result). %% should return Result = 'Theorem'.
%?- tprove(bimp(not(not(p)),p),Result). %% should return Result = 'Theorem'.
%?- tprove(bimp(p,q),Result). %% should return Result = 'Not a Theorem'.
%?- tprove(bimp(p,not(p)),Result). %% should return Result = 'Not a Theorem'.

%% ********************************* STARTER CODE ************************************
%%
%% contains code for propositional tableau method
%%

tprove(Formula,'Theorem') :-
	closedTableau([[f(Formula)]]),!.
tprove(Formula,'Not a Theorem') :-
	\+ closedTableau([[f(Formula)]]).

%% closedTableau returns true for an empty list, meaning
%% all formerly present branches were removed by the removeClosedBranches/2
%% predicate because they all had opposite-signed formulas.
closedTableau([]) :- !.
closedTableau(OldTableau) :-
    expand(OldTableau,TempTableau),
    removeClosedBranches(TempTableau,NewTableau),
    closedTableau(NewTableau).

%% removeClosedBranches/2 removes those branches in the tableau
%% list that have opposite-signed formulas.

removeClosedBranches([],[]).
removeClosedBranches([Branch|Rest],Tableau) :-
    closedBranch(Branch),
    !,
    removeClosedBranches(Rest,Tableau).
removeClosedBranches([Branch|Rest],[Branch|Tableau]) :-
    \+ closedBranch(Branch),
    !,
    removeClosedBranches(Rest,Tableau).


%% closedBranch/1 returns true if Branch contains two opposite-signed
%% formulas.

closedBranch(Branch) :-
    member(t(X),Branch),
    member(f(X),Branch).

%% expand/2 expands (only) the first branch of the tableau,
%% using any of the three expansion predicates.  If none
%% of those apply, then recursively call into the rest of the
%% list.

expand([Branch|Tableau],[NewBranch|Tableau]) :-
    unaryExpansion(Branch,NewBranch),
    !.
expand([Branch|Tableau],[NewBranch|Tableau]) :-
    conjunctiveExpansion(Branch,NewBranch),
    !.
expand([Branch|Tableau],[NewBranch1,NewBranch2|Tableau]) :-
    disjunctiveExpansion(Branch,NewBranch1,NewBranch2),
    !.
%% this is a mod for this q_7_3_solution - support bi-implication.
expand([Branch|Tableau],[NewBranch|Tableau]) :-
    bidisjunctiveExpansion(Branch,NewBranch),
    !.
expand([Branch|Rest],[Branch|NewRest]) :-
    expand(Rest,NewRest).

%% these are the expansion predicates for each of the unary,
%% conjunctive, and disjunctive expansions in the tableau
%% method.

unaryExpansion(Branch,[Component|Temp]) :-
    unary(SignedFormula,Component),
    removeFirst(SignedFormula,Branch,Temp).

conjunctiveExpansion(Branch,[Comp1,Comp2|Temp]) :-
    conjunctive(SignedFormula,Comp1,Comp2),
    removeFirst(SignedFormula,Branch,Temp).

disjunctiveExpansion(Branch,[Comp1|Temp],[Comp2|Temp]) :-
    disjunctive(SignedFormula,Comp1,Comp2),
    removeFirst(SignedFormula,Branch,Temp).

bidisjunctiveExpansion(Branch,[Comp1,Comp2|Temp]) :-
    conjunctive(SignedFormula,Comp1,Comp2),
    removeFirst(SignedFormula,Branch,Temp).

%% these predicates give the rules for replacement
%% of terms in the tableau method.

unary(t(not(X)),f(X)).
unary(f(not(X)),t(X)).

%% Added rules to support biconditional
conjunctive(t(bimp(X,Y)), t(X), t(Y)).
conjunctive(t(bimp(X,Y)), f(X), f(Y)).
conjunctive(f(bimp(X,Y)), t(X), f(Y)).
conjunctive(f(bimp(X,Y)), f(X), t(Y)).

conjunctive(t(and(X,Y)),t(X),t(Y)).
conjunctive(f(or(X,Y)),f(X),f(Y)).
conjunctive(f(imp(X,Y)),t(X),f(Y)).

disjunctive(f(and(X,Y)),f(X),f(Y)).
disjunctive(t(or(X,Y)),t(X),t(Y)).
disjunctive(t(imp(X,Y)),f(X),t(Y)).

%% removeFirst/2 removes the first occurrence of an element,
%% X from a list.
removeFirst(X,[X|Tail],Tail) :- !.
removeFirst(X,[Head|Tail],[Head|NewTail]):-
   removeFirst(X,Tail,NewTail).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% END STARTER CODE ********************************\

%Q7.4
%Modify the code so that the propositional resolution prover handles the bi-implication connective <->. Use say, bimp/2 as the Prolog notation for this connective.
%Your code should give the following results when querying the predicate rprove/1, with the following formulas:
%**** Your code will be tested against these queries, and other ones as well. *****
%?- rprove(bimp(not(or(p,q)),and(not(p),not(q))),Result). %% should return Result = 'Theorem'.
%?- rprove(bimp(not(and(p,q)),or(not(p),not(q))),Result). %% should return Result = 'Theorem'.
%?- rprove(bimp(not(not(p)),p),Result). %% should return Result = 'Theorem'.
%?- rprove(bimp(p,q),Result). %% should return Result = 'Not a Theorem'.
%?- rprove(bimp(p,not(p)),Result). %% should return Result = 'Not a Theorem'.

%% q_7_4_solution.pl
%%
%% author:  Joe Johnson
%%
%% Modify the code so that the propositional resolution prover handles the bi-implication
%% connective <->.  Use say, bimp/2 as the Prolog notation for this connective.
%%
%% Your code should give the following results when querying the predicate
%% rprove/1, with the following formulas:
%%
%% **** Your code will be tested against these queries, and other ones as well. *****
%%
%% ?- rprove(bimp(not(or(p,q)),and(not(p),not(q))),Result).  %% should return Result = 'Theorem'.
%%
%% ?- rprove(bimp(not(and(p,q)),or(not(p),not(q))),Result).  %% should return Result = 'Theorem'.
%%
%% ?- rprove(bimp(not(not(p)),p),Result).  %% should return Result = 'Theorem'.
%%
%% ?- rprove(bimp(p,q),Result).  %% should return Result = 'Not a Theorem'.
%%
%% ?- rprove(bimp(p,not(p)),Result).  %% should return Result = 'Not a Theorem'.
%%


%% ********************************* STARTER CODE ************************************
%%
%% contains code for the following:
%% * code for conversion of formula to NNF, and then to CNF (from cnf.pl)
%% * propositional resolution theorem prover (from propResolution.pl)
%% * utility predicates (from comSemPredicates.pl)
%%


%% first, code for converting formula to NNF, CNF:

cnf(Formula,SetCNF):-
   nnf(Formula,NNF),
   nnf2cnf([[NNF]],[],CNF),
   setCnf(CNF,SetCNF).

nnf(not(and(F1,F2)),or(N1,N2)):- 
   nnf(not(F1),N1), 
   nnf(not(F2),N2).
   
nnf(and(F1,F2),and(N1,N2)):- 
   nnf(F1,N1),
   nnf(F2,N2).
   
nnf(not(or(F1,F2)),and(N1,N2)):-
   nnf(not(F1),N1),
   nnf(not(F2),N2).
   
nnf(or(F1,F2),or(N1,N2)):-
   nnf(F1,N1),
   nnf(F2,N2).

nnf(not(imp(F1,F2)),and(N1,N2)):- 
   nnf(F1,N1),
   nnf(not(F2),N2).
   
nnf(imp(F1,F2),or(N1,N2)):- 
   nnf(not(F1),N1),
   nnf(F2,N2).

%% *************************************************************************
%% code to support bi-implication - this is the only modification needed
%% to support bimp.

nnf(bimp(F1, F2), or(N1, N2)) :-
    nnf(and(F1, F2), N1),
    nnf(and(not(F1), not(F2)), N2).

nnf(not(bimp(F1, F2)), or(N1, N2)) :-
    nnf(and(F1, not(F2)), N1),
    nnf(and(F2, not(F1)), N2).

%% _________________________________________________________________________
%% _________________________________________________________________________
%% _________________________________________________________________________

%% *************************************************************************

nnf(not(not(F1)),N1):-
   nnf(F1,N1).
   
nnf(F1,F1):-
   literal(F1).


/*========================================================================
   Literals
========================================================================*/

literal(not(F)):- atomic(F).
literal(F):- atomic(F).


/*========================================================================
   Convert From Negative Normal Form to Conjunctive Normal Form
========================================================================*/

nnf2cnf([],_,[]).

nnf2cnf([[]|Tcon],Lit,[Lit|NewTcon]):-
   !,
   nnf2cnf(Tcon,[],NewTcon).

nnf2cnf([[and(F1,F2)|Tdis]|Tcon],Lits,Output):-
   !,
   append(Tdis,Lits,Full),
   nnf2cnf([[F1|Full],[F2|Full]|Tcon],[],Output).

nnf2cnf([[or(F1,F2)|Tdis]|Tcon],Lits,Output):-
   !,
   nnf2cnf([[F1,F2|Tdis]|Tcon],Lits,Output).

nnf2cnf([[Lit|Tdis]|Tcon],Lits,Output):-
   nnf2cnf([Tdis|Tcon],[Lit|Lits],Output).


/*========================================================================
   Remove Duplicates
========================================================================*/

setCnf(Cnf1,Cnf2):-
   setCnf(Cnf1,[],Cnf2).

setCnf([],Cnf1,Cnf2):-
   removeDuplicates(Cnf1,Cnf2).

setCnf([X1|L1],L2,L3):-
   removeDuplicates(X1,X2),
   setCnf(L1,[X2|L2],L3).


%% next, code for resolution theorem prover:

rprove(Formula):-
   cnf(not(Formula),CNF),
   refute(CNF).

rprove(Formula,Result):-
   (  
      rprove(Formula), !,
      Result='Theorem'
   ;
      Result='Not a theorem'
   ).
   

/*========================================================================
   Refute
========================================================================*/

refute(C):-
   member([],C).

refute(C):-
   \+ member([],C),
   resolveList(C,[],Output),
   unionSets(Output,C,NewC),
   \+ NewC = C, 
   refute(NewC).


/*========================================================================
    Resolve a list of clauses
========================================================================*/

resolveList([],Acc,Acc).

resolveList([Clause|List],Acc1,Acc3):-
   resolveClauseList(List,Clause,Acc1,Acc2),
   resolveList(List,Acc2,Acc3).


/*========================================================================
   Resolve a clause against a list
========================================================================*/

resolveClauseList([],_,Acc,Acc).

resolveClauseList([H|L],Clause,Acc1,Acc3):-
   resolve(Clause,H,Result), 
   unionSets([Result],Acc1,Acc2),
   resolveClauseList(L,Clause,Acc2,Acc3).

resolveClauseList([H|L],Clause,Acc1,Acc2):-
   \+ resolve(Clause,H,_),
   resolveClauseList(L,Clause,Acc1,Acc2).


/*========================================================================
   Resolve two clauses
========================================================================*/

resolve(Clause1,Clause2,NewClause):-
   selectFromList(Lit,Clause1,Temp1),
   selectFromList(not(Lit),Clause2,Temp2), 
   unionSets(Temp1,Temp2,NewClause).

resolve(Clause1,Clause2,NewClause):-
   selectFromList(not(Lit),Clause1,Temp1),
   selectFromList(Lit,Clause2,Temp2),
   unionSets(Temp1,Temp2,NewClause).


selectFromList(X,[X|L],L).
selectFromList(X,[Y|L1],[Y|L2]):-
   selectFromList(X,L1,L2).



%% finally, utility predicates:

removeFirst(X,[X|Tail],Tail) :- !.
removeFirst(X,[Head|Tail],[Head|NewTail]):-
   removeFirst(X,Tail,NewTail).


removeDuplicates([],[]).

removeDuplicates([X|L],Pruned):-
	member(Y,L), X==Y, !,
	removeDuplicates(L,Pruned).

removeDuplicates([X|L],[X|Pruned]):-
	removeDuplicates(L,Pruned).


unionSets([],L,L).

unionSets([X|L1],L2,L3):-
   member(Y,L2), 
   X==Y, !,
   unionSets(L1,L2,L3).

unionSets([X|L1],L2,[X|L3]):-
   unionSets(L1,L2,L3).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% END STARTER CODE ********************************
