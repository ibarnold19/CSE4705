%Homework 2
%Bryan Arnold
%CSE 4705

%Q2.1
%Below is a tiny lexicon (that is, information about individual words).  Write a mini grammar consisting of one syntactic rule, a predicate, sentence%(Word1,Word2,Word3,Word4,Word5), which defines a sentence to be an entity consisting of five words in the following order: 
%a determiner, a noun, a verb, a determiner, a noun

word(determiner,a).

word(detemriner,every).

word(noun,criminal).

word(noun,'big kahuna burger').

word(verb,eats).

word(verb,likes).



sentence(Word1, Word2, Word3, Word4, Word5) :- word(determiner, Word1),

					       word(noun, Word2),

					       word(verb, Word3),
    
					       word(determiner, Word4),
    
					       word(noun, Word5).

%Q2.2
%Here are six Italian words:  astante, astoria, baratto, cobalto, pistola, statale.  
%They are to be arranged, crossword puzzle fashion in the following grid:
%The knowledge base in the starter code represents a lexicon containing these Italian words.  
%Next, the \= operator is the "not unifies" operator.  So, for example, the predicate,
%diff(X,Y) :- X\=Y returns true if X does not unify with Y.  For example, diff(john,ted) returns True while diff(john,john) returns false (Try it! - but you need to define %the diff predicate and load it in to prolog to make this happen).
%Anyways, write a predicate based on the lexicon below that renders these words in crossword above.  But further, utilize the \= operator so that each word is placed in the %puzzle exactly once.  (Without the \= operator, the code will return arrangements in which one word could be placed in multiple locations, such as baratta in position H1 as %well as V1 and V2).  The first three arguments should be the vertical words from left to right, adn the last three arguments should be horizontal words from top to bottom.

word(astante, a,s,t,a,n,t,e).

word(astoria, a,s,t,o,r,i,a).

word(baratto, b,a,r,a,t,t,o).

word(cobalto, c,o,b,a,l,t,o).

word(pistola, p,i,s,t,o,l,a).

word(statale, s,t,a,t,a,l,e).



crossword(X1, X2, X3, Y1, Y2, Y3) :-
 word(X1, _, L1, _, L2, _, L3, _),

				     word(X2, _, L4, _, L5, _, L6, _),

				     word(X3, _, L7, _, L8, _, L9, _),
    
				     word(Y1, _, L1, _, L4, _, L7, _),
    
				     word(Y2, _, L2, _, L5, _, L8, _),
    
				     word(Y3, _, L3, _, L6, _, L9, _),
    
				     X1 \= Y1, X2 \= Y2, X3 \= Y3.

%Q2.3
%Given the knowledge base of travel information, shown below.  Write a predicate travel/2 which determines whether it is possible to travel from one place to another by %chaining together car, train, and plane journeys.  For example, your program should answer yes to the query, travel(valmont,raglan).

byCar(auckland,hamilton). 

byCar(hamilton,raglan). 

byCar(valmont,saarbruecken). 

byCar(valmont,metz). 



byTrain(metz,frankfurt).

byTrain(saarbruecken,frankfurt). 

byTrain(metz,paris). 

byTrain(saarbruecken,paris). 

    
    

byPlane(frankfurt,bangkok). 

byPlane(frankfurt,singapore). 

byPlane(paris,losAngeles). 

byPlane(bangkok,auckland). 

byPlane(singapore,auckland). 

byPlane(losAngeles,auckland).



step(X, Y) :- byCar(X, Y).

step(X, Y) :- byTrain(X, Y).

step(X, Y) :- byPlane(X, Y).


travel(X, Y) :- step(X, Y).

travel(X, Y) :- step(X, Z), travel(Z, Y).

%Q2.4
%So, by using travel/2 to query the above database, you can find out it is possible to go from Valmont to Raglan.  If you are planning such a voyage, that's already something %useful to know, but you would probably prefer to have the precise route from Valmont to Raglan.  Write a prediate, travel/3 which tells you which route to take when %traveling from one place to another.  For example, the program should respond, X = go(valmont,metz, go(metz,paris, go(paris,losAngeles))) to the query travel%(valmont,losAngeles,X).
%Important:  Make sure to follow the following rules for specifying the clauses of your predicate:
%1.  All non-recursive rules should be listed above recursive rules.
%2.  For both non-recursive rules and for recursive rules, the following ordering of rules should be observed:  a rule for byCar, then a rule for byTrain, then a rule %for byPlane.

byCar(auckland,hamilton). 

byCar(hamilton,raglan). 

byCar(valmont,saarbruecken). 

byCar(valmont,metz).
 
    

byTrain(metz,frankfurt). 

byTrain(saarbruecken,frankfurt). 

byTrain(metz,paris). 

byTrain(saarbruecken,paris). 
 
   

byPlane(frankfurt,bangkok). 

byPlane(frankfurt,singapore). 

byPlane(paris,losAngeles). 

byPlane(bangkok,auckland). 

byPlane(singapore,auckland). 

byPlane(losAngeles,auckland).



step(X, Y) :- byCar(X, Y).

step(X, Y) :- byTrain(X, Y).

step(X, Y) :- byPlane(X, Y).
travel(X, Y, go(X, Y)) :- step(X, Y).

travel(X, Y, go(X, Z, Route)) :- step(X, Z),
    travel(Z, Y, Route).

%Q2.5
%Suppose we are given a knowledge base with the facts as given in the starter code.  Write a predicate listtran(G,E) which translates a list of German number words to the %corresponding list of English number words.  For example, listtran([eins,neun,zwei],X) should give X = [one,nine,two].  Your program should also work in the other direction. %For example if you give it the query listtran(X,[one, seven,six,two]) it should return X = [eins,sieben,sechs,zwei].

tran(eins,one).

tran(zwei,two).

tran(drei,three).

tran(vier,four).

tran(fuenf,five).

tran(sechs,six).

tran(sieben,seven).

tran(acht,eight).

tran(neun,nine).



listtran([], []).

listtran([X | W1], [Y | W2]) :- 
tran(X, Y),
 listtran(W1, W2).

%Q2.6
%Write a 3-place predicate combine1 which takes three lists as arguments and combines the elements of the first two lists into the third as follows:
%combine1([a,b,c],[1,2,3],X).
%X = [a,1,b,2,c,3].
%combine1([f,b,yip,yup],[glu,gla,gli,glo],Result).
%Result = [f,glu,b,gla,yip,gli,yup,glo].

combine1([], [], []).

combine1([X1 | Y1], [X2 | Y2], [X1, X2 | Y3]) :-
 combine1(Y1, Y2, Y3).

%Q2.7
%Now write a 3-place predicate combine2 which takes three lists as arguments and combines the elements of the first two lists into the third as follows:
%combine2([a,b,c],[1,2,3],X).
%X = [[a,1],[b,2],[c,3]].
%combine2([f,b,yip,yup],[glu,gla,gli,glo],Result).
%Result = [[f,glu],[b,gla],[yip,gli],[yup,glo]].

combine2([], [], []).

combine2([X1 | Y1], [X2 | Y2], [[X1, X2] | Y3]) :-
 combine2(Y1, Y2, Y3).

%Q2.8
%Finally, write a 3-place predicate combine3 which takes three lists as arguments and combines the elements of the first two lists into the third as follows:
%combine3([a,b,c],[1,2,3],X).
%X = [j(a,1),j(b,2),j(c,3)].

combine3([], [], []).

combine3([X1 | Y1], [X2 | Y2], [j(X1, X2) | Y3]) :-
 combine3(Y1, Y2, Y3).
