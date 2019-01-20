%Homework 1
%Bryan Arnold
%CSE 4705

%Q1.1
%Represent the following in Prolog:  Butch is a killer.
killer(butch).

%Q1.2
%Represent the following in Prolog:  Mia and Marcellus are married.
married(mia, marcellus).

%Q1.3
%Represent the following in Prolog:  Zed is dead.
dead(zed).

%Q1.4
%Represent the following in Prolog:  Marcellus kills everyone who gives Mia a footmassage.
footmassage(yolanda,jody).

footmassage(john,mia).

footmassage(gertude,mia).

footmassage(john,jerry).


kills(marcellus, X) :- footmassage(X, mia).

%Q1.5
%Represent the following in Prolog:  Mia loves everyone who is a good dancer.
badDancer(gertrude).

goodDancer(matilda).

goodDancer(john).

goodDancer(yolanda).


love(mia, X) :- goodDancer(X).

%Q1.6
%Represent the following in Prolog:  Jules eats anything that is nutritious or tasty.
nonnutritious(bubblegum).

nutritious(raisins).

gross(broccoli).

tasty(peanutbutter).

nutritious(broccoli).


eat(jules, X) :- nutritious(X); tasty(X).
