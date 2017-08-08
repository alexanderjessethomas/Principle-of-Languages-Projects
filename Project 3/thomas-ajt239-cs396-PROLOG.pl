% Student Name: Alex Thomas
% Nau Student ID: ajt239
% Professor Name: Dr. Eck Doerry
% Class: CS396
% Date: 5/4/2016


%This handles if you multiply by 0
multn(0,_,Z):-
	Z = 0.
%This is the actual multiplication function
% X and Y are input where Z is the output
multn(X, Y, Z):-
	X > 0,
	X1 is X -1,
	multn(X1, Y, Z1),
	Z is  Z1 + Y.

%This is the ndelete function as it was defined in the assignment
ndelete(Number, LIST, OUTPUT_LIST) :-
    ndelete(Number, 1, LIST, OUTPUT_LIST).

%This is the base case for the ndelete function
ndelete(1, _, _, OUTPUT_LIST) :-
    OUTPUT_LIST = [],
    !.
% If the list that is inputed is empty this returns an empty list.
ndelete(_, _, [], OUTPUT_LIST) :-
    OUTPUT_LIST = [].

%Another ndelete helper function
ndelete(Number, Number, [_|T], OUTPUT_LIST) :-
    ndelete(Number, 1, T, OtherOUTPUT_LIST),
    OUTPUT_LIST = OtherOUTPUT_LIST,
    !.

% This is not actually deleting it is just skipping the targeted
% elements in the list
ndelete(Number, Counter, [H|T], OUTPUT_LIST) :-
    New_Counter is Counter + 1,
    ndelete(Number, New_Counter, T, OUTPUT_LIST2),
    OUTPUT_LIST = [H|OUTPUT_LIST2],
    !.

%Start of Social Network Code
:- use_module(library(lists)).
%rule that Doerry Gives
knows(A,B) :-
	link(A, B);
	link(B, A).

% Define a functor called connect and return if there is a chain
% of people that connect the two people that are the parameters together
connect(A,B) :-
	connect(A,B, [], OUTPUT),
	write(OUTPUT).

% This is saying that if there is alink between two people they are
% connected.
connect(A,B, CONNECTIONS, CONNECTION_CHAIN) :-
	not(member(A, CONNECTIONS)),
	knows(A,B),
	CONNECTION_CHAIN = [A,B].

%This is the recursive case
% Two people are recursively connected when the first parameter person
% knows a bunch of other people. Which hopefully the last person that is
% made by the connections is the targeted person parameter.
connect(A,B, CONNECTIONS, CONNECTION_CHAIN) :-
	not(member(A, CONNECTIONS)),
	NEXT_CONNECTION= [A|CONNECTIONS],
	knows(A,C),
	C \= B,
	connect(C,B, NEXT_CONNECTION, CONNECTION_CHAIN2),
	CONNECTION_CHAIN = [A|CONNECTION_CHAIN2].

%Links for testing
%link(zach, charles).
%link(kyle, colter).
%link(kyle, garret).
%link(kyle, zach).
%link(garret, rachel).
%link(dillon, luther).
%link(dillon, brandon).
%link(colter, brandon).
%link(colter, dillon).
%link(doug, luther).
%link(doug, kyle).
%link(charles, garret).
%link(charles, colter).
