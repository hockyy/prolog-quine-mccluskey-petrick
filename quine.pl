% :- set_prolog_flag(verbose, silent).
:- use_module(library(clpfd)).
:- use_module(library(pairs)).

% Base case is when the length is enough
number_binarylist(-1, _, []).
% Convert Number into binary digits with length Length
number_binarylist(Length, Number, BinaryList) :-
    Length > -1,  
    NextLength is Length - 1,
    number_binarylist(NextLength, Number, NextBinaryList),
    CurrentValue is (Number>>Length) mod 2,
    append([CurrentValue], NextBinaryList, BinaryList).

% Check the number of difference of List
diff_count([], [], 0).
diff_count([X | List1], [Y | List2], Result) :-
    diff_count(List1, List2, NextResult),
    ((X \= Y) -> (Result is NextResult + 1); (Result is NextResult)).

is_minterms([]).
is_minterms([X | List1]) :- X =< 2, X >= 0, is_minterms(List1).

% unity(L1, L2, L3) is true when L1 and L2's element difference is denoted by 2 in L3,
% else the same element as L1. e.g: unity([0,1,1,1,0], [0,1,0,1,0], [0,1,2,1,0]).
unity([],[],[]).
unity([X | List1], [Y | List2], [Z | ListUnified]) :-
    ((X = Y) -> (
        Z = X
    );(
        Z = 2
    )),
    unity(List1, List2, ListUnified).

unified_term([_, _, A, B], Unified) :-
    unity(A, B, Unified).

included_term([IndexA, IndexB, _, _], [IndexA, IndexB]).

% Iterate quine will receive a list of minterms object.
% minterms object 
iterate_quine(BinaryList, Result) :-
    (length(BinaryList, 1)) -> (Result = BinaryList, !);
    % writeln(BinaryList),
    % maplist(is_minterms, BinaryList),
    length(BinaryList, MintermsLength),
    % write(MintermsLength),
    MintermsLengthMinusOne is MintermsLength - 1,
    [X, Y] ins 0..MintermsLengthMinusOne,
    X #< Y,

    findall([X, Y, ElementX, ElementY],
        (nth0(X, BinaryList, ElementX),
        nth0(Y, BinaryList, ElementY),
        diff_count(ElementX, ElementY, 1))
        ,UnifyableTerms),
    % write(UnifyableTerms),
    maplist(unified_term, UnifyableTerms, Unified),

    maplist(included_term, UnifyableTerms, OnlyIndexUnflattened),
    flatten(OnlyIndexUnflattened, OnlyIndex),
    % Find index that has been included in the unified terms
    list_to_set(OnlyIndex, OnlyIndexSet),
    % Find minterms index that has not been unified

    findall(A, between(0, MintermsLength, A), AllTerms),
    subtract(AllTerms, OnlyIndexSet, NonUnifiedIndices),
    findall(NonUnifiedBinaryList,
        (member(NonUnifiedIndex, NonUnifiedIndices),
        nth0(NonUnifiedIndex, BinaryList, NonUnifiedBinaryList)),
        NonUnifiedBinaryLists),
    
    % Append the non unified and the unified binary list.
    append(NonUnifiedBinaryLists, Unified, TemporaryResult),
    
    % Make sure no duplicate is encountered
    list_to_set(TemporaryResult, IterationResult),
    % writeln(IterationResult),
    % First iteration to find prime implicant is done
    ((OnlyIndexSet = []) -> (
        Result = IterationResult
    );(
        iterate_quine(IterationResult, Result)    
    )).

match_with_dontcares([],[]).
match_with_dontcares([X | List1], [X | List2]) :-
    match_with_dontcares(List1, List2).

match_with_dontcares([_ | List1], [2 | List2]) :-
    match_with_dontcares(List1, List2).

quine(N, Minterms, Output) :-
    % Get TwoPower
    TwoPower is 2 ** N - 1,
    
    % Split the minterms using comma
    split_string(Minterms, ",", ",", SubStrings),
    
    % From strings to numbers
    maplist(number_string, Numbers, SubStrings),
    
    % Asserts all the elements of minterms is between it
    maplist(call(between, 0, TwoPower), Numbers),
    NMinusOne is N - 1,
    maplist(call(number_binarylist, NMinusOne), Numbers, BinaryList),
    
    % Output = BinaryList,
    iterate_quine(BinaryList, PrimeImplicants),
    Output = PrimeImplicants.
    % writeln(BinaryList),
    % writeln(PrimeImplicants),
    % writeln(""),
    % halt,
