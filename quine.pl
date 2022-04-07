% :- set_prolog_flag(verbose, silent).
:- use_module(library(clpfd)).

% Base case is when the length is enough
number_binarylist(-1, _, []).
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

quine(N, Minterms, Output) :-
    % Get TwoPower
    TwoPower is 2 ** N - 1,
    % Split the minterms using comma
    split_string(Minterms, ",", ",", SubStrings),
    % From strings to numbers
    maplist(number_string, Numbers, SubStrings),
    % Asserts all the elements of minterms is between it
    maplist(call(between, 0, TwoPower), Numbers),
    maplist(call(number_binarylist, N - 1), Numbers, BinaryList),
    % Output = BinaryList,
    length(BinaryList, MintermsLength),
    MintermsLengthMinusOne is MintermsLength - 1,
    [X, Y] ins 0..MintermsLengthMinusOne,
    X #< Y,
    % X is 0,
    % Y is 1,
    nth0(X, BinaryList, ElementX),
    nth0(Y, BinaryList, ElementY),

    diff_count(ElementX, ElementY, 1),
    Output = [X, Y].