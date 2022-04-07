% :- set_prolog_flag(verbose, silent).

% Base case is when the length is enough
number_binarylist(-1, _, []).
number_binarylist(Length, Number, BinaryList) :-
    Length > -1,  
    NextLength is Length - 1,
    number_binarylist(NextLength, Number, NextBinaryList),
    CurrentValue is (Number>>Length) mod 2,
    append([CurrentValue], NextBinaryList, BinaryList).

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
    Output = BinaryList.