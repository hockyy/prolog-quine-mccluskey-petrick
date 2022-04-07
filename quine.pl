% :- set_prolog_flag(verbose, silent).

go(N, Minterms, Output) :- TwoPower is 2 ** N - 1,
    split_string(Minterms, ",", ",", SubStrings),
    maplist(number_string, Numbers, SubStrings),
    maplist(call(between, 0, TwoPower), Numbers),
    Output = Numbers.
