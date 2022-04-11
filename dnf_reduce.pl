/*
   Minimize a set of clauses in DNF - based on the Quine-McCluskey algorithm
   with the final stage implemented as an exhaustive search (could use
   best-first search for very complex reductions or could implement Petrick's
   method).

   Dr Andreas Schoter
   2011/11/29
   www.asch.org.uk
*/
:- module(dnf_reduce, [minimize_clauseset/2,
		       op(900, fx, ~),
                       convert_dnf_alg/2,
                       convert_dnf_term/2
]).

:- ensure_loaded(library(ordsets)).

/* Top level goal to the system.

   (+DnfSet, -ReducedSet)
   Take the DnfSet of minterms and reduce the clauses into minimal forms using
   the Quine-McCluskey algorithm.
*/
minimize_clauseset(DnfSet, ReducedSet):-
        retractall(mt_clause(_)),
        decorate_minterms(DnfSet, 0, MinTerms, DSet),
        prime_implicants(DSet, Primes),
        essential_implicants(Primes, MinTerms, Essence),
        xsetof(M, E^I^(member(M, MinTerms),
                       \+ (member(E-I, Essence),member(M,I))),
               Rem),
        cover_missing_minterms(Rem, Primes, Extras),
        append(Extras, Essence, Reduced0),
        strip_decoration(Reduced0, ReducedSet).


/* Decorate each minterm with an index set.

   (+MintermSet, +Index, -IndexSet, -DecoratedMintermSet)
   This is the first operation on the input DNF clauses. The index set is used
   to track wich minterms are implied by which implicant clauses later in the
   process.
*/
decorate_minterms([], _, [], []).
decorate_minterms([Clause|Rest], Num, [Num|X], [Clause-[Num]|RestOut]):-
        Num1 is Num + 1,
        decorate_minterms(Rest, Num1, X, RestOut).


/* Compute all of the prime implicants for the clause set.

   (+DecoratedDnfSet, -PrimeImplicants)
   Reduce pairs of clauses in the DecoratedDnfSet and disguard any that are
   implied by other clauses too o leave the PrimeImplicants.
*/
prime_implicants(DSet, Reduced):-
        full_reduce(DSet, RSet),
        setof(C-M, ((member(C-M, DSet);member(C-M, RSet)),
                    \+ mt_clause(C)),
              Reduced).

% recursively reduce the set until no more reducations can be made
full_reduce([], []):- !.
full_reduce(Set, ReducedSet):-
        xsetof(Clause, reduced_clause(Set, Clause), PartReducedSet),
        append(PartReducedSet, S, ReducedSet),
        full_reduce(PartReducedSet, S).

% reduce a pair of clauses from the list to give a reduced clause with combined
% minterm covering list
reduced_clause(ClauseList, R-M):-
        select(C1-M1, ClauseList, Rest),
        member(C2-M2, Rest),
        clause_reduce(C1, C2, R),
        ord_union(M1, M2, M),
        store_clause(C1),
        store_clause(C2).

% reduce two clauses each with N literals to a clause with N-1 literals
clause_reduce([A|R],[B|R],R):-
        compliments(A,B),!.
clause_reduce([A|R1],[A|R2],[A|R]):-
        clause_reduce(R1,R2,R).


/* Compute the essential prime implicants.

   (+PrimeImplicants, +MintermList, -EssentialImplicants)
   The EssentialImplicants are those PrimeImplicants that are the only implicants
   for any single minterm in the MintermList.
*/
essential_implicants(ClauseSet, MinTerms, Essence):-
        xsetof(Z, M^Clause^I^(member(M, MinTerms),
                             setof(Clause-I, (member(Clause-I, ClauseSet),
                                              member(M, I)),
                                   [Z])),
              Essence).


/* Find prime implicants which systematically imply any minterms not implied by
   the essential prime implicants.

   (+RemainingMintermList, +PrimeImplicants, -ExtraImplicants)
   The ExtraImplicants are those implicants from the PrimeImplicants that are
   needed in order to imply all of the members in the RemainingMintermList.
*/
cover_missing_minterms([], _, []):- !.
cover_missing_minterms(MinTerms, Primes, Extras):-
        length(MinTerms, Lm),
        write(Lm),write(' minterms to cover... '),
        statistics(inferences, I0),
        setof(C-I, M^(member(M, MinTerms), member(C-I, Primes), member(M, I)),
              Candidates),
        minimal_candidates(MinTerms, Candidates, Extras),
        statistics(inferences, I1),
        I is I1 - I0,
        write('took '),write(I),writeln(' inferences').


% find all the candidate sets of terms and return them in order of simplicity
minimal_candidates(MinTerms, Candidates, Additions):-
        setof(Extras, candidate(MinTerms, Candidates, 0-[], Extras), ExtraSet),
        sort(ExtraSet, SortedSet),
        member(_C-Additions, SortedSet).

% a candidate is a set of implicants that imply all the given minterms
candidate([], _, Accumulator, Accumulator).
candidate([Mt|Terms], Candidates, Cost-Accum, Extras):-
        member(_C-I, Accum),
        member(Mt, I),
        candidate(Terms, Candidates, Cost-Accum, Extras).
candidate([Mt|Terms], Candidates, Cost-Accum, Extras):-
        select(C-I, Candidates, RemC),
        member(Mt, I),
        length(C, CCost),
        NewCost is Cost + CCost,
        candidate(Terms, RemC, NewCost-[C-I|Accum], Extras).


/* Strip minterm indexes from the clause set.

   (+DecoratedClauseSet, -PlainClauseSet)
   This is the last operation done on the minimal form clauses before they are
   returned as the result. All minterm index sets are removed.
*/
strip_decoration([], []).
strip_decoration([Clause-_MinTermList|Rest],[Clause|RestC]):-
        strip_decoration(Rest, RestC).


compliments(~A, A):- !.
compliments(A, ~A).

store_clause(C):-
        mt_clause(C),!.
store_clause(C):-
        asserta(mt_clause(C)).

xsetof(X, G, S):-
        setof(X, G, S),!.
xsetof(_, _, []).


/* Two top level clauses to convert the DNF list form into other formats.
*/

/* Convert DNF list to a string representation of terms suitable to feed into
   the JavaScript 7 Segment Display simulator.
*/
convert_dnf_alg([Clause|DnfList], AlgString):-
        c_dnf_as(DnfList, Clause, '', AlgString).

c_dnf_as([], Clause, AsIn, AsOut):-
        c_dnf_clause(Clause, AsIn, AsOut).
c_dnf_as([C|Rest], Clause, AsIn, AsOut):-
        c_dnf_clause(Clause, AsIn, AsTemp),
        string_concat(AsTemp, '+', AsMid),
        c_dnf_as(Rest, C, AsMid, AsOut).

c_dnf_clause([], As, As).
c_dnf_clause([~X|Rest], AsIn, AsOut):- !,
        string_to_atom(XStr, X),
        string_concat(AsIn, XStr, AsT0),
        string_concat(AsT0, '\'', AsMid),
        c_dnf_clause(Rest, AsMid, AsOut).
c_dnf_clause([X|Rest], AsIn, AsOut):-
        string_concat(AsIn, X, AsMid),
        c_dnf_clause(Rest, AsMid, AsOut).


/* Convert DNF list to a string representation of terms suitable to feed into
   Christopher Vickery's Java program.

   See: http://babbage.cs.qc.edu/courses/Minimize/
*/
convert_dnf_term([Clause|DnfList], TString):-
        c_dnf_ts(DnfList, Clause, '', TString).

c_dnf_ts([], [T|Clause], AsIn, AsOut):-
        c_dnf_ct(Clause, T, AsIn, AsOut).
c_dnf_ts([C|Rest], [T|Clause], AsIn, AsOut):-
        c_dnf_ct(Clause, T, AsIn, AsTemp),
        string_concat(AsTemp, ' | ', AsMid),
        c_dnf_ts(Rest, C, AsMid, AsOut).

c_dnf_ct([], ~Lit, AsIn, AsOut):- !,
        string_concat(AsIn, '~', AsTemp0),
        string_to_atom(AStr, Lit),
        string_concat(AsTemp0, AStr, AsOut).
c_dnf_ct([], Lit, AsIn, AsOut):-
        string_to_atom(AStr, Lit),
        string_concat(AsIn, AStr, AsOut).
c_dnf_ct([L|Clause], ~Lit, AsIn, AsOut):- !,
        string_concat(AsIn, '~', AsTemp0),
        string_to_atom(LStr, Lit),
        string_concat(AsTemp0, LStr, AsTemp1),
        string_concat(AsTemp1, '&', AsMid),
        c_dnf_ct(Clause, L, AsMid, AsOut).
c_dnf_ct([L|Clause], Lit, AsIn, AsOut):-
        string_to_atom(LStr, Lit),
        string_concat(AsIn, LStr, AsTemp1),
        string_concat(AsTemp1, '&', AsMid),
        c_dnf_ct(Clause, L, AsMid, AsOut).
