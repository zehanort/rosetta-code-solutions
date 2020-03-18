%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Twelve statements                                    %
% link: https://rosettacode.org/wiki/Twelve_statements %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% A brute-force solution with built-in tools only.
% Pretty naive idea:
% 1. Define what it means for statements 1-12 to hold
% 2. Create all possible combinations of statements validity
% 3. Test each statement with each combination and see if the result (T/F)
%    is what this combination was expecting
% Extra credit not implemented (yet)

%%% some helping predicates %%%
exactly(0, []).
exactly(N, [H|T]) :-
    (
        H
    ->
        NewN is N - 1, exactly(NewN, T)
    ;
        exactly(N, T)
    ).

entail(X, Y) :-
    ( X -> Y ; not(X) ).

filter_by_idxs([], _, []).
filter_by_idxs([IdxH|IdxT], ST, [FilteredH|FilteredT]) :-
    nth1(IdxH, ST, FilteredH),
    filter_by_idxs(IdxT, ST, FilteredT).

%%%%%%%%%%%%%%%%%%
%%% STATEMENTS %%%
%%%%%%%%%%%%%%%%%%

% 1.  This is a numbered list of twelve statements.
s1(SL) :-
    length(SL, 12).

% 2.  Exactly 3 of the last 6 statements are true.
s2(SL) :-
    filter_by_idxs([7, 8, 9, 10, 11, 12], SL, Last6),
    exactly(3, Last6).

% 3.  Exactly 2 of the even-numbered statements are true.
s3(SL) :-
    filter_by_idxs([2, 4, 6, 8, 10, 12], SL, EvenStatements),
    exactly(2, EvenStatements).

%  4.  If statement 5 is true, then statements 6 and 7 are both true.
s4(SL) :-
    filter_by_idxs([5, 6, 7], SL, [S5, S6, S7]),
    entail(S5, S6), entail(S5, S7).

%  5.  The 3 preceding statements are all false.
s5(SL) :-
    filter_by_idxs([2, 3, 4], SL, Prev3),
    exactly(0, Prev3).

%  6.  Exactly 4 of the odd-numbered statements are true.
s6(SL) :-
    filter_by_idxs([1, 3, 5, 7, 9, 11], SL, OddStatements),
    exactly(4, OddStatements).

%  7.  Either statement 2 or 3 is true, but not both.
s7(SL) :-
    filter_by_idxs([2, 3], SL, S2S3),
    exactly(1, S2S3).

%  8.  If statement 7 is true, then 5 and 6 are both true.
s8(SL) :-
    filter_by_idxs([5, 6, 7], SL, [S5, S6, S7]),
    entail(S7, S5), entail(S7, S6).

%  9.  Exactly 3 of the first 6 statements are true.
s9(SL) :-
    filter_by_idxs([1, 2, 3, 4, 5, 6], SL, First6),
    exactly(3, First6).

% 10.  The next two statements are both true.
s10(SL) :-
    filter_by_idxs([11, 12], SL, Next2),
    exactly(2, Next2).

% 11.  Exactly 1 of statements 7, 8 and 9 are true.
s11(SL) :-
    filter_by_idxs([7, 8, 9], SL, Statements7to9),
    exactly(1, Statements7to9).

% 12.  Exactly 4 of the preceding statements are true.
s12(SL) :-
    findall(X, between(1, 11, X), AllPrecedingIdx),
    filter_by_idxs(AllPrecedingIdx, SL, AllPreceding),
    exactly(4, AllPreceding).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% MAIN PREDICATE HELPERS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 2 helper predicates to create all possible lists with
% true and false of a custom length
unfold([], []).
unfold([H|T], [[false|H], [true|H]|L]) :-
    unfold(T, L).

binary_n_list(N, L) :-
    (
        N = 0 -> L = [[]]
    ;
        N1 is N - 1,
        binary_n_list(N1, L1),
        unfold(L1, L)
    ).

check_statements(Vals) :-
    L = [s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12],
    check_all_statements(Vals, L, Vals).

check_all_statements(_, [], []).
check_all_statements(Vals, [Pred|PredT], [ExpectedValue|ValsT]) :-
    Goal =.. [Pred, Vals],
    (
        Goal, ExpectedValue           % sX is true, which was expected
    ;
        not(Goal), not(ExpectedValue) % sX is false, which was expected
    ),
    check_all_statements(Vals, PredT, ValsT).

solution(ASolution) :-
    binary_n_list(12, AllPossibleEvals),
    member(ASolution, AllPossibleEvals),
    check_statements(ASolution).

print_solutions(_, []).
print_solutions(Idx, [NextSolution|RestSolutions]) :-
    bool2letter(NextSolution, Solution),
    write(Idx), write(': '), write(Solution), nl,
    NextIdx is Idx + 1,
    print_solutions(NextIdx, RestSolutions).

bool2letter([], []).
bool2letter([Bool|RestBool], [Letter|RestLetters]) :-
    (
        ( Bool = true -> Letter = 'T' )
    ;
        ( Bool = false -> Letter = 'F' )
    ),
    bool2letter(RestBool, RestLetters).

%%%%%%%%%%%%%%%%%%%%%%%
%%% MAIN PREDICATES %%%
%%%%%%%%%%%%%%%%%%%%%%%

% simple solver
solve_twelve_statements_problem(AllSolutions) :-
    findall(ASolution, solution(ASolution), AllSolutions).

% solver with pretty printing
solve_twelve_statements_problem :-
    solve_twelve_statements_problem(AllSolutions),
    length(AllSolutions, NumOfSols),
    write('Number of solutions: '), write(NumOfSols), nl,
    print_solutions(1, AllSolutions), !.
