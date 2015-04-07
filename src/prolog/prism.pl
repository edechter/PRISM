%% -*- Prolog -*-

%%========================================================================
%%
%% This module contains a set of error and warning messages displayed in
%% the Prism system.  Each message entry has the following form:
%%
%%     $pp_message(ID,Type,Message)
%%
%% <ID> is a positive integer that identifies the message.
%%
%% <Type> denotes the message type, which is one of the following:
%%
%%     * fatal
%%     * inter(nal error)
%%     * error
%%     * fail
%%     * warn
%%     * obsol(ete)
%%     * info
%%
%% <Message> is (to be written).
%%
%%========================================================================

%%
%% Errors related to probabilistic models
%%

% Errors related to probabilities
$pp_message($msg(0000),error,"invalid probability -- {1}").
$pp_message($msg(0001),error,"invalid probability list -- {1}").
$pp_message($msg(0002),error,"invalid ratio list -- {1}").
$pp_message($msg(0003),error,"invalid probabilistic atomic formula -- {1}").
$pp_message($msg(0004),error,"invalid user-defined probabilistic atomic formula -- {1}").
$pp_message($msg(0005),error,"invalid extended probabilistic atomic formula -- {1}").
$pp_message($msg(0006),error,"invalid tabled probabilistic atomic formula -- {1}").
$pp_message($msg(0007),error,"invalid probabilistic callable -- {1}").

% Errors related to random switches
$pp_message($msg(0100),error,"no multi-valued switch declarations given").
$pp_message($msg(0101),error,"non-ground switch name -- {1}").
$pp_message($msg(0102),error,"outcome space not given -- {1}").
$pp_message($msg(0103),error,"probability distribution not given -- {1}").
$pp_message($msg(0104),error,"hyperparameters not given -- {1}").
%$pp_message($msg(0105),error,"").
$pp_message($msg(0106),error,"modified outcome space; probabilities expected to be unfixed -- {1}").
$pp_message($msg(0107),error,"modified outcome space; obsolete expectations -- {1}").
$pp_message($msg(0108),error,"modified outcome space; hyperparameters expected to be unfixed -- {1}").
$pp_message($msg(0109),warn, "distribution fixed -- {1}").
$pp_message($msg(0110),warn, "hyperparameters fixed -- {1}").
$pp_message($msg(0111),error,"invalid saved switch information -- {1}").
$pp_message($msg(0112),error,"dynamic change of outcome space with clean_table being off").

% Errors related to distribution
$pp_message($msg(0200),error,"invalid distribution -- {1}").
$pp_message($msg(0201),error,"invalid hyperparameters -- {1}").
$pp_message($msg(0202),error,"default distribution unavailable").
$pp_message($msg(0203),error,"default hyperparameters unavailable").
$pp_message($msg(0204),error,"invalid number of outcomes -- {1}").
$pp_message($msg(0205),error,"invalid switch configuration -- {1}").
%$pp_message($msg(0206),error,"").
%$pp_message($msg(0207),error,"").
$pp_message($msg(0208),error,"invalid alpha values -- {1}").
$pp_message($msg(0209),error,"invalid delta values -- {1}").
$pp_message($msg(0210),error,"distribution does not match -- ({1},{2})").
$pp_message($msg(0211),error,"size unmatched -- ({1},{2})").

%%
%% Errors related to built-ins for probabilistic inferences
%%

% Errors in loading
$pp_message($msg(1000),error,"invalid filename -- {1}").
$pp_message($msg(1001),error,"invalid PRISM option -- {1}").
$pp_message($msg(1002),warn, "tabling disabled in the consultation mode").
$pp_message($msg(1003),error,"batch file not specified").
$pp_message($msg(1004),error,"prism_main/0-1 undefined -- {1}").
$pp_message($msg(1005),error,"invalid module for prism or upprism").
$pp_message($msg(1006),error,"invalid PRISM options -- {1}").

% Errors in translation
$pp_message($msg(1100),fail ,"bad or duplicate predicate -- {1}").
$pp_message($msg(1101),error,"co-existing p_table and p_not_table declarations").
$pp_message($msg(1102),error,"invalid predicate indicator -- {1}").
$pp_message($msg(1103),error,"invalid call in write_call").
$pp_message($msg(1104),warn, "parameters left unset/unfixed; ground terms expected -- values_x({1},_,{2})").
$pp_message($msg(1105),error,"invalid outcome space; ground list expected").

% Errors in sampling
$pp_message($msg(1201),error,"invalid goal; probabilistic goal expected -- {1}").
$pp_message($msg(1202),error,"invalid constraint; callable term expected -- {1}").
$pp_message($msg(1203),error,"invalid number of samples; positive integer expected -- {1}").
$pp_message($msg(1204),error,"invalid number of trials; `inf' or positive integer expected -- {1}").

% Errors in EM learning
$pp_message($msg(1300),error,"no observed data; the data_source flag set to `none'").
$pp_message($msg(1301),error,"no observed data; data/1 undefined").
$pp_message($msg(1302),error,"invalid observed data -- {1}").
$pp_message($msg(1303),error,"invalid observed goal; tabled probabilistic atomic formula expected -- {1}").
$pp_message($msg(1304),error,"no explanations -- {1}").
$pp_message($msg(1305),error,"DAEM not applicable to models with failure").
$pp_message($msg(1306),error,"invalied goal count; positive integer expected -- {1}").

% Errors in other probabilistic inferences
$pp_message($msg(1400),error,"invalid number of top-ranked expls; positive integer expected -- {1}").
$pp_message($msg(1401),error,"invalid number of intermediate candidate expls; positive integer expected -- {1}").
$pp_message($msg(1402),error,"invalid subgoal aggregation pattern -- {1}").
$pp_message($msg(1403),error,"invalid subgoal pattern -- {1}").
$pp_message($msg(1404),warn, "subgoals unmatched").
$pp_message($msg(1405),error,"invalid subgoal argument; integer expected -- {1}").
$pp_message($msg(1406),error,"invalid subgoal argument; atom expected -- {1}").
$pp_message($msg(1407),error,"invalid subgoal argument; ground compound expected -- {1}").
$pp_message($msg(1408),error,"invalid subgoal argument; list expected -- {1}").
$pp_message($msg(1409),error,"invalid subgoal argument; d-list expected -- {1}").

% Errors in MCMC
$pp_message($msg(1500),error,"`failure' is not allowed in MCMC -- {1}").
$pp_message($msg(1501),error,"invalid burn-in -- {1}").
$pp_message($msg(1502),error,"invalid skip -- {1}").
$pp_message($msg(1503),error,"invalid step for traces -- {1}").
$pp_message($msg(1504),error,"invalid step for post-parameters -- {1}").
$pp_message($msg(1505),error,"switch information does not exist -- {1}").
$pp_message($msg(1506),error,"MCMC sampling has not been done").
$pp_message($msg(1507),error,"invalid number of trials; positive integer expected -- {1}").

% Errors in CRF
$pp_message($msg(1600),error,"no feature function; fsw/3 undefined").
$pp_message($msg(1601),error,"non-ground goal name -- {1}").
$pp_message($msg(1602),error,"invalid goal argument; goal/2 expected -- {1}").
$pp_message($msg(1603),error,"`failure' is not allowed in CRF learn -- {1}").
$pp_message($msg(1604),error,"goal/1 to compute Z(goal) is not defined -- (1)").

%%
%% Errors related to built-ins for auxiliary operations
%%

% Errors in random operations
$pp_message($msg(2000),error,"invalid random seed -- {1}").
$pp_message($msg(2001),error,"invalid random state -- {1}").
$pp_message($msg(2002),error,"invalid max value; positive integer expected -- {1}").
$pp_message($msg(2003),error,"invalid min value; integer expected -- {1}").
$pp_message($msg(2004),error,"invalid max value; integer expected -- {1}").
$pp_message($msg(2005),error,"invalid max value; positive number expected -- {1}").
$pp_message($msg(2006),error,"invalid min value; number expected -- {1}").
$pp_message($msg(2007),error,"invalid max value; number expected -- {1}").
$pp_message($msg(2008),error,"invalid min/max pair -- ({1},{2})").
$pp_message($msg(2009),error,"invalid mu; number expected -- {1}").
$pp_message($msg(2010),error,"invalid sigma; positive number expected -- {1}").
$pp_message($msg(2011),error,"invalid elements; list expected -- {1}").
$pp_message($msg(2012),error,"invalid number of selections; integer expected -- {1}").
$pp_message($msg(2013),error,"number of selections out of range -- {1}").
$pp_message($msg(2014),error,"invalid number of groups; positive integer expected -- {1}").

% Errors in list handling
$pp_message($msg(2100),error,"invalid predicate name -- {1}").
$pp_message($msg(2101),error,"invalid unary operator -- {1}").
$pp_message($msg(2102),error,"invalid binary operator -- {1}").
$pp_message($msg(2103),error,"invalid argument; list not shorter than {2} expected -- {1}").
$pp_message($msg(2104),error,"invalid argument; list expected -- {1}").
$pp_message($msg(2105),error,"invalid argument; non-negative integer expected -- {1}").
$pp_message($msg(2106),error,"invalid argument; positive integer expected -- {1}").
$pp_message($msg(2107),error,"invalid agglist operation -- {1}").
$pp_message($msg(2108),error,"invalid argument; list of numbers expected -- {1}").
$pp_message($msg(2109),error,"invalid argument; list or nil expected -- {1}").
$pp_message($msg(2110),error,"invalid argument; list of non-variables expected -- {1}").
$pp_message($msg(2111),error,"invalid argument; callable expected -- {1}").

%%
%% Miscellaneous errors
%%

% File I/Os
$pp_message($msg(3000),error,"invalid file specification -- {1}").
$pp_message($msg(3001),error,"file not found -- {1}").
$pp_message($msg(3002),error,"unknown or illegal option -- {1}").
$pp_message($msg(3003),error,"duplicate option -- {1}").
$pp_message($msg(3004),error,"no information on the last observation").
$pp_message($msg(3005),error,"too few rows").
$pp_message($msg(3006),error,"too few columns").
$pp_message($msg(3007),error,"parsing failure in CSV format").
$pp_message($msg(3008),warn, "too few rows compared to the specification").
$pp_message($msg(3009),error,"invalid data for CSV format; atomic term expected -- {1}").

% Execution flags
$pp_message($msg(3100),error,"invalid prism flag -- {1}").
$pp_message($msg(3101),error,"invalid value for {1} -- {2}").
$pp_message($msg(3102),warn, "prism flag replaced by {2} -- {1}").
$pp_message($msg(3103),error,"prism flag deleted in version {2} -- {1}").
$pp_message($msg(3104),error,"prism flag value deleted in version {2} -- {1}").
$pp_message($msg(3105),warn, "prism flag value replaced by {2} -- {1}").

% Write calls
$pp_message($msg(3200),error,"control constructs (other than conjunction) disallowed -- {1}").

% Deprecated predicates
$pp_message($msg(3300),warn, "predicate replaced by {2} -- {1}").
$pp_message($msg(3301),warn, "predicate deprecated -- {1}").

% Math predicates
$pp_message($msg(3400),error,"invalid argument -- {1}").

% Multi-process
$pp_message($msg(3500),error,"multi-process version unavailable").

%%
%% System-related errors
%%

% Internal errors
$pp_message($msg(9800),inter,"error term not found").
$pp_message($msg(9801),inter,"error message not found").
$pp_message($msg(9802),inter,"invalid internal representation").
$pp_message($msg(9803),inter,"unmatched branches").
$pp_message($msg(9804),inter,"unexpected failure").
$pp_message($msg(9805),inter,"failure in hash-id registration -- {1}").

% Fatal errors
$pp_message($msg(9900),fatal,"assertion failure -- {1}").
%%----------------------------------------

$pp_emit_message(MsgID) :-
    $pp_emit_message(MsgID,[]).

$pp_emit_message(MsgID,Args) :-
    $pp_assert($pp_message(MsgID,Type,Format)),
    $pp_message_head(Type,Head),
    format("***  ~w: ",[Head]),
    $pp_format_message(Format,Args),
    format("~n",[]).

$pp_message_head(fatal,'PRISM FATAL ERROR').
$pp_message_head(inter,'PRISM INTERNAL ERROR').
$pp_message_head(error,'PRISM ERROR').
$pp_message_head(fail ,'PRISM WARNING').
$pp_message_head(warn ,'PRISM WARNING').
$pp_message_head(obosl,'PRISM WARNING').
$pp_message_head(info ,'PRISM INFO').

%%----------------------------------------

$pp_assert(Cond) :-
    ( call(Cond) ->
      true
    ; $pp_emit_message($msg(9900),[Cond]), halt
    ).

%%----------------------------------------

% instanciation errors
$pp_raise_instanciation_error(MsgID,Source) :-
    $pp_raise_instanciation_error(MsgID,[],Source).
$pp_raise_instanciation_error(MsgID,Args,Source) :-
    $pp_emit_message(MsgID,Args),
    throw(error(instanciation_error,Source)).

% type errors
$pp_raise_type_error(MsgID,[Type,Culprit],Source) :-
    $pp_raise_type_error(MsgID,[],[Type,Culprit],Source).
$pp_raise_type_error(MsgID,Args,[Type,Culprit],Source) :-
    $pp_emit_message(MsgID,Args),
    throw(error(type_error(Type,Culprit),Source)).

% domain errors
$pp_raise_domain_error(MsgID,[Domain,Culprit],Source) :-
    $pp_raise_domain_error(MsgID,[],[Domain,Culprit],Source).
$pp_raise_domain_error(MsgID,Args,[Domain,Culprit],Source) :-
    $pp_emit_message(MsgID,Args),
    throw(error(domain_error(Domain,Culprit),Source)).

% existence errors
$pp_raise_existence_error(MsgID,[ObjType,Culprit],Source) :-
    $pp_raise_existence_error(MsgID,[],[ObjType,Culprit],Source).
$pp_raise_existence_error(MsgID,Args,[ObjType,Culprit],Source) :-
    $pp_emit_message(MsgID,Args),
    throw(error(existence_error(ObjType,Culprit),Source)).

% permission errors
$pp_raise_permission_error(MsgID,[Operation,PermissionType,Culprit],Source) :-
    $pp_raise_permission_error(MsgID,[],
                               [Operation,PermissionType,Culprit],
                               Source).
$pp_raise_permission_error(MsgID,Args,
                           [Operation,PermissionType,Culprit],
                           Source) :-
    $pp_emit_message(MsgID,Args),
    throw(error(permission_error(Operation,PermissionType,Culprit),Source)).

% evaluation errors
$pp_raise_evaluation_error(MsgID,Error,Source) :-
    $pp_raise_evaluation_error(MsgID,[],Error,Source).
$pp_raise_evaluation_error(MsgID,Args,Error,Source) :-
    $pp_emit_message(MsgID,Args),
    throw(error(evaluation_error(Error),Source)).

% runtime errors
$pp_raise_runtime_error(MsgID,Error,Source) :-
    $pp_raise_runtime_error(MsgID,[],Error,Source).
$pp_raise_runtime_error(MsgID,Args,Error,Source) :-
    $pp_emit_message(MsgID,Args),
    throw(error(prism_runtime_error(Error),Source)).

% translation errors
$pp_raise_trans_error(MsgID,Error,Source) :-
    $pp_raise_trans_error(MsgID,[],Error,Source).
$pp_raise_trans_error(MsgID,Args,Error,Source) :-
    $pp_emit_message(MsgID,Args),
    throw(error(prism_translation_error(Error),Source)).

% internal errors
$pp_raise_internal_error(MsgID,Error,Source) :-
    $pp_raise_internal_error(MsgID,[],Error,Source).
$pp_raise_internal_error(MsgID,Args,Error,Source) :-
    $pp_emit_message(MsgID,Args),
    throw(error(prism_internal_error(Error),Source)).

% warnings
$pp_raise_warning(MsgID) :- $pp_raise_warning(MsgID,[]).
$pp_raise_warning(MsgID,Args) :-
    ( get_prism_flag(warn,on) -> $pp_emit_message(MsgID,Args)
    ; true
    ).

%%----------------------------------------
%% typical internal errors

$pp_raise_unmatched_branches(Source) :-
    $pp_raise_internal_error($msg(9803),unmatched_branches,Source).
$pp_raise_unmatched_branches(Source,Position) :-
    $pp_raise_internal_error($msg(9803),unmatched_branches(Position),Source).

$pp_raise_unexpected_failure(Source) :-
    $pp_raise_internal_error($msg(9804),unexpected_failure,Source).

$pp_raise_unexpected_failure(Source,Position) :-
    $pp_raise_internal_error($msg(9804),unexpected_failure(Position),Source).

%%----------------------------------------

$pp_raise_on_require(Xs,MsgID,Source,Pred) :-
    $pp_emit_message(MsgID,Xs),
    append(Xs,[Error],Args),
    G =.. [Pred|Args],
    ( call(G) ->
      true
    ; $pp_emit_message($msg(9800)),
      Error = prism_internal_error(error_term_not_found)
    ),
    throw(error(Error,Source)).

%%----------------------------------------

$pp_require_atom(X,MsgID,Source) :-
    ( atom(X) ->
      true
    ; $pp_raise_on_require([X],MsgID,Source,$pp_error_atom)
    ).

$pp_error_atom(X,instantiation_error) :-
    \+ ground(X), !.
$pp_error_atom(X,type_error(atom,X)) :-
    \+ atom(X), !.

%%----------------------------------------

$pp_require_atomic(X,MsgID,Source) :-
    ( atomic(X) ->
      true
    ; $pp_raise_on_require([X],MsgID,Source,$pp_error_atomic)
    ).

$pp_error_atomic(X,instantiation_error) :-
    \+ ground(X), !.
$pp_error_atomic(X,type_error(atomic,X)) :-
    \+ atomic(X), !.

%%----------------------------------------

$pp_require_nonvar(X,MsgID,Source) :-
    ( nonvar(X) ->
      true
    ; $pp_raise_on_require([X],MsgID,Source,$pp_error_nonvar)
    ).

$pp_error_nonvar(X,instantiation_error) :-
    var(X), !.

%%----------------------------------------

$pp_require_nonvars(Xs,MsgID,Source) :-
    ( $pp_test_nonvars(Xs) -> true
    ; $pp_raise_on_require([Xs],MsgID,Source,$pp_error_nonvars)
    ).

$pp_test_nonvars(Xs) :-
    Xs = [_|_],
    $pp_test_nonvars1(Xs).

$pp_test_nonvars1([]).
$pp_test_nonvars1([X|Xs]) :-
    nonvar(X),!,
    $pp_test_nonvars1(Xs).

$pp_error_nonvars(Xs,Error) :-
    $pp_error_ground(Xs,Error), !.
$pp_error_nonvars(Xs,Error) :-
    $pp_error_list(Xs,Error), !.
$pp_error_nonvars(Xs,domain_error(non_variables,Xs)) :-
    member(X,Xs),
    var(X), !.

%%----------------------------------------

$pp_require_ground(X,MsgID,Source) :-
    ( ground(X) ->
      true
    ; $pp_raise_on_require([X],MsgID,Source,$pp_error_ground)
    ).

$pp_error_ground(X,instantiation_error) :-
    \+ ground(X), !.

%%----------------------------------------

$pp_require_callable(X,MsgID,Source) :-
    ( callable(X) ->
      true
    ; $pp_raise_on_require([X],MsgID,Source,$pp_error_callable)
    ).

$pp_error_callable(X,instantiation_error) :-
    var(X), !.
$pp_error_callable(X,type_error(callable,X)) :-
    \+ callable(X), !.

%%----------------------------------------

$pp_require_integer(X,MsgID,Source) :-
    ( integer(X) ->
      true
    ; $pp_raise_on_require([X],MsgID,Source,$pp_error_integer)
    ).

$pp_error_integer(X,instantiation_error) :-
    var(X), !.
$pp_error_integer(X,type_error(integer,X)) :-
    \+ integer(X), !.

%%----------------------------------------

$pp_require_positive_integer(X,MsgID,Source) :-
    ( integer(X), X > 0 ->
      true
    ; $pp_raise_on_require([X],MsgID,Source,$pp_error_positive_integer)
    ).

$pp_error_positive_integer(X,Error) :-
    $pp_error_integer(X,Error), !.
$pp_error_positive_integer(X,domain_error(greater_than_zero,X)) :-
    X =< 0, !.

%%----------------------------------------

$pp_require_non_negative_integer(X,MsgID,Source) :-
    ( integer(X), X >= 0 ->
      true
    ; $pp_raise_on_require([X],MsgID,Source,$pp_error_non_negative_integer)
    ).

$pp_error_non_negative_integer(X,Error) :-
    $pp_error_integer(X,Error), !.
$pp_error_non_negative_integer(X,domain_error(not_less_than_zero,X)) :-
    X < 0, !.

%%----------------------------------------

$pp_require_number(X,MsgID,Source) :-
    ( number(X) ->
      true
    ; $pp_raise_on_require([X],MsgID,Source,$pp_error_number)
    ).

$pp_error_number(X,instantiation_error) :-
    var(X), !.
$pp_error_number(X,type_error(number,X)) :-
    \+ number(X), !.

%%----------------------------------------

$pp_require_numbers(Xs,MsgID,Source) :-
    ( $pp_test_numbers(Xs) -> true
    ; $pp_raise_on_require([Xs],MsgID,Source,
                           $pp_error_numbers)
    ).

$pp_test_numbers(Xs) :-
    Xs = [_|_],
    $pp_test_numbers1(Xs).

$pp_test_numbers1([]).
$pp_test_numbers1([X|Xs]) :-
    number(X),!,
    $pp_test_numbers1(Xs).

$pp_error_numbers(Xs,Error) :-
    $pp_error_ground(Xs,Error), !.
$pp_error_numbers(Xs,Error) :-
    $pp_error_list(Xs,Error), !.
$pp_error_numbers(Xs,domain_error(numbers,Xs)) :-
    member(X,Xs),
    \+ number(X), !.

%%----------------------------------------

$pp_require_positive_number(X,MsgID,Source) :-
    ( number(X), X > 0 ->
      true
    ; $pp_raise_on_require([X],MsgID,Source,$pp_error_positive_number)
    ).

$pp_error_positive_number(X,Error) :-
    $pp_error_number(X,Error), !.
$pp_error_positive_number(X,domain_error(greater_than_zero,X)) :-
    X =< 0, !.

%%----------------------------------------

$pp_require_positive_numbers(Xs,MsgID,Source) :-
    ( $pp_test_positive_numbers(Xs) -> true
    ; $pp_raise_on_require([Xs],MsgID,Source,
                           $pp_error_positive_numbers)
    ).

$pp_test_positive_numbers(Xs) :-
    Xs = [_|_],
    $pp_test_positive_numbers1(Xs).

$pp_test_positive_numbers1([]).
$pp_test_positive_numbers1([X|Xs]) :-
    number(X),
    X > 0,!,
    $pp_test_positive_numbers1(Xs).

$pp_error_positive_numbers(Xs,Error) :-
    $pp_error_ground(Xs,Error), !.
$pp_error_positive_numbers(Xs,Error) :-
    $pp_error_list(Xs,Error), !.
$pp_error_positive_numbers(Xs,domain_error(positive_numbers,Xs)) :-
    member(X,Xs),
    (\+ number(X) ; X =< 0), !.

%%----------------------------------------

$pp_require_number_not_less_than(X,Min,MsgID,Source) :-
    $pp_assert(number(Min)),
    ( number(X), X >= Min ->
      true
    ; $pp_raise_on_require([X,Min],MsgID,Source,$pp_error_number_not_less_than)
    ).

$pp_error_number_not_less_than(X,_,Error) :-
    $pp_error_number(X,Error), !.
$pp_error_number_not_less_than(X,Min,domain_error(not_less_than(Min),X)) :-
    X < Min, !.

%%----------------------------------------

$pp_require_numbers_not_less_than(Xs,Min,MsgID,Source) :-
    $pp_assert(number(Min)),
    ( $pp_test_numbers_not_less_than(Min,Xs) -> true
    ; $pp_raise_on_require([Xs,Min],MsgID,Source,
                           $pp_error_numbers_not_less_than)
    ).

$pp_test_numbers_not_less_than(Min,Xs) :-
    Xs = [_|_],
    $pp_test_numbers_not_less_than1(Min,Xs).

$pp_test_numbers_not_less_than1(_,[]).
$pp_test_numbers_not_less_than1(Min,[X|Xs]) :-
    number(X),
    X >= Min,!,
    $pp_test_numbers_not_less_than1(Min,Xs).

$pp_error_numbers_not_less_than(Xs,_,Error) :-
    $pp_error_ground(Xs,Error), !.
$pp_error_numbers_not_less_than(Xs,_,Error) :-
    $pp_error_list(Xs,Error), !.
$pp_error_numbers_not_less_than(Xs,Min,
                                domain_error(numbers_not_less_than(Min),Xs)) :-
    member(X,Xs),
    (\+ number(X) ; X < Min ), !.

%%----------------------------------------

$pp_require_non_negative_number(X,MsgID,Source) :-
    ( number(X), X >= 0 ->
      true
    ; $pp_raise_on_require([X],MsgID,Source,$pp_error_non_negative_number)
    ).

$pp_error_non_negative_number(X,Error) :-
    $pp_error_number(X,Error), !.
$pp_error_non_negative_number(X,domain_error(not_less_than_zero,X)) :-
    X < 0, !.

%%----------------------------------------

$pp_require_non_negative_numbers(Xs,MsgID,Source) :-
    ( $pp_test_non_negative_numbers(Xs) -> true
    ; $pp_raise_on_require([Xs],MsgID,Source,$pp_error_non_negative_numbers)
    ).

$pp_test_non_negative_numbers(Xs) :-
    Xs = [_|_],
    $pp_test_non_negative_numbers1(Xs).

$pp_test_non_negative_numbers1([]).
$pp_test_non_negative_numbers1([X|Xs]) :-
    number(X),
    X >= 0.0,!,
    $pp_test_non_negative_numbers1(Xs).

$pp_error_non_negative_numbers(Xs,Error) :-
    $pp_error_ground(Xs,Error), !.
$pp_error_non_negative_numbers(Xs,Error) :-
    $pp_error_list(Xs,Error), !.
$pp_error_non_negative_numbers(Xs,domain_error(non_negative_numbers,Xs)) :-
    member(X,Xs),
    (\+ number(X) ; X < 0 ), !.

%%----------------------------------------

$pp_require_list(X,MsgID,Source) :-
    ( nonvar(X), X = [_|_] -> true
    ; $pp_raise_on_require([X],MsgID,Source,$pp_error_list)
    ).

$pp_error_list(X,instanciation_error) :-
    var(X), !.
$pp_error_list(X,type_error(list,X)) :-
    X \= [_|_], !.

%%----------------------------------------

$pp_require_list_or_nil(X,MsgID,Source) :-
    ( nonvar(X), (X = [_|_] ; X = []) -> true
    ; $pp_raise_on_require([X],MsgID,Source,$pp_error_list_or_nil)
    ).

$pp_error_list_or_nil(X,instanciation_error) :-
    var(X), !.
$pp_error_list_or_nil(X,type_error(list_or_nil,X)) :-
    X \= [_|_], X \= [], !.

%%----------------------------------------

$pp_require_list_not_shorter_than(X,Min,MsgID,Source) :-
    $pp_assert(integer(Min)),
    $pp_assert(Min >= 0),
    ( $pp_test_list_not_shorter_than(X,Min) -> true
    ; $pp_raise_on_require([X,Min],MsgID,Source,$pp_error_list_not_shorter_than)
    ).

$pp_test_list_not_shorter_than(X,Min) :-
    nonvar(X),
    ( X = [_|_] ; X = [] ),
    length(X,L), L >= Min.

$pp_error_list_not_shorter_than(X,_Min,instanciation_error) :-
    var(X), !.
$pp_error_list_not_shorter_than(X,_Min,type_error(list,X)) :-
    X \= [_|_], X \= [], !.
$pp_error_list_not_shorter_than(X,Min,type_error(list_not_shorter_than(Min),X)) :-
    length(X,L), L < Min, !.

%%----------------------------------------

$pp_require_compound(X,MsgID,Source) :-
    ( compound(X) ->
      true
    ; $pp_raise_on_require([X],MsgID,Source,$pp_error_compound)
    ).

$pp_error_compound(X,instantiation_error) :-
    var(X), !.
$pp_error_compound(X,type_error(compound,X)) :-
    \+ compound(X), !.

%%----------------------------------------

$pp_require_integer_range(Min,Max,MsgID,Source) :-
    $pp_assert(integer(Min)),
    $pp_assert(integer(Max)),
    ( Min < Max ->
      true
    ; $pp_raise_on_require([Min,Max],MsgID,Source,$pp_error_integer_range)
    ).

$pp_error_integer_range(Min,Max,Error) :-
    Min >= Max,
    Error = domain_error(integer_range,[Min,Max]), !.

%%----------------------------------------

$pp_require_integer_range_incl(Min,Max,MsgID,Source) :-
    $pp_assert(integer(Min)),
    $pp_assert(integer(Max)),
    ( Min =< Max ->
      true
    ; $pp_raise_on_require([Min,Max],MsgID,Source,$pp_error_integer_range_incl)
    ).

$pp_error_integer_range_incl(Min,Max,Error) :-
    Min > Max,
    Error = domain_error(integer_range_inclusive,[Min,Max]), !.

%%----------------------------------------

$pp_require_integer_range_excl(Min,Max,MsgID,Source) :-
    $pp_assert(integer(Min)),
    $pp_assert(integer(Max)),
    ( Min + 1 > Min, Min + 1 < Max ->   % (Min + 1 =< Min) -> overflow
      true
    ; $pp_raise_on_require([Min,Max],MsgID,Source,$pp_error_integer_range_excl)
    ).

$pp_error_integer_range_excl(Min,Max,Error) :-
    ( Min + 1 =< Min ; Min + 1 >= Max ),
    Error = domain_error(integer_range_exclusive,[Min,Max]), !.

%%----------------------------------------

$pp_require_number_range_incl(Min,Max,MsgID,Source) :-
    $pp_assert(number(Min)),
    $pp_assert(number(Max)),
    ( Min =< Max ->
      true
    ; $pp_raise_on_require([Min,Max],MsgID,Source,$pp_error_number_range_incl)
    ).

$pp_error_number_range_incl(Min,Max,Error) :-
    Min > Max,
    Error = domain_error(number_range_inclusive,[Min,Max]), !.

%%----------------------------------------

$pp_require_number_range_excl(Min,Max,MsgID,Source) :-
    $pp_assert(number(Min)),
    $pp_assert(number(Max)),
    ( Min < Max ->
      true
    ; $pp_raise_on_require([Min,Max],MsgID,Source,$pp_error_number_range_excl)
    ).

$pp_error_number_range_excl(Min,Max,Error) :-
    Min >= Max,
    Error = domain_error(number_range_exclusive,[Min,Max]), !.

%%----------------------------------------

$pp_require_membership(X,Xs,MsgID,Source) :-
    $pp_assert(Xs = [_|_]),
    ( nonvar(X),membchk(X,Xs) -> true
    ; $pp_raise_on_require([X,Xs],MsgID,Source,$pp_error_membership)
    ).

$pp_error_membership(X,_Xs,Error) :-
    $pp_error_nonvar(X,Error), !.
$pp_error_membership(X,Xs,domain_error(Xs,X)) :-
    \+ membchk(X,Xs), !.

%%----------------------------------------

$pp_require_predicate_indicator(X,MsgID,Source) :-
    ( $pp_test_predicate_indicator(X) -> true
    ; $pp_raise_on_require([X],MsgID,Source,$pp_error_predicate_indicator)
    ).

$pp_test_predicate_indicator(X) :-
    X = F/N, atom(F), integer(N), N >= 0.

$pp_error_predicate_indicator(X,Error) :-
    $pp_error_ground(X,Error), !.
$pp_error_predicate_indicator(X,type_error(predicate_indicator,X)) :-
    \+ $pp_test_predicate_indicator(X), !.

%%----------------------------------------

$pp_require_user_probabilistic_atom(X,MsgID,Source) :-
    ( $pp_is_user_probabilistic_atom(X) -> true
    ; $pp_raise_on_require([X],MsgID,Source,
                           $pp_error_user_probabilistic_atom)
    ).

$pp_error_user_probabilistic_atom(X,Error) :-
    $pp_error_nonvar(X,Error), !.
$pp_error_user_probabilistic_atom(X,Error) :-
    $pp_error_callable(X,Error), !.
$pp_error_user_probabilistic_atom(X,type_error(user_probabilistic_atom,X)) :-
    \+ $pp_is_user_probabilistic_atom(X), !.

%%----------------------------------------

$pp_require_probabilistic_atom(X,MsgID,Source) :-
    ( $pp_is_probabilistic_atom(X) -> true
    ; $pp_raise_on_require([X],MsgID,Source,
                           $pp_error_probabilistic_atom)
    ).

$pp_error_probabilistic_atom(X,Error) :-
    $pp_error_nonvar(X,Error), !.
$pp_error_probabilistic_atom(X,Error) :-
    $pp_error_callable(X,Error), !.
$pp_error_probabilistic_atom(X,type_error(probabilistic_atom,X)) :-
    \+ $pp_is_probabilistic_atom(X), !.

%%----------------------------------------

$pp_require_extended_probabilistic_atom(X,MsgID,Source) :-
    ( $pp_is_extended_probabilistic_atom(X) -> true
    ; $pp_raise_on_require([X],MsgID,Source,
                           $pp_error_extended_probabilistic_atom)
    ).

$pp_error_extended_probabilistic_atom(X,Error) :-
    $pp_error_probabilistic_atom(X,Error), !.
$pp_error_extended_probabilistic_atom(X,type_error(extended_probabilistic_atom,X)) :-
    \+ $pp_is_extended_probabilistic_atom(X), !.

%%----------------------------------------

$pp_require_probabilistic_callable(X,MsgID,Source) :-
    ( $pp_is_probabilistic_callable(X) -> true
    ; $pp_raise_on_require([X],MsgID,Source,
                           $pp_error_probabilistic_callable)
    ).

$pp_error_probabilistic_callable(X,Error) :-
    $pp_error_probabilistic_atom(X,Error), !.
$pp_error_probabilistic_callable(X,type_error(probabilistic_callable,X)) :-
    \+ $pp_is_probabilistic_callable(X), !.

%%----------------------------------------

$pp_require_tabled_probabilistic_atom(X,MsgID,Source) :-
    ( $pp_is_tabled_probabilistic_atom(X) -> true
    ; $pp_raise_on_require([X],MsgID,Source,
                           $pp_error_tabled_probabilistic_atom)
    ).

$pp_error_tabled_probabilistic_atom(X,Error) :-
    $pp_error_probabilistic_atom(X,Error), !.
$pp_error_tabled_probabilistic_atom(X,type_error(tabled_probabilistic_atom,X)) :-
    \+ $pp_is_tabled_probabilistic_atom(X), !.

%%----------------------------------------

$pp_require_msw_declaration(MsgID,Source) :-
    ( current_predicate($pu_values/2) -> true
    ; $pp_raise_on_require([],MsgID,Source,$pp_error_msw_declaration)
    ).

$pp_error_msw_declaration(msw_declaration_not_found) :-
    \+ current_predicate($pu_values/2), !.

%%----------------------------------------

$pp_require_switch_outcomes(X,MsgID,Source) :-
    $pp_assert(ground(X)),
    ( current_predicate($pu_values/2),
      $pu_values(X,_)
          -> true
    ; $pp_raise_on_require([X],MsgID,Source,$pp_error_switch_outcomes)
    ).

$pp_error_switch_outcomes(_X,Error) :-
    $pp_error_msw_declaration(Error), !.
$pp_error_switch_outcomes(X,existence_error(outcome,X)) :-
    \+ $pu_values(X,_), !.

%%----------------------------------------

$pp_require_prism_flag(Flag,MsgID,Source) :-
    ( $pp_test_prism_flag(Flag) -> true
    ; $pp_raise_on_require([Flag],MsgID,Source,$pp_error_prism_flag)
    ).

$pp_test_prism_flag(Flag) :-
    atom(Flag),
    ( $pp_prism_flag(Flag,_,_,_)
    ; current_predicate($pp_prism_flag_renamed/2),
        $pp_prism_flag_renamed(Flag,Flag1),
        $pp_prism_flag(Flag1,_,_,_)
    ).

$pp_error_prism_flag(Flag,Error) :-
    $pp_error_atom(Flag,Error), !.
$pp_error_prism_flag(Flag,domain_error(prism_flag,Flag)) :-
    \+ $pp_test_prism_flag(Flag), !.

%%----------------------------------------

$pp_require_prism_flag_value(Flag,Value,MsgID,Source) :-
    $pp_assert($pp_test_prism_flag(Flag)),
    ( $pp_test_prism_flag_value(Flag,Value) -> true
    ; $pp_raise_on_require([Flag,Value],MsgID,Source,$pp_error_prism_flag_value)
    ).

$pp_test_prism_flag_value(Flag,Value) :-
    ground(Value),
    ( $pp_prism_flag(Flag,Type,_,_),
      $pp_check_prism_flag(Type,Value,_,_)
    ; current_predicate($pp_prism_flag_renamed/2),
        $pp_prism_flag_renamed(Flag,Flag1),
        $pp_prism_flag(Flag1,Type,_,_),
        $pp_check_prism_flag(Type,Value,_,_)
    ).

$pp_error_prism_flag_value(_Flag,Value,Error) :-
    $pp_error_ground(Value,Error), !.
$pp_error_prism_flag_value(Flag,Value,
                           domain_error(prism_flag_value(Flag),Value)) :-
    \+ $pp_test_prism_flag_value(Flag,Value), !.

%%----------------------------------------

$pp_require_distribution(X,MsgID,Source) :-
    get_prism_flag(error_on_invalid_distribution,F),
    ( $pp_test_distribution(X) -> true
    ; F=off ;$pp_raise_on_require([X],MsgID,Source,$pp_error_distribution)
    ).

% we do not check each element at this moment
$pp_test_distribution(X) :-
    ( $pp_test_fixed_size_distribution(X)
    ; $pp_test_variable_size_distribution(X)
    ).

$pp_test_variable_size_distribution(X) :-
    ground(X),
    ( X = uniform
    ; X = f_geometric
    ; X = f_geometric(Base)      -> number(Base), Base > 1
    ; X = f_geometric(Base,Type) -> number(Base), Base > 1, membchk(Type,[asc,desc])
    ; X = random
    ; X = noisy_u
    ; X = default
    ).

$pp_error_distribution(X,Error) :-
    $pp_error_ground(X,Error), !.
$pp_error_distribution(X,domain_error(distribution,X)) :-
    \+ $pp_test_distribution(X), !.

%%----------------------------------------

$pp_require_fixed_size_distribution(X,MsgID,Source) :-
    ( $pp_test_fixed_size_distribution(X) -> true
    ; $pp_raise_on_require([X],MsgID,Source,$pp_error_fixed_size_distribution)
    ).

% we do not check each element at this moment
$pp_test_fixed_size_distribution(X) :-
    ground(X),
    ( $pp_test_probabilities(X)
    ; $pp_test_probabilities_plus(X)
    ; $pp_test_ratio(X)
    ).

$pp_test_probabilities_plus(X) :-
    $pp_expr_to_list('+',X,Ps),
    length(Ps,L),
    L > 1,!,
    $pp_test_probabilities(Ps).

$pp_test_ratio(X) :-
    $pp_expr_to_list(':',X,Rs),
    length(Rs,L),
    L > 1,!,
    $pp_test_non_negative_numbers(Rs),
    \+ $pp_test_zeros(Rs).

$pp_test_zeros([]).
$pp_test_zeros([Z|Zs]):-
    -1.0e-15 < Z,
     1.0e-15 > Z,!,
    $pp_test_zeros(Zs).

$pp_error_fixed_size_distribution(X,Error) :-
    $pp_error_ground(X,Error), !.
$pp_error_fixed_size_distribution(X,domain_error(fixed_size_distribution,X)) :-
    \+ $pp_test_fixed_size_distribution(X), !.

%%----------------------------------------

$pp_require_probability(X,MsgID,Source) :-
    ( $pp_test_probability(X) -> true
    ; $pp_raise_on_require([X],MsgID,Source,$pp_error_probability)
    ).

$pp_test_probability(X) :-
    number(X),
    X >= 0.0,
    X =< 1.0.

$pp_error_probability(X,Error) :-
    $pp_error_number(X,Error), !.
$pp_error_probability(X,domain_error(probability,X)) :-
    ( X < 0.0 ;  X > 1.0 ), !.

%%----------------------------------------

$pp_require_probabilities(Ps,MsgID,Source) :-
    ( $pp_test_probabilities(Ps) -> true
    ; $pp_raise_on_require([Ps],MsgID,Source,$pp_error_probabilities)
    ).

$pp_test_probabilities(Ps) :-
    Ps = [_|_],
    $pp_test_probabilities1(Ps),
    sumlist(Ps,Sum),
    abs(Sum - 1.0) =< 1.0e-12.

$pp_test_probabilities1([]).
$pp_test_probabilities1([P|Ps]) :-
    $pp_test_probability(P),!,
    $pp_test_probabilities1(Ps).

$pp_error_probabilities(Ps,Error) :-
    $pp_error_list(Ps,Error), !.
$pp_error_probabilities(Ps,Error) :-
    member(P,Ps),
    $pp_error_probability(P,Error), !.
$pp_error_probabilities(Ps,domain_error(probabilities,Ps)) :-
    sumlist(Ps,Sum),
    abs(Sum - 1.0) > 1.0e-12, !.

%%----------------------------------------

$pp_require_hyperparameters(X,MsgID,Source) :-
    ( $pp_test_hyperparameters(X) -> true
    ; $pp_raise_on_require([X],MsgID,Source,$pp_error_hyperparameters)
    ).

$pp_test_hyperparameters(X) :-
    ( $pp_test_fixed_size_hyperparameters(X)
    ; $pp_test_variable_size_hyperparameters(X)
    ).

$pp_test_variable_size_hyperparameters(X) :-
    ground(X),
    ( number(X) -> X >= 0.0
    ; X = uniform
    ; X = uniform(U) -> number(U), U >= 0
    ; X = f_geometric
    ; X = f_geometric(Base) ->
        number(Base), Base > 1
    ; X = f_geometric(Init,Base) ->
        number(Init), Init >= 0,
        number(Base), Base >  1
    ; X = f_geometric(Init,Base,Type) ->
        number(Init), Init >= 0,
        number(Base), Base >  1,
        membchk(Type,[asc,desc])
    ; X = default
    ).

$pp_error_hyperparameters(X,Error) :-
    $pp_error_ground(X,Error), !.
$pp_error_hyperparameters(X,domain_error(hyperparameters,X)) :-
    \+ $pp_test_hyperparameters(X), !.

%%----------------------------------------

$pp_require_fixed_size_hyperparameters(X,MsgID,Source) :-
    ( $pp_test_fixed_size_hyperparameters(X) -> true
    ; $pp_raise_on_require([X],MsgID,Source,$pp_error_fixed_size_hyperparameters)
    ).

$pp_test_fixed_size_hyperparameters(X) :-
    ground(X),
    get_prism_flag(crf_enable,F),F=on->$pp_test_numbers(X);
    $pp_test_non_negative_numbers(X).

$pp_error_fixed_size_hyperparameters(X,Error) :-
    $pp_error_ground(X,Error), !.
$pp_error_fixed_size_hyperparameters(X,domain_error(fixed_size_hyperparameters,X)) :-
    \+ $pp_test_fixed_size_hyperparameters(X), !.

%%----------------------------------------

$pp_require_prism_option(X,MsgID,Source) :-
    ( $pp_test_prism_option(X) -> true
    ; $pp_raise_on_require([X],MsgID,Source,$pp_error_prism_option)
    ).

$pp_test_prism_option(X) :-
    ground(X),
    ( X = dump
    ; X = consult
    ; X = compile
    ; X = load
    ; X = v
    ; X = verb
    ; X = nv
    ; X = noverb
    ; X = consult
    ; X = (_=_)
    ).

$pp_error_prism_option(X,Error) :-
    $pp_error_ground(X,Error), !.
$pp_error_prism_option(X,domain_error(prism_option,X)) :-
    \+ $pp_test_prism_option(X), !.

%%----------------------------------------

% aggregate pattern is so flexible that we can only check if
% X is callable or not

$pp_require_hindsight_aggregate_pattern(X,MsgID,Source) :-
    ( $pp_test_hindsight_aggregate_pattern(X) -> true
    ; $pp_raise_on_require([X],MsgID,Source,
                           $pp_error_hindsight_aggregate_pattern)
    ).

$pp_test_hindsight_aggregate_pattern(X) :-
    callable(X).

$pp_error_hindsight_aggregate_pattern(X,Error) :-
    $pp_error_nonvar(X,Error), !.
$pp_error_hindsight_aggregate_pattern(X,Error) :-
    $pp_error_callable(X,Error), !.

%%----------------------------------------

$pp_require_saved_switch(X,MsgID,Source) :-
    ( $pp_test_saved_switch(X) -> true
    ; $pp_raise_on_require([X],MsgID,Source,$pp_error_saved_switch)
    ).

$pp_test_saved_switch(X) :-
    nonvar(X),
    X = switch(_,_,_,_).

$pp_error_saved_switch(X,Error) :-
    $pp_error_nonvar(X,Error), !.
$pp_error_saved_switch(X,domain_error(saved_switch,X)) :-
    \+ $pp_test_saved_switch(X), !.

%%----------------------------------------

$pp_require_write_callable(X,MsgID,Source) :-
    ( $pp_is_write_callable(X) -> true
    ; $pp_raise_on_require([X],MsgID,Source,$pp_write_callable)
    ).

$pp_error_write_callable(X,Error) :-
    $pp_error_nonvar(X,Error), !.
$pp_error_write_callable(X,Error) :-
    $pp_error_callable(X,Error), !.
$pp_error_write_callable(X,domain_error(write_callable,X)) :-
    \+ $pp_is_write_callable(X), !.
%%----------------------------------------

:- random_set_seed.

%%----------------------------------------

random_get_seed(Seed) :-
    global_get($pg_random_seed,Seed),!.

random_set_seed :-
    $pc_random_auto_seed(Seed),
    random_set_seed(Seed),!.

random_set_seed(Seed) :-
    $pp_require_random_seed(Seed,$msg(2000),random_set_seed/1),
    ( integer(Seed) -> $pc_random_init_by_seed(Seed)
    ; Seed ?= [_|_] -> $pc_random_init_by_list(Seed)
    ; %% else
      $pp_assert(fail)
    ), !,
    global_set($pg_random_seed,Seed),!.

random_get_state(State) :-
    $pc_random_get_state(State),!.

random_set_state(State) :-
    $pp_require_random_state(State,$msg(2001),random_set_state/1),
    $pc_random_set_state(State),!.

% deprecated predicates:

set_seed(Seed) :-
    $pp_raise_warning($msg(3300),[set_seed/1,random_set_seed/1]),
    random_set_seed(Seed).

set_seed_time :-
    $pp_raise_warning($msg(3300),[set_seed_time/0,random_set_seed/0]),
    random_set_seed.

set_seed_time(Seed) :-
    $pp_raise_warning($msg(3301),[set_seed_time/1]),
    random_set_seed,
    random_get_seed(Seed).

%%----------------------------------------

random_int(Max,Value) :-
    $pp_require_positive_integer(Max,$msg(2002),random_int/2),
    $pc_random_int(Max,Value).

random_int(Min,Max,Value) :-
    $pp_require_integer(Min,$msg(2003),random_int/3),
    $pp_require_integer(Max,$msg(2004),random_int/3),
    $pp_require_integer_range(Min,Max,$msg(2008),random_int/3),
    Max1 is Max - 1,
    $pc_random_int(Min,Max1,Value).

random_int_incl(Min,Max,Value) :-
    $pp_require_integer(Min,$msg(2003),random_int_incl/3),
    $pp_require_integer(Max,$msg(2004),random_int_incl/3),
    $pp_require_integer_range_incl(Min,Max,$msg(2008),random_int/3),
    $pc_random_int(Min,Max,Value).


random_int_excl(Min,Max,Value) :-
    $pp_require_integer(Min,$msg(2003),random_int_excl/3),
    $pp_require_integer(Max,$msg(2004),random_int_excl/3),
    $pp_require_integer_range_excl(Min,Max,$msg(2008),random_int/3),
    Min1 is Min + 1,
    Max1 is Max - 1,
    $pc_random_int(Min1,Max1,Value).

%%----------------------------------------

random_uniform(Value) :-
    $pc_random_float(Value).

random_uniform(Max,Value) :-
    $pp_require_positive_number(Max,$msg(2005),random_uniform/2),
    $pc_random_float(Value0),
    Value is Value0 * Max.

random_uniform(Min,Max,Value) :-
    $pp_require_number(Min,$msg(2006),random_uniform/3),
    $pp_require_number(Max,$msg(2007),random_uniform/3),
    $pp_require_number_range_excl(Min,Max,$msg(2008),random_uniform/3),
    $pc_random_float(Value0),
    Value is Value0 * (Max - Min) + Min.

random_gaussian(Value) :-
    $pc_random_gaussian(Value).

random_gaussian(Mu,Sigma,Value) :-
    $pp_require_number(Mu,$msg(2009),random_gaussian/3),
    $pp_require_positive_number(Sigma,$msg(2010),random_gaussian/3),
    $pc_random_gaussian(Value0),
    Value is Value0 * Sigma + Mu.

%%----------------------------------------

random_select(List,Value) :-
    random_select(List,uniform,Value).

random_select(List,Dist,Value) :-
    $pp_require_list(List,$msg(2011),random_select/3),
    $pp_require_distribution(Dist,$msg(0200),random_select/3),
    expand_values(List,List1),
    length(List1,L1),
    $pp_spec_to_ratio(Dist,L1,Ratio,random_select/3),
    length(Ratio,L2),
    ( L1 is L2 -> true
    ; $pp_raise_runtime_error($msg(0210),[List,Dist],unmatched_distribution,
                              random_select/3)
    ),
    sumlist(Ratio,Sum),
    random_uniform(Sum,Rand),!,
    $pp_random_select(Ratio,List1,Rand,Value).

$pp_random_select([X|Xs],[Y|Ys],R,Value) :-
    ( R >= X, Xs ?= [_|_] ->
      R1 is R - X, !, $pp_random_select(Xs,Ys,R1,Value)
    ; Y = Value
    ),!.

% deprecated predicates:

dice(List,Value) :-
    $pp_raise_warning($msg(3300),[dice/2,random_select/2]),
    random_select(List,Value).

dice(List,Dist,Value) :-
    $pp_raise_warning($msg(3300),[dice/3,random_select/3]),
    random_select(List,Dist,Value).

%%----------------------------------------

random_multiselect(List,N,Result) :-
    $pp_require_list(List,$msg(2011),random_multiselect/3),
    $pp_require_integer(N,$msg(2012),random_multiselect/3),
    length(List,L),
    ( \+ ( 1 =< N, N =< L ) ->
      $pp_raise_runtime_error($msg(2013),[N],
                              invalid_argument,random_multiselect/3)
    ; true
    ), !,
    new_bigarray(Elems,L),
    new_bigarray(Flags,L),
    $pp_random_multiselect1(1,L,Elems,Flags),
    M is L - N,
    ( N =< M ->
      $pp_random_multiselect2(1,N,L,Elems,Flags),
      $pp_random_multiselect3(1,1,Flags,List,Result)
    ; $pp_random_multiselect2(1,M,L,Elems,Flags),
      $pp_random_multiselect3(1,0,Flags,List,Result)
    ).

$pp_random_multiselect1(K,L,_,_), K > L =>
    true.
$pp_random_multiselect1(K,L,Elems,Flags), K =< L =>
    bigarray_put(Elems,K,K),
    bigarray_put(Flags,K,0),
    K1 is K + 1, !,
    $pp_random_multiselect1(K1,L,Elems,Flags).

$pp_random_multiselect2(K,N,_,_,_), K > N =>
    true.
$pp_random_multiselect2(K,N,L,Elems,Flags), K =< N =>
    random_int_incl(K,L,J),
    bigarray_get(Elems,K,VK),
    bigarray_get(Elems,J,VJ),
    bigarray_put(Elems,J,VK),
    bigarray_put(Elems,K,VJ),
    bigarray_put(Flags,VJ,1),
    K1 is K + 1, !,
    $pp_random_multiselect2(K1,N,L,Elems,Flags).

$pp_random_multiselect3(_,_,_,Xs,Ys), Xs = [] =>
    Ys = [].
$pp_random_multiselect3(K,Query,Flags,Xs,Ys), Xs = [X|Xs1] =>
    ( bigarray_get(Flags,K,Query) -> Ys = [X|Ys1] ; Ys = Ys1 ),
    K1 is K + 1, !,
    $pp_random_multiselect3(K1,Query,Flags,Xs1,Ys1).

%%----------------------------------------

random_group(List,N,Result) :-
    $pp_require_list(List,$msg(2011),random_group/3),
    $pp_require_positive_integer(N,$msg(2014),random_group/3),
    List = List1,
    new_bigarray(Array,N),
    $pp_random_group1(1,N,Array),
    $pp_random_group2(List1,N,Array),
    $pp_random_group3(1,N,Array,Result).

$pp_random_group1(K,N,_), K > N =>
    true.
$pp_random_group1(K,N,Array), K =< N =>
    bigarray_put(Array,K,Xs-Xs),
    K1 is K + 1, !,
    $pp_random_group1(K1,N,Array).

$pp_random_group2(Xs,_,_), Xs = [] =>
    true.
$pp_random_group2(Xs,N,Array), Xs = [X|Xs1] =>
    $pc_random_int(N,Z0),
    Z is Z0 + 1,
    bigarray_get(Array,Z,Ys0-Ys1),
    Ys1 = [X|Ys2],
    bigarray_put(Array,Z,Ys0-Ys2), !,
    $pp_random_group2(Xs1,N,Array).

$pp_random_group3(K,N,_,Xs), K > N =>
    Xs = [].
$pp_random_group3(K,N,Array,Xs), K =< N =>
    bigarray_get(Array,K,X-[]),
    Xs = [X|Xs1],
    K1 is K + 1, !,
    $pp_random_group3(K1,N,Array,Xs1).

%%----------------------------------------

random_shuffle(List0,List) :-
    $pp_require_list(List0,$msg(2011),random_shuffle/3),
    list_to_bigarray(List0,Array),
    bigarray_length(Array,Size),
    $pp_random_shuffle(1,Size,Array),
    bigarray_to_list(Array,List).

$pp_random_shuffle(K,N,_), K > N =>
    true.
$pp_random_shuffle(K,N,Array), K =< N =>
    random_int_incl(K,N,J),
    bigarray_get(Array,K,VK),
    bigarray_get(Array,J,VJ),
    bigarray_put(Array,J,VK),
    bigarray_put(Array,K,VJ),
    K1 is K + 1, !,
    $pp_random_shuffle(K1,N,Array).

%%----------------------------------------

$pp_require_random_seed(X,ID,Source) :-
    ( $pp_test_random_seed(X) -> true
    ; $pp_raise_on_require([X],ID,Source,$pp_error_random_seed)
    ).

$pp_test_random_seed(X), integer(X) => true.
$pp_test_random_seed(X), X = [Y],   integer(Y) => true.
$pp_test_random_seed(X), X = [Y|Z], integer(Y) =>
    Z ?= [_|_],
    $pp_test_random_seed(Z).

$pp_error_random_seed(X,instantiation_error) :-
    \+ ground(X), !.
$pp_error_random_seed(X,domain_error(random_seed,X)) :-
    \+ $pp_test_random_seed(X), !.

%%----------------------------------------

$pp_require_random_state(X,ID,Source) :-
    ( $pp_test_random_state(X) ->
      true
    ; $pp_raise_on_require([X],ID,Source,$pp_error_random_state)
    ).

$pp_test_random_state(X) :-
    functor(X,$randstate,833),
    $pp_test_random_state(X,1).

$pp_test_random_state(_,N), N >  833 => true.
$pp_test_random_state(X,N), N =< 833 =>
    arg(N,X,Arg),
    integer(Arg),
    N1 is N + 1,
    $pp_test_random_state(X,N1).

$pp_error_random_state(X,instantiation_error) :-
    \+ ground(X), !.
$pp_error_random_state(X,type_error(compound,X)) :-
    \+ compound(X), !.
$pp_error_random_state(X,domain_error(random_state,X)) :-
    \+ $pp_test_random_state(X), !.

%%----------------------------------------
%%--------------------------------
%%  Entry Point

$pp_format_message(Format,Args) :-
    $pp_format_message_loop(Format,Args).


%%--------------------------------
%%  Main Loop

$pp_format_message_loop([],_) :- !.
$pp_format_message_loop(Format,Args) :-
    Format = [Next|Format0],
    ( Next == 0'{ -> % '
      $pp_format_message_loop(Format0,Format1,Args)
    ; Next == 0'~ -> % '
      Format0 = [Code|Format1], $pp_format_message_char(Code)
    ; %% else
      Format0 = Format1, $pp_format_message_char(Next)
    ), !,
    $pp_format_message_loop(Format1,Args).

$pp_format_message_loop(Format0,Format1,Args) :-
    $pp_format_message_spec(Format0,Format1,N),
    nth(N,Args,Arg), !,
    $pp_format_message_term(Arg).
$pp_format_message_loop(Format0,Format0,_Args) :-
    $pp_format_message_char(0'{). % '


%%--------------------------------
%%  Format Spec Extraction

$pp_format_message_spec(Format0,Format1,N) :-
    $pp_format_message_spec(Format0,Format1,[],Codes),
    number_codes(N,Codes).

$pp_format_message_spec(Xs0,Xs1,Ys,Ys) :-
    Xs0 = [0'}|Xs1], !. % '
$pp_format_message_spec(Xs0,Xs1,Zs0,Ys) :-
    Xs0 = [X|Xs2],
    integer(X),
    X >= 48,
    X =< 57,
    Zs1 = [X|Zs0], !,
    $pp_format_message_spec(Xs2,Xs1,Zs1,Ys).


%%--------------------------------
%%  Output

$pp_format_message_char(Code) :-
    format("~c",[Code]).
$pp_format_message_term(Term) :-
    format("~w",[Term]).
% predicate_info
:- dynamic $pd_is_prob_pred/2.
:- dynamic $pd_is_tabled_pred/2.

% switch_info
:- dynamic $pd_parameters/3.
:- dynamic $pd_hyperparameters/4.
:- dynamic $pd_expectations/3.
:- dynamic $pd_hyperexpectations/3.
:- dynamic $pd_fixed_parameters/1.
:- dynamic $pd_fixed_hyperparameters/1.

% dummy_goals
:- dynamic $pd_dummy_goal_table/2.

% temporary byte-code files
:- dynamic $pd_tmp_out/2.

% learn_stats
:- dynamic $ps_log_likelihood/1.
:- dynamic $ps_log_post/1.
:- dynamic $ps_num_switches/1.
:- dynamic $ps_num_switch_values/1.
:- dynamic $ps_num_iterations/1.
:- dynamic $ps_num_iterations_vb/1.
:- dynamic $ps_bic_score/1.
:- dynamic $ps_cs_score/1.
:- dynamic $ps_free_energy/1.
:- dynamic $ps_learn_time/1.
:- dynamic $ps_learn_search_time/1.
:- dynamic $ps_em_time/1.
:- dynamic $ps_learn_table_space/1.

% graph_stats
:- dynamic $ps_num_subgraphs/1.
:- dynamic $ps_num_nodes/1.
:- dynamic $ps_num_goal_nodes/1.
:- dynamic $ps_num_switch_nodes/1.
:- dynamic $ps_avg_shared/1.

% infer_stats
:- dynamic $ps_infer_time/1.
:- dynamic $ps_infer_search_time/1.
:- dynamic $ps_infer_calc_time/1.

% mcmc_stats
:- dynamic $ps_mcmc_sample_time/1.
:- dynamic $ps_mcmc_marg_time/1.
:- dynamic $ps_mcmc_pred_time/1.
:- dynamic $ps_mcmc_exact_time/1.
%% -*- Prolog -*-

%%----------------------------------------
%%  Version and copyright statement

$pp_version('2.2a').
$pp_copyright('PRISM 2.2 alpha 1, (C) Sato Lab, Tokyo Institute of Technology, September, 2014').

get_version(V)  :- $pp_version(V).
print_version   :- $pp_version(V),     !, format("~w~n",[V]).
print_copyright :- $pp_copyright(Msg), !, format("~w~n",[Msg]).


%%----------------------------------------
%%  Operators

:- op(1160,xfx,times).

:- op(1150,fx,sample).
:- op(1150,fx,prob).
:- op(1150,fx,log_prob).
:- op(1150,fx,probf).
:- op(1150,fx,probfi).
:- op(1150,fx,probfo).
:- op(1150,fx,probfv).
:- op(1150,fx,probfio).
:- op(1150,fx,viterbi).
:- op(1150,fx,viterbif).
:- op(1150,fx,viterbig).
:- op(1150,fx,hindsight).
:- op(1150,fx,chindsight).

:- op(1150,fy,p_table).
:- op(1150,fy,p_not_table).

:- op(600,xfx,@).

:- op(950,fx,?? ).
:- op(950,fx,??*).
:- op(950,fx,??>).
:- op(950,fx,??<).
:- op(950,fx,??+).
:- op(950,fx,??-).


%%----------------------------------------
%%  Declarations

% only declarations. no effect when executed
p_table(_).
p_not_table(_).

:- table $prism_eg_path/3.
:- table $prism_expl_msw/3.
:- table $expl_interp_single_call/3.


%%----------------------------------------
%% Initializations

:- ( $pc_mp_mode -> true ; print_copyright ).
:- random_set_seed.
:- reset_prism_flags.
:- $pp_register_tmp_out.


%%----------------------------------------
%% Help messages

$help_mess("~nType 'prism_help' for usage.~n").   % Hook for B-Prolog

prism_help :-
    format(" prism(File)             -- compile and load a program~n",[]),
    format(" prism(Opts,File)        -- compile and load a program~n",[]),
    nl,
    format(" msw(I,V)                -- the switch I randomly outputs the value V~n",[]),
    nl,
    format(" learn(Gs)               -- learn the parameters~n",[]),
    format(" learn                   -- learn the parameters from data_source~n",[]),
    format(" sample(Goal)            -- get a sampled instance of Goal~n",[]),
    format(" prob(Goal,P)            -- compute a probability~n",[]),
    format(" probf(Goal,F)           -- compute an explanation graph~n",[]),
    format(" viterbi(Goal,P)         -- compute a Viterbi probability~n",[]),
    format(" viterbif(Goal,P,F)      -- compute a Viterbi probability with its explanation~n",[]),
    format(" hindsight(Goal,Patt,Ps) -- compute hindsight probabilities~n",[]),
    nl,
    format(" set_sw(Sw,Params)       -- set parameters of a switch~n",[]),
    format(" get_sw(Sw,SwInfo)       -- get information of a switch~n",[]),
    format(" set_prism_flag(Flg,Val) -- set a new value to a flag~n",[]),
    format(" get_prism_flag(Flg,Val) -- get the current value of a flag~n",[]),
    nl,
    format(" please consult the user's manual for details.~n",[]).


%%----------------------------------------
%% Loading a program

prism(File) :-
    prism([],File).

prism(Opts,File) :-
    $pp_require_atom(File,$msg(3000),prism/2),
    $pp_require_list_or_nil(Opts,$msg(1006),prism/2),
    reset_prism_flags,
    $pp_set_options(Opts),
    ( member(consult,Opts) ->
        $pp_search_file(File,File1,[".psm",""]),
        Pred = $pp_consult(File1)
    ; member(load,Opts) ->
        $pp_search_file(File,File1,[".psm.out",".out",""]),
        Pred = $pp_load(File1)
    ; ( member(dump,Opts) -> D = 1 ; D = 0 ),
        global_set($pg_dump_compiled,D),
        $pp_search_file(File,File1,[".psm",""]),
        Pred = $pp_compile_load(File1)
    ),!,
    global_del(failure,0),
    global_set($pg_dummy_goal_count,0),
    call(Pred),!.
prism(_Opts,File) :-
    $pp_raise_existence_error($msg(3001),[File],
                              [prism_file,File],existence_error).

$pp_compile_load(File) :-
    $pp_add_out_extension(File,OutFile),
    $pp_clean_dynamic_preds, 
    $pp_compile(File,_DmpFile,OutFile),
    $pp_load(OutFile).

$pp_load(File) :-
    not(not($myload(File))),
    $pp_init_tables_aux,
    $pp_init_tables,!.
% We do not perform translation
%   -- the explanation search will be done by meta-interpreters
$pp_consult(File) :-
    $pp_clean_dynamic_preds, 
    new_hashtable(PPredTab),
    Info = $trans_info(_DoTable,_TPredTab,_NoDebug,PPredTab),
    $pp_bpif_read_program(Prog,File),
    $pp_extract_decls(Prog,Info),
    $pp_trans_values(Prog,Prog1),
    $pp_analyze(Prog1,Info),
    $pp_tabled_to_nontabled(Prog1,Prog2),
    assert($pd_is_tabled_pred($disabled_by_consult,0)),
    $pp_separate_demon_load(Prog2,Prog3,Prog4),
         % $damon_load/0 should be consulted after loading the entire program
    $pp_consult_preds(Prog4,_ProgCompiled),
    $pp_consult_preds(Prog3,_ProgCompiled),
    $pp_init_tables_aux,
    $pp_init_tables.


$pp_set_options([]).
$pp_set_options([O|Options]) :-
    $pp_require_prism_option(O,$msg(1001),prism/2),
    $pp_set_one_option(O),!,
    $pp_set_options(Options).

$pp_set_one_option(dump)    :- true.
$pp_set_one_option(consult) :- true.
$pp_set_one_option(compile) :- true.
$pp_set_one_option(load)    :- true.
$pp_set_one_option(v)       :- set_prism_flag(verb,full).
$pp_set_one_option(verb)    :- set_prism_flag(verb,full).
$pp_set_one_option(nv)      :- set_prism_flag(verb,none).
$pp_set_one_option(noverb)  :- set_prism_flag(verb,none).
$pp_set_one_option(Att=Val) :- set_prism_flag(Att,Val).


%%----------------------------------------
%% Clean up databases

$pp_clean_dynamic_preds :-
    $pp_clean_predicate_info,
    $pp_clean_switch_info,
    $pp_clean_dummy_goal_table,
    $pp_clean_graph_stats,
    $pp_clean_learn_stats,
    $pp_clean_infer_stats.

$pp_clean_predicate_info :-
    retractall($pd_is_prob_pred(_,_)),
    retractall($pd_is_tabled_pred(_,_)),!.

$pp_clean_switch_info :-
    retractall($pd_parameters(_,_,_)),
    retractall($pd_hyperparameters(_,_,_,_)),
    retractall($pd_expectations(_,_,_)),
    retractall($pd_hyperexpectations(_,_,_)),
    retractall($pd_fixed_parameters(_)),
    retractall($pd_fixed_hyperparameters(_)),!.

$pp_init_tables :-
    initialize_table,
    $pc_prism_id_table_init,
    $pc_clean_base_egraph,  % base support graph and switches
    $pc_alloc_egraph,       % get ready for the following steps
    $pc_clean_mcmc_samples, % mcmc samples
    global_set($pg_dummy_goal_count,0).

$pp_init_tables_if_necessary :-
    ( get_prism_flag(clean_table, on) -> $pp_init_tables
    ; true
    ),!.

$pp_init_tables_aux :-
    $pc_clean_egraph,      % derived external graphs
    $pc_clean_external_tables,!.


%%----------------------------------------
%% Show the program information

show_values :-
    format("Outcome spaces:~n",[]),!,
    findall([Sw,Vals],($pp_registered_sw(Sw),get_values1(Sw,Vals)),SwVals0),
    sort(SwVals0,SwVals1),
    $pp_show_values_list(SwVals1),!.

$pp_show_values_list([]).
$pp_show_values_list([[Sw,Vals]|SwVals]) :-
    format("  ~q: ~q~n",[Sw,Vals]),!,
    $pp_show_values_list(SwVals).

%% (Note) $pd_is_{prob,tabled}_pred/2 are dynamic, so we don't have to call
%%        current_predicate/1.  We don't check the input rigorously either
%%        for flexibility.

is_prob_pred(F/N) :- is_prob_pred(F,N).
is_prob_pred(F,N) :- $pd_is_prob_pred(F,N).

is_tabled_pred(F/N) :- is_tabled_pred(F,N).
is_tabled_pred(F,N) :- $pd_is_tabled_pred(F,N).

show_prob_preds :-
    format("Probabilistic predicates:~n",[]),!,
    findall(F0/N0,is_prob_pred(F0,N0),Preds0),
    sort(Preds0,Preds),
    ( member(F/N,Preds),
      format("  ~q/~w~n",[F,N]),
      fail
    ; true
    ),!.

show_tabled_preds :-
    $pd_is_tabled_pred($disabled_by_consult,_),!,
    $pp_raise_warning($msg(1002)).

show_tabled_preds :-
    format("Tabled probabilistic predicates:~n",[]),!,
    findall(F0/N0,is_tabled_pred(F0,N0),Preds0),
    sort(Preds0,Preds),
    ( member(F/N,Preds),
      format("  ~q/~w~n",[F,N]),
      fail
    ; true
    ),!.

%% aliases
show_prob_pred    :- show_prob_preds.
show_table_pred   :- show_tabled_preds.
show_table_preds  :- show_tabled_preds.
show_tabled_pred  :- show_tabled_preds.


%%----------------------------------------
%% Predicates for batch (non-interactive) execution

$pp_batch :-
    catch($pp_batch_core,Err,$pp_batch_error(Err)).

$pp_batch_error(Err) :-
    Err == abort,!.
$pp_batch_error(Err) :-
    Err == interrupt,!,
    format("Aborted by interruption~n",[]),
    abort.
$pp_batch_error(Err) :-
    format("Aborted by exception -- ~w~n",[Err]),
    abort.

$pp_batch_core :-
    get_main_args([Arg|Args]),!,
    $pp_batch_load(Arg,File),
    $pp_batch_main(Args,File).
$pp_batch_core :-
    $pp_raise_existence_error($msg(1003),[prism_file,unknown],$pp_batch/1).

$pp_batch_load(Arg,File) :-
    ( atom_chars(Arg,[p,r,i,s,m,  ':'|FileChars]) ->
        atom_chars(File,FileChars), FileChars \== [], prism(File)
    ; atom_chars(Arg,[p,r,i,s,m,n,':'|FileChars]) ->
        atom_chars(File,FileChars), FileChars \== [], prismn(File)
    ; atom_chars(Arg,[l,o,a,d,    ':'|FileChars]) ->
        atom_chars(File,FileChars), FileChars \== [], prism([load],File)
    ; prism(Arg), File = Arg
    ),!.

$pp_batch_main(Args,File) :-
    ( current_predicate(prism_main/1) -> Goal = prism_main(Args)
    ; current_predicate(prism_main/0) -> Goal = prism_main
    ; $pp_raise_existence_error($msg(1004),[File],[batch_predicate,File],
                                $pp_batch_main/2)
    ),!,
    %% use of call/1 is for the parallel version
    call($pp_batch_call(Goal)).


%%----------------------------------------
%% Registeration of the name of a temporary byte-code file used for
%% the current process

$pp_register_tmp_out :-
    retractall($pd_tmp_out(_,_)),
    random_int(1048576,R),
    $pp_build_tmp_out(R,TmpOutBase,TmpOutFull),
    assert($pd_tmp_out(TmpOutBase,TmpOutFull)),!.
    
$pp_build_tmp_out(R,TmpOutBase,TmpOutFull) :-
    number_codes(R,Codes0),
    append("__tmp",Codes0,Codes1),
    append(Codes1,".out",Codes2),
    atom_codes(TmpOutBase,Codes1),
    atom_codes(TmpOutFull,Codes2).


%%----------------------------------------
%% Miscellaneous routines

$pp_tabled_to_nontabled([],Prog) => Prog = [].
$pp_tabled_to_nontabled([pred(F,N,M,Delay,_Tabled,Cls)|Preds],Prog) => 
    Prog = [pred(F,N,M,Delay,_,Cls)|Prog1], !,
    $pp_tabled_to_nontabled(Preds,Prog1).


$pp_separate_demon_load([],[],[]).
$pp_separate_demon_load([pred($damon_load,0,X0,X1,X2,X3)|Prog0],
                        [pred($damon_load,0,X0,X1,X2,X3)|Prog1],
                        Prog2) :- !,
    $pp_separate_demon_load(Prog0,Prog1,Prog2).
$pp_separate_demon_load([P|Prog0],Prog1,[P|Prog2]) :- !,
    $pp_separate_demon_load(Prog0,Prog1,Prog2).


$pp_search_file(File,File1,Suffixes) :-
    member(Suffix,Suffixes),
    $pp_add_extension(File,File1,Suffix),
    exists(File1),!.


$pp_add_psm_extension(File,PsmFile) :-
    $pp_add_extension(File,PsmFile,".psm").

$pp_add_out_extension(File,OutFile) :-
    $pp_add_extension(File,OutFile,".out").

$pp_add_extension(File,File1,Extension) :-
    ( atom(File) -> name(File,FileString)
    ; File ?= [_|_] -> File = FileString
    ; $pp_raise_domain_error($msg(1000),[File],[filename,File],
                             $pp_add_extension/3)
    ),
    append(FileString,Extension,FileString1),
    name(File1,FileString1).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% set_sw/1,set_sw/2: initialize the prob. of MSW

set_sw(Sw) :- set_sw(Sw,default).

set_sw(Sw,Dist) :-
    $pp_require_ground(Sw,$msg(0101),set_sw/2),
    $pp_require_switch_outcomes(Sw,$msg(0102),set_sw/2),
    (get_prism_flag(crf_enable,on) -> $pp_require_crf_params(Dist,$msg(0200),set_sw/2)
       ;$pp_require_distribution(Dist,$msg(0200),set_sw/2)),
    $pp_set_sw(Sw,Dist).

$pp_set_sw(Sw,Dist) :-
    ( $pd_fixed_parameters(Sw) -> $pp_raise_warning($msg(0109),[Sw])
    ; $pp_get_values(Sw,Values),
      length(Values,N),
      (get_prism_flag(crf_enable,on) -> expand_fprobs(Dist,N,Probs);expand_probs(Dist,N,Probs)),
      ( retract($pd_parameters(Sw,_,_)) -> true ; true),
      assert($pd_parameters(Sw,Values,Probs))
    ),!.

%% set_sw_all(Sw): set parameters to all switches that matches with Sw.

set_sw_all          :- $pp_set_sw_all(_,default).
set_sw_all(Sw)      :- $pp_set_sw_all(Sw,default).
set_sw_all(Sw,Dist) :- $pp_set_sw_all(Sw,Dist).

$pp_set_sw_all(Sw,Dist) :-
    findall(Sw,$pp_registered_sw(Sw),Sws),
    $pp_set_sw_list(Sws,Dist),!.

$pp_set_sw_list([],_).
$pp_set_sw_list([Sw|Sws],Dist) :-
    set_sw(Sw,Dist),!,
    $pp_set_sw_list(Sws,Dist).

% fix switches
fix_sw(Sw,Dist) :-
    $pp_require_ground(Sw,$msg(0101),fix_sw/2),
    $pp_require_switch_outcomes(Sw,$msg(0102),fix_sw/2),
    (get_prism_flag(crf_enable,on) -> $pp_require_crf_params(Dist,$msg(0200),fix_sw/2)
       ;$pp_require_distribution(Dist,$msg(0200),set_sw/2)),
    $pp_unfix_sw(Sw),
    $pp_set_sw(Sw,Dist),
    $pp_fix_sw(Sw),!.

fix_sw(Sw) :- var(Sw),!,
    ( get_sw(switch(Sw1,_,_,_)),
      fix_sw(Sw1),
      fail
    ; true
    ).
fix_sw(Sw) :- Sw = [_|_],!,
    $pp_fix_sw_list(Sw).
fix_sw(Sw) :-
    ( $pd_parameters(Sw,_,_),
      $pp_fix_sw(Sw),
      fail
    ; true
    ).

$pp_fix_sw_list([]).
$pp_fix_sw_list([Sw|Sws]) :-
    fix_sw(Sw),!,
    $pp_fix_sw_list(Sws).

$pp_fix_sw(Sw) :-
    ( $pd_fixed_parameters(Sw) -> true
    ; assert($pd_fixed_parameters(Sw))
    ).

unfix_sw(Sw) :- var(Sw),!,
    ( get_sw(switch(Sw1,_,_,_)),
      unfix_sw(Sw1),
      fail
    ; true
    ).
unfix_sw(SwList) :- SwList = [_|_],!,$pp_unfix_sw_list(SwList).
unfix_sw(Sw) :-
    ( $pd_parameters(Sw,_,_),
      $pp_unfix_sw(Sw),
      fail
    ; true
    ).

$pp_unfix_sw_list([]).
$pp_unfix_sw_list([Sw|Sws]) :-
    $pp_unfix_sw(Sw),!,
    $pp_unfix_sw_list(Sws).

$pp_unfix_sw(Sw) :-
    ( retract($pd_fixed_parameters(Sw)) -> true ; true).

% show msw
show_sw :- show_sw(_).

show_sw(Sw) :-
   findall(Sw,$pp_registered_sw(Sw),Sws0),
   sort(Sws0,Sws),
   $pp_show_sw_list(Sws).

$pp_show_sw_list([]) :- !.
$pp_show_sw_list([Sw|Sws]) :-!,
   $pp_show_sw1(Sw),!,
   $pp_show_sw_list(Sws).

% We can assume Sw is ground
$pp_show_sw1(Sw) :-
    $pp_get_parameters(Sw,Values,Probs),
    format("Switch ~w: ",[Sw]),
    ( $pd_fixed_parameters(Sw) -> write('fixed_p:') ; write('unfixed_p:') ),
    $pp_show_sw_values(Values,Probs),
    nl.

$pp_show_sw_values([],_Ps).
$pp_show_sw_values([V|Vs],[P|Ps]) :-
    format(" ~w (p: ~9f)",[V,P]),!,
    $pp_show_sw_values(Vs,Ps).

get_sw(Sw) :-
    get_sw(SwName,Status,Values,Probs),
    Sw = switch(SwName,Status,Values,Probs).

get_sw(Sw,[Status,Values,Probs]) :-
    get_sw(Sw,Status,Values,Probs).

% - Inconsitency of outcome spaces are checked in advance in
%   $pp_get_parameters/3 and $pp_get_expectations/3.

get_sw(Sw,Status,Values,Probs) :-
    $pp_get_parameters(Sw,Values,Probs),
    ( $pd_fixed_parameters(Sw) -> Status = fixed ; Status = unfixed ).
    
get_sw(Sw,Status,Values,Probs,Es) :-
    $pp_get_parameters(Sw,Values,Probs),
    $pp_get_expectations(Sw,_,Es),
    ( $pd_fixed_parameters(Sw) -> Status = fixed ; Status = unfixed ).

%% save/restore switch information

save_sw :- save_sw('Saved_SW').

save_sw(File) :-
    open(File,write,Stream),
    $pp_call_with_stream(Stream,$pp_save_sw_core(Stream)),!.

$pp_save_sw_core(Stream) :-
    ( get_sw(SwName,Status,Values,Probs),
      format(Stream,"switch(~q,~q,~q,",[SwName,Status,Values]),
      $pp_write_distribution(Stream,Probs,'['),
      format(Stream,"]).~n",[]),
      fail
    ; true
    ).

$pp_write_distribution(_,[],_).
$pp_write_distribution(Stream,[Prob|Probs],C) :-
    format(Stream,"~w~15e",[C,Prob]),!,
    $pp_write_distribution(Stream,Probs,',').

restore_sw :- restore_sw('Saved_SW').

restore_sw(File) :-
    open(File,read,Stream),
    $pp_call_with_stream(Stream,$pp_restore_sw_core(Stream)),!.

$pp_restore_sw_core(Stream) :-
    repeat,
    read(Stream,Switch),
    ( Switch == end_of_file
    ; $pp_require_saved_switch(Switch,$msg(0111),restore_sw/1),
      Switch = switch(ID,_,_,Params),
      set_sw(ID,Params),
      fail
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% set_sw_{a,d}/1-2: initialize the hyperparameters of MSW

set_sw_a(Sw) :- set_sw_a(Sw,default).

set_sw_a(Sw,Spec) :-
    $pp_require_ground(Sw,$msg(0101),set_sw_a/2),
    $pp_require_switch_outcomes(Sw,$msg(0102),set_sw_a/2),
    $pp_require_hyperparameters(Spec,$msg(0208),set_sw_a/2),
    $pp_set_sw_a(Sw,Spec).

$pp_set_sw_a(Sw,Spec) :-
    ( $pd_fixed_hyperparameters(Sw) -> $pp_raise_warning($msg(0110),[Sw])
    ; $pp_get_values(Sw,Values),
      length(Values,N),
      $pp_expand_pseudo_counts(set_sw_a/2,Spec,N,Alphas,Deltas),
      ( retract($pd_hyperparameters(Sw,_,_,_)) -> true ; true),
      assert($pd_hyperparameters(Sw,Values,Alphas,Deltas))
    ),!.

set_sw_d(Sw) :- set_sw_d(Sw,default).

set_sw_d(Sw,Spec) :-
    $pp_require_ground(Sw,$msg(0101),set_sw_d/2),
    $pp_require_switch_outcomes(Sw,$msg(0102),set_sw_d/2),
    $pp_require_hyperparameters(Spec,$msg(0209),set_sw_d/2),
    $pp_set_sw_d(Sw,Spec).

$pp_set_sw_d(Sw,Spec) :-
    ( $pd_fixed_hyperparameters(Sw) -> $pp_raise_warning($msg(0110),[Sw])
    ; $pp_get_values(Sw,Values),
      length(Values,N),
      $pp_expand_pseudo_counts(set_sw_d/2,Spec,N,Alphas,Deltas),
      ( retract($pd_hyperparameters(Sw,_,_,_)) -> true ; true),
      assert($pd_hyperparameters(Sw,Values,Alphas,Deltas))
    ),!.

% wrapper for getting alphas and deltas
$pp_expand_pseudo_counts(Caller,Spec,N,Alphas,Deltas) :-
    expand_pseudo_counts(Spec,N,Hs),
    ( Spec = default ->
      ( get_prism_flag(default_sw_a,$disabled) -> Mode = delta
      ; Mode = alpha
      )
    ; Caller = set_sw_a/2 -> Mode = alpha
    ; Mode = delta
    ),
    ( Mode = alpha ->
        Alphas = Hs,
        ((get_prism_flag(crf_enable,on) -> $pp_test_numbers(Alphas);$pp_test_positive_numbers(Alphas)) -> true
        ; $pp_raise_domain_error($msg(0208),[Spec],[alphas,Spec],Caller)
        ),                                        % a bit dirty
        $pp_alpha_to_delta(Alphas,Deltas)
    ; % Mode = delta
        Deltas = Hs,
        $pp_delta_to_alpha(Deltas,Alphas)
    ).

%% aliases for backward compatibility
set_sw_h(Sw)      :- set_sw_d(Sw).
set_sw_h(Sw,Spec) :- set_sw_d(Sw,Spec).

%%% set_sw_all_{a,d}(Sw):
%%%     set hyperparameters to all switches that matches with Sw.

set_sw_all_a :- set_sw_all_a(_).

set_sw_all_a(Sw) :- set_sw_all_a(Sw,default).

set_sw_all_a(Sw,Spec) :-
    findall(Sw,$pp_registered_sw(Sw),Sws),
    $pp_set_sw_a_list(Sws,Spec),!.

$pp_set_sw_a_list([],_).
$pp_set_sw_a_list([Sw|Sws],Spec) :-
    set_sw_a(Sw,Spec),!,
    $pp_set_sw_a_list(Sws,Spec).


set_sw_all_d :- set_sw_all_d(_).

set_sw_all_d(Sw) :- set_sw_all_d(Sw,default).

set_sw_all_d(Sw,Spec) :-
    findall(Sw,$pp_registered_sw(Sw),Sws),
    $pp_set_sw_d_list(Sws,Spec),!.

$pp_set_sw_d_list([],_).
$pp_set_sw_d_list([Sw|Sws],Spec) :-
    set_sw_d(Sw,Spec),!,
    $pp_set_sw_d_list(Sws,Spec).

%% aliases for backward compatibility

set_sw_all_h          :- set_sw_all_d.
set_sw_all_h(Sw)      :- set_sw_all_d(Sw).
set_sw_all_h(Sw,Spec) :- set_sw_all_d(Sw,Spec).

set_sw_a_all          :- set_sw_all_a.
set_sw_a_all(Sw)      :- set_sw_all_a(Sw).
set_sw_a_all(Sw,Spec) :- set_sw_all_a(Sw,Spec).

set_sw_d_all          :- set_sw_all_d.
set_sw_d_all(Sw)      :- set_sw_all_d(Sw).
set_sw_d_all(Sw,Spec) :- set_sw_all_d(Sw,Spec).

set_sw_h_all          :- set_sw_all_h.
set_sw_h_all(Sw)      :- set_sw_all_h(Sw).
set_sw_h_all(Sw,Spec) :- set_sw_all_h(Sw,Spec).

%%% fix_sw_h(Sw,Spec) :- fix the hyperparameters of Sw at Spec

fix_sw_a(Sw,Spec) :-
    $pp_require_ground(Sw,$msg(0101),fix_sw_a/2),
    $pp_require_switch_outcomes(Sw,$msg(0102),fix_sw_a/2),
    $pp_require_hyperparameters(Spec,$msg(0208),fix_sw_a/2),
    $pp_unfix_sw_h(Sw),
    $pp_set_sw_a(Sw,Spec),
    $pp_fix_sw_h(Sw),!.

fix_sw_a(Sw) :- var(Sw),!,
    ( get_sw_a(switch(Sw1,_,_,_)),
      fix_sw_a(Sw1),
      fail
    ; true
    ).
fix_sw_a(Sw) :- Sw = [_|_],!,
    $pp_fix_sw_a_list(Sw).
fix_sw_a(Sw) :-
    ( $pd_hyperparameters(Sw,_,_,_),
      $pp_fix_sw_h(Sw),
      fail
    ; true
    ),!.

$pp_fix_sw_a_list([]).
$pp_fix_sw_a_list([Sw|Sws]) :-
    fix_sw_a(Sw),!,
    $pp_fix_sw_a_list(Sws).

fix_sw_d(Sw,Spec) :-
    $pp_require_ground(Sw,$msg(0101),fix_sw_d/2),
    $pp_require_switch_outcomes(Sw,$msg(0102),fix_sw_d/2),
    $pp_require_hyperparameters(Spec,$msg(0209),fix_sw_d/2),
    $pp_unfix_sw_h(Sw),
    $pp_set_sw_d(Sw,Spec),
    $pp_fix_sw_h(Sw),!.

fix_sw_d(Sw) :- var(Sw),!,
    ( get_sw_d(switch(Sw1,_,_,_)),
      fix_sw_d(Sw1),
      fail
    ; true
    ).
fix_sw_d(Sw) :- Sw = [_|_],!,
    $pp_fix_sw_d_list(Sw).
fix_sw_d(Sw) :-
    ( $pd_hyperparameters(Sw,_,_,_),
      $pp_fix_sw_h(Sw),
      fail
    ; true
    ),!.

$pp_fix_sw_d_list([]).
$pp_fix_sw_d_list([Sw|Sws]) :-
    fix_sw_d(Sw),!,
    $pp_fix_sw_d_list(Sws).

$pp_fix_sw_h(Sw) :-
    ( clause($pd_fixed_hyperparameters(Sw),_) -> true
    ; assert($pd_fixed_hyperparameters(Sw))
    ).

%% aliases for backward compatibility

fix_sw_h(Sw,Spec) :- fix_sw_d(Sw,Spec).
fix_sw_h(Sw)      :- fix_sw_d(Sw).

%%% unfix_sw_{a,d}(Sw) :- unfix the hyperparameters of Sw

unfix_sw_d(Sw) :- var(Sw),!,
    ( get_sw_d(switch(Sw1,_,_,_)),
      unfix_sw_d(Sw1),
      fail
    ; true
    ).
unfix_sw_d(SwList) :- SwList = [_|_],!,
    $pp_unfix_sw_d_list(SwList).
unfix_sw_d(Sw) :-
    ( $pd_hyperparameters(Sw,_,_,_),
      $pp_unfix_sw_h(Sw),
      fail
    ; true
    ),!.

$pp_unfix_sw_d_list([]).
$pp_unfix_sw_d_list([Sw|Sws]) :-
    unfix_sw_d(Sw),!,
    $pp_unfix_sw_d_list(Sws).

$pp_unfix_sw_h(Sw) :-
    ( retract($pd_fixed_hyperparameters(Sw)) -> true
    ; true
    ).

%% aliases

unfix_sw_a(Sw) :- unfix_sw_d(Sw).
unfix_sw_h(Sw) :- unfix_sw_d(Sw).

%%% show hyperparameters

show_sw_a :- show_sw_a(_).

show_sw_a(Sw) :-
   findall(Sw,$pp_registered_sw(Sw),Sws0),
   sort(Sws0,Sws),
   $pp_show_sw_list_a(Sws).

$pp_show_sw_list_a([]) :- !.
$pp_show_sw_list_a([Sw|Sws]) :- !,
   $pp_show_sw1_a(Sw),!,
   $pp_show_sw_list_a(Sws).

$pp_show_sw1_a(Sw) :-
    $pp_get_hyperparameters(Sw,Values,Alphas,_),
    format("Switch ~w: ",[Sw]),
    ( $pd_fixed_hyperparameters(Sw) -> write('fixed_h:') ; write('unfixed_h:') ),
    $pp_show_sw_a_values(Values,Alphas),
    nl.

$pp_show_sw_a_values([],_).
$pp_show_sw_a_values([V|Vs],[A|As]) :-
    format(" ~w (a: ~9f)",[V,A]),!,
    $pp_show_sw_a_values(Vs,As).

show_sw_d :- show_sw_d(_).

show_sw_d(Sw) :-
   findall(Sw,$pp_registered_sw(Sw),Sws0),
   sort(Sws0,Sws),
   $pp_show_sw_list_d(Sws).

$pp_show_sw_list_d([]) :- !.
$pp_show_sw_list_d([Sw|Sws]) :- !,
   $pp_show_sw1_d(Sw),!,
   $pp_show_sw_list_d(Sws).

$pp_show_sw1_d(Sw) :-
    $pp_get_hyperparameters(Sw,Values,_,Deltas),
    format("Switch ~w: ",[Sw]),
    ( $pd_fixed_hyperparameters(Sw) -> write('fixed_h:') ; write('unfixed_h:') ),
    $pp_show_sw_d_values(Values,Deltas),
    nl.

$pp_show_sw_d_values([],_).
$pp_show_sw_d_values([V|Vs],[D|Ds]) :-
    format(" ~w (d: ~9f)",[V,D]),!,
    $pp_show_sw_d_values(Vs,Ds).

%% aliases

show_sw_h     :- show_sw_d.
show_sw_h(Sw) :- show_sw_d(Sw).

%%% show both parameters and hyperparameters

show_sw_pa :- show_sw_pa(_).

show_sw_pa(Sw) :-
    findall(Sw,$pp_registered_sw(Sw),Sws0),
    sort(Sws0,Sws),
    $pp_show_sw_list_pa(Sws).

$pp_show_sw_list_pa([]) :- !.
$pp_show_sw_list_pa([Sw|Sws]) :- !,
    $pp_show_sw1_pa(Sw),!,
    $pp_show_sw_list_pa(Sws).

$pp_show_sw1_pa(Sw) :-
    $pp_get_parameters(Sw,Values,Probs),
    $pp_get_hyperparameters(Sw,_,Alphas,_),
    format("Switch ~w: ",[Sw]),
    ( $pd_fixed_parameters(Sw)      -> write('fixed_p,') ; write('unfixed_p,') ),
    ( $pd_fixed_hyperparameters(Sw) -> write('fixed_h:') ; write('unfixed_h:') ),
    $pp_show_sw_pa_values(Values,Probs,Alphas),
    nl,!.

$pp_show_sw_pa_values([],_,_).

$pp_show_sw_pa_values([V|Vs],[P|Ps],[A|As]) :-
    format(" ~w (p: ~9f, a: ~9f)",[V,P,A]),!,
    $pp_show_sw_pa_values(Vs,Ps,As).

$pp_show_sw_pa_values([V|Vs],[P|Ps],$not_assigned) :-
    format(" ~w (p: ~9f, a: n/a)",[V,P]),!,
    $pp_show_sw_pa_values(Vs,Ps,$not_assigned).

$pp_show_sw_pa_values([V|Vs],$not_assigned,[A|As]) :-
    format(" ~w (p: n/a, a: ~9f)",[V,A]),!,
    $pp_show_sw_pa_values(Vs,$not_assigned,As).

show_sw_pd :- show_sw_pd(_).

show_sw_pd(Sw) :-
    findall(Sw,$pp_registered_sw(Sw),Sws0),
    sort(Sws0,Sws),
    $pp_show_sw_list_pd(Sws).

$pp_show_sw_list_pd([]) :- !.
$pp_show_sw_list_pd([Sw|Sws]) :- !,
    $pp_show_sw1_pd(Sw),!,
    $pp_show_sw_list_pd(Sws).

$pp_show_sw1_pd(Sw) :-
    $pp_get_parameters(Sw,Values,Probs),
    $pp_get_hyperparameters(Sw,_,_,Deltas),
    format("Switch ~w: ",[Sw]),
    ( $pd_fixed_parameters(Sw)      -> write('fixed_p,') ; write('unfixed_p,') ),
    ( $pd_fixed_hyperparameters(Sw) -> write('fixed_h:') ; write('unfixed_h:') ),
    $pp_show_sw_pd_values(Values,Probs,Deltas),
    nl,!.

$pp_show_sw_pd_values([],_,_).

$pp_show_sw_pd_values([V|Vs],[P|Ps],[D|Ds]) :-
    format(" ~w (p: ~9f, d: ~9f)",[V,P,D]),!,
    $pp_show_sw_pd_values(Vs,Ps,Ds).

$pp_show_sw_pd_values([V|Vs],[P|Ps],$not_assigned) :-
    format(" ~w (p: ~9f, d: n/a)",[V,P]),!,
    $pp_show_sw_pd_values(Vs,Ps,$not_assigned).

$pp_show_sw_pd_values([V|Vs],$not_assigned,[D|Ds]) :-
    format(" ~w (p: n/a, d: ~9f)",[V,D]),!,
    $pp_show_sw_pd_values(Vs,$not_assigned,Ds).

%% aliases

show_sw_b     :- show_sw_pd.
show_sw_b(Sw) :- show_sw_pd(Sw).

%%% get switch information including hyperparameters

get_sw_a(Sw) :-
    get_sw_a(SwName,Status,Values,Alphas),
    Sw = switch(SwName,Status,Values,Alphas).

get_sw_a(Sw,[Status,Values,Alphas]) :- get_sw_a(Sw,Status,Values,Alphas).

get_sw_a(Sw,Status,Values,Alphas) :-
    $pp_get_hyperparameters(Sw,Values,Alphas,_),
    ( $pd_fixed_hyperparameters(Sw) -> Status = fixed ; Status = unfixed ).

get_sw_a(Sw,Status,Values,Alphas,Es) :-
    $pp_get_hyperparameters(Sw,Values,Alphas,_),
    $pp_get_hyperexpectations(Sw,_,Es),
    ( $pd_fixed_hyperparameters(Sw) -> Status = fixed ; Status = unfixed ).

get_sw_d(Sw) :-
    get_sw_d(SwName,Status,Values,Deltas),
    Sw = switch(SwName,Status,Values,Deltas).

get_sw_d(Sw,[Status,Values,Deltas]) :- get_sw_d(Sw,Status,Values,Deltas).

get_sw_d(Sw,Status,Values,Deltas) :-
    $pp_get_hyperparameters(Sw,Values,_,Deltas),
    ( $pd_fixed_hyperparameters(Sw) -> Status = fixed ; Status = unfixed ).

get_sw_d(Sw,Status,Values,Deltas,Es) :-
    $pp_get_hyperparameters(Sw,Values,_,Deltas),
    $pp_get_expectations(Sw,_,Es),
    ( $pd_fixed_hyperparameters(Sw) -> Status = fixed ; Status = unfixed ).

%% aliases

get_sw_h(Sw)              :- get_sw_d(Sw).
get_sw_h(Sw,Info)         :- get_sw_d(Sw,Info).
get_sw_h(Sw,Status,Vs,Ds) :- get_sw_d(Sw,Status,Vs,Ds).

%%% get switch information including both parameters and hyperparameters

get_sw_pa(Sw) :-
    get_sw_pa(SwName,StatusPair,Values,Probs,Alphas),
    Sw = switch(SwName,StatusPair,Values,Probs,Alphas).

get_sw_pa(Sw,[StatusPair,Values,Probs,Alphas]) :-
    get_sw_pa(Sw,StatusPair,Values,Probs,Alphas).

get_sw_pa(Sw,[StatusP,StatusH],Values,Probs,Alphas) :-
    $pp_get_parameters(Sw,Values,Probs),
    $pp_get_hyperparameters(Sw,_,Alphas,_),
    ( $pd_fixed_parameters(Sw)      -> StatusP = fixed ; StatusP = unfixed ),
    ( $pd_fixed_hyperparameters(Sw) -> StatusH = fixed ; StatusH = unfixed ).

get_sw_pa(Sw,[StatusP,StatusH],Values,Probs,Alphas,Es) :-
    $pp_get_parameters(Sw,Values,Probs),
    $pp_get_hyperparameters(Sw,_,Alphas,_),
    $pp_get_hyperexpectations(Sw,_,Es),
    ( $pd_fixed_parameters(Sw)      -> StatusP = fixed ; StatusP = unfixed ),
    ( $pd_fixed_hyperparameters(Sw) -> StatusH = fixed ; StatusH = unfixed ).

get_sw_pd(Sw) :-
    get_sw_pd(SwName,StatusPair,Values,Probs,Deltas),
    Sw = switch(SwName,StatusPair,Values,Probs,Deltas).

get_sw_pd(Sw,[StatusPair,Values,Probs,Deltas]) :-
    get_sw_pd(Sw,StatusPair,Values,Probs,Deltas).

get_sw_pd(Sw,[StatusP,StatusH],Values,Probs,Deltas) :-
    $pp_get_parameters(Sw,Values,Probs),
    $pp_get_hyperparameters(Sw,_,_,Deltas),
    ( $pd_fixed_parameters(Sw)      -> StatusP = fixed ; StatusP = unfixed ),
    ( $pd_fixed_hyperparameters(Sw) -> StatusH = fixed ; StatusH = unfixed ).

get_sw_pd(Sw,[StatusP,StatusH],Values,Probs,Deltas,Es) :-
    $pp_get_parameters(Sw,Values,Probs),
    $pp_get_hyperparameters(Sw,_,_,Deltas),
    $pp_get_expectations(Sw,_,Es),
    ( $pd_fixed_parameters(Sw)      -> StatusP = fixed ; StatusP = unfixed ),
    ( $pd_fixed_hyperparameters(Sw) -> StatusH = fixed ; StatusH = unfixed ).

%% aliases

get_sw_b(Sw)                   :- get_sw_pd(Sw).
get_sw_b(Sw,Info)              :- get_sw_pd(Sw,Info).
get_sw_b(Sw,StatusPH,Vs,Ps,Ds) :- get_sw_pd(Sw,StatusPH,Vs,Ps,Ds).

%%%% save hyperparameters

save_sw_a :- save_sw_a('Saved_SW_A').

save_sw_a(File) :-
    open(File,write,Stream),
    $pp_call_with_stream(Stream,$pp_save_sw_a_core(Stream)),!.

$pp_save_sw_a_core(Stream) :-
    ( get_sw_a(SwName,Status,Values,Alphas),
      format(Stream,"switch(~q,~q,~q,",[SwName,Status,Values]),
      $pp_write_hyperparameters(Stream,Alphas,'['),
      format(Stream,"]).~n",[]),
      fail
    ; true
    ).

save_sw_d :- save_sw_d('Saved_SW_D').

save_sw_d(File) :-
    open(File,write,Stream),
    $pp_call_with_stream(Stream,$pp_save_sw_d_core(Stream)),!.

$pp_save_sw_d_core(Stream) :-
    ( get_sw_d(SwName,Status,Values,Deltas),
      format(Stream,"switch(~q,~q,~q,",[SwName,Status,Values]),
      $pp_write_hyperparameters(Stream,Deltas,'['),
      format(Stream,"]).~n",[]),
      fail
    ; true
    ).

$pp_write_hyperparameters(_,[],_).
$pp_write_hyperparameters(Stream,[H|Hs],C) :-
    format(Stream,"~w~15e",[C,H]),!,
    $pp_write_hyperparameters(Stream,Hs,',').

%% aliases

save_sw_h       :- save_sw_d.
save_sw_h(File) :- save_sw_d(File).

%%%% restore hyperparameters

restore_sw_a :- restore_sw_a('Saved_SW_A').

restore_sw_a(File) :-
    open(File,read,Stream),
    $pp_call_with_stream(Stream,$pp_restore_sw_a_core(Stream)),!.

$pp_restore_sw_a_core(Stream) :-
    repeat,
    read(Stream,Switch),
    ( Switch == end_of_file
    ; $pp_require_saved_switch(Switch,$msg(0111),restore_sw_a/1),
      Switch = switch(ID,_,_,Alphas),
      set_sw_a(ID,Alphas),
      fail
    ).

restore_sw_d :- restore_sw_d('Saved_SW_D').

restore_sw_d(File) :-
    open(File,read,Stream),
    $pp_call_with_stream(Stream,$pp_restore_sw_d_core(Stream)),!.

$pp_restore_sw_d_core(Stream) :-
    repeat,
    read(Stream,Switch),
    ( Switch == end_of_file
    ; $pp_require_saved_switch(Switch,$msg(0111),restore_sw_d/1),
      Switch = switch(ID,_,_,Deltas),
      set_sw_d(ID,Deltas),
      fail
    ).

%% aliases

restore_sw_h       :- restore_sw_d.
restore_sw_h(File) :- restore_sw_d(File).

%%%% save both parameters and hyperparameters

save_sw_pa :- save_sw, save_sw_a.

save_sw_pa(FileP,FileA) :-
    save_sw(FileP),
    save_sw_a(FileA),!.

save_sw_pd :- save_sw, save_sw_d.

save_sw_pd(FileP,FileD) :-
    save_sw(FileP),
    save_sw_d(FileD),!.

%% aliases

save_sw_b              :- save_sw_pd.
save_sw_b(FileP,FileD) :- save_sw_pd(FileP,FileD).

%%%% restore both parameters and hyperparameters

restore_sw_pa :- restore_sw, restore_sw_a.

restore_sw_pa(FileP,FileA) :-
    restore_sw(FileP),
    restore_sw_a(FileA),!.

restore_sw_pd :- restore_sw, restore_sw_d.

restore_sw_pd(FileP,FileD) :-
    restore_sw(FileP),
    restore_sw_d(FileD),!.

%% aliases

restore_sw_b              :- restore_sw_pd.
restore_sw_b(FileP,FileD) :- restore_sw_pd(FileP,FileD).

%%----------------------------------------
%%  [Note]
%%  $pp_get_{values,parameters,expectations,hyperparameters}/2 do not check
%%  the groundness of switch names.

% raises a exception when there are no msw declarations
% (and can be a replacement of values/2 called in the clause bodies)
get_values(Sw,Values) :-
    $pp_require_msw_declaration($msg(0100),get_values0/2),
    $pp_get_values(Sw,Values).

% provides a simple access to value declarations
get_values0(Sw,Values) :-
    current_predicate($pu_values/2),
    $pp_get_values(Sw,Values).

% deterministically behaves and raises a exception also when there is no msw
% declaration that matches with Sw
get_values1(Sw,Values) :-
    $pp_require_ground(Sw,$msg(0101),get_values1/2),
    $pp_require_switch_outcomes(Sw,$msg(0102),get_values1/2),
    $pp_get_values(Sw,Values),!.

% $pu_values/2 = translated values declarations
$pp_get_values(Sw,Values) :- $pu_values(Sw,Values).

%%----------------------------------------
%% Wrappers to the switch database

$pp_get_parameters(Sw,Values,Probs) :-
     ( ground(Sw) ->
         get_values1(Sw,Values),
         ( $pd_parameters(Sw,Values0,Probs0) ->
             ( Values0 = Values -> Probs = Probs0
             ; $pd_fixed_parameters(Sw) ->
                 $pp_raise_runtime_error($msg(0106),[Sw],
                                         modified_switch_outcomes,
                                         $pp_get_parameters/3)
             ; get_prism_flag(clean_table,off) ->
                   $pp_raise_runtime_error($msg(0112),[Sw],
                                           modified_switch_outcomes,
                                           $pp_get_parameters/3)
             ; set_sw(Sw,default),!,
               $pd_parameters(Sw,Values,Probs)
             )
         ; set_sw(Sw,default),!,
           $pd_parameters(Sw,Values,Probs)
         )
     ; $pd_parameters(Sw,Values,Probs)
         % if Sw is not ground, we do not assign the default distribution
     ).

% [Note] set_sw_a(Sw,default) and set_sw_d(Sw,default) behaves in the same way
$pp_get_hyperparameters(Sw,Values,Alphas,Deltas) :-
     ( ground(Sw) ->
         get_values1(Sw,Values),
         ( $pd_hyperparameters(Sw,Values0,Alphas0,Deltas0) ->
             ( Values0 = Values ->
                 Alphas = Alphas0,
                 Deltas = Deltas0
             ; $pd_fixed_hyperparameters(Sw) ->
                   $pp_raise_runtime_error($msg(0108),[Sw],
                                           modified_switch_outcomes,
                                           $pp_get_hyperparameters/4)
             ; get_prism_flag(clean_table,off) ->
                   $pp_raise_runtime_error($msg(0112),[Sw],
                                           modified_switch_outcomes,
                                           $pp_get_hyperparameters/4)
             ; set_sw_a(Sw,default),!,
               $pd_hyperparameters(Sw,Values,Alphas,Deltas)
             )
         ; set_sw_a(Sw,default),!,
           $pd_hyperparameters(Sw,Values,Alphas,Deltas)
         )
     ; $pd_hyperparameters(Sw,Values,Alphas,Deltas)
     ).

$pp_get_expectations(Sw,Values,Es) :-
     ( ground(Sw) ->
         get_values1(Sw,Values),
         $pd_expectations(Sw,Values0,Es0),
         ( Values0 = Values -> Es = Es0
         ; $pp_raise_runtime_error($msg(0107),[Sw],modified_switch_outcomes,
                                   $pp_get_expectations/3)
         )
     ; $pd_expectations(Sw,Values,Es)
     ).

$pp_get_hyperexpectations(Sw,Values,Es) :-
     ( ground(Sw) ->
         get_values1(Sw,Values),
         $pd_hyperexpectations(Sw,Values0,Es0),
         ( Values0 = Values -> Es = Es0
         ; $pp_raise_runtime_error($msg(0107),[Sw],modified_switch_outcomes,
                                   $pp_get_hyperexpectations/3)
         )
     ; $pd_hyperexpectations(Sw,Values,Es)
     ).

%%----------------------------------------

$pp_registered_sw(Sw) :-   % ground switch name will be returned
     ( $pd_parameters(Sw,_,_)
     ; $pd_hyperparameters(Sw,_,_,_)
     ).

show_reg_sw :-
     get_reg_sw_list(Sws),
     $pp_show_reg_sw(Sws).

$pp_show_reg_sw(Sws) :-
     format("Registered random switches:~n",[]),
     $pp_show_reg_sw1(Sws).

$pp_show_reg_sw1([]).
$pp_show_reg_sw1([Sw|Sws]) :-
     format("  ~w~n",[Sw]),!,
     $pp_show_reg_sw1(Sws).

get_reg_sw(Sw) :-
     get_reg_sw_list(Sws),!,
     member(Sw,Sws).

get_reg_sw_list(Sws) :-
     findall(Sw,$pp_registered_sw(Sw),Sws0),
     sort(Sws0,Sws).

%%----------------------------------------

alpha_to_delta(Alphas,Deltas) :-
    (get_prism_flag(crf_enable,on) -> $pp_require_numbers(Alphas,$msg(0208),alpha_to_delta/2)
      ;$pp_require_non_negative_numbers(Alphas,$msg(0208),alpha_to_delta/2)),
    $pp_alpha_to_delta(Alphas,Deltas).

$pp_alpha_to_delta([],[]).
$pp_alpha_to_delta([Alpha|Alphas],[Delta|Deltas]) :-
     Delta is Alpha - 1,!,
     $pp_alpha_to_delta(Alphas,Deltas).

delta_to_alpha(Deltas,Alphas) :-
    (get_prism_flag(crf_enable,on) -> $pp_require_numbers(Delta,$msg(0208),delta_to_alpha/2)
      ;$pp_require_non_negative_numbers(Deltas,$msg(0209),delta_to_alpha/2)),
    $pp_delta_to_alpha(Deltas,Alphas).

$pp_delta_to_alpha([],[]).
$pp_delta_to_alpha([Delta|Deltas],[Alpha|Alphas]) :-
     Alpha is Delta + 1,!,
     $pp_delta_to_alpha(Deltas,Alphas).
learn :-
    get_prism_flag(learn_mode,Mode0),
    $pp_conv_learn_mode(Mode0,Mode,VT),
    ( VT = 0 -> $pp_learn_main(Mode)
    ; VT = 1 -> $pp_vlearn_main(Mode)
    ).
learn(Goals) :-
    get_prism_flag(learn_mode,Mode0),
    $pp_conv_learn_mode(Mode0,Mode,VT),
    ( VT = 0 -> $pp_learn_main(Mode,Goals)
    ; VT = 1 -> $pp_vlearn_main(Mode,Goals)
    ).

learn_p :-
    $pp_learn_main(ml).
learn_p(Goals) :-
    $pp_learn_main(ml,Goals).
learn_h :-
    $pp_learn_main(vb).
learn_h(Goals) :-
    $pp_learn_main(vb,Goals).
learn_b :-
    $pp_learn_main(both).
learn_b(Goals) :-
    $pp_learn_main(both,Goals).

%% for the parallel version
$pp_learn_main(Mode) :- call($pp_learn_core(Mode)).
$pp_learn_main(Mode,Goals) :- call($pp_learn_core(Mode,Goals)).

$pp_learn_data_file(FileName) :-
    get_prism_flag(data_source,Source),
    ( Source == none ->
        $pp_raise_runtime_error($msg(1300),data_source_not_found,
                                $pp_learn_data_file/1)
    ; Source == data/1 ->
      ( current_predicate(data/1) -> data(FileName)
      ; $pp_raise_runtime_error($msg(1301),data_source_not_found,
                                $pp_learn_data_file/1)
      )
    ; Source = file(FileName)
    ; $pp_raise_unmatched_branches($pp_learn_data_file/1)
    ),!.

$pp_learn_check_goals(Goals) :-
    $pp_require_observed_data(Goals,$msg(1302),$pp_learn_core/1),
    $pp_learn_check_goals1(Goals),
    ( get_prism_flag(daem,on),
      membchk(failure,Goals)
        -> $pp_raise_runtime_error($msg(1305),daem_with_failure,
                                   $pp_learn_core/1)
    ; true
    ).

$pp_learn_check_goals1([]).
$pp_learn_check_goals1([G0|Gs]) :-
    ( (G0 = goal(G,Count) ; G0 = count(G,Count) ; G0 = (Count times G) ) ->
        $pp_require_positive_integer(Count,$msg(1306),$pp_learn_core/1)
    ; G = G0
    ),
    $pp_require_tabled_probabilistic_atom(G,$msg(1303),$pp_learn_core/1),!,
    $pp_learn_check_goals1(Gs).

$pp_learn_core(Mode) :-
    $pp_learn_data_file(FileName),
    load_clauses(FileName,Goals,[]),!,
    $pp_learn_core(Mode,Goals).

$pp_learn_core(Mode,Goals) :-
    $pp_learn_check_goals(Goals),
    $pp_learn_message(MsgS,MsgE,MsgT,MsgM),
    $pc_set_em_message(MsgE),
    cputime(Start),
    $pp_clean_learn_info,
    $pp_learn_reset_hparams(Mode),
    $pp_trans_goals(Goals,GoalCountPairs,AllGoals),!,
    global_set($pg_observed_facts,GoalCountPairs),
    cputime(StartExpl),
    global_set($pg_num_goals,0),
    $pp_find_explanations(AllGoals),!,
    $pp_print_num_goals(MsgS),
    cputime(EndExpl),
    statistics(table,[TableSpace,_]),
    $pp_format_if(MsgM,"Exporting switch information to the EM routine ... "),
    flush_output,
    $pp_export_sw_info,
    $pp_format_if(MsgM,"done~n"),
    $pp_observed_facts(GoalCountPairs,GidCountPairs,
                       0,Len,0,NGoals,-1,FailRootIndex),
    $pc_prism_prepare(GidCountPairs,Len,NGoals,FailRootIndex),
    cputime(StartEM),
    $pp_em(Mode,Output),
    cputime(EndEM),
    $pc_import_occ_switches(NewSws,NSwitches,NSwVals),
    $pp_decode_update_switches(Mode,NewSws),
    $pc_import_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    cputime(End),
    $pp_assert_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    $pp_assert_learn_stats(Mode,Output,NSwitches,NSwVals,TableSpace,
                           Start,End,StartExpl,EndExpl,StartEM,EndEM,1000),
    $pp_print_learn_stats_message(MsgT),
    $pp_print_learn_end_message(MsgM,Mode),!.

$pp_clean_learn_info :-
    $pp_clean_dummy_goal_table,
    $pp_clean_graph_stats,
    $pp_clean_learn_stats,
    $pp_init_tables_aux,
    $pp_init_tables_if_necessary,!.

$pp_conv_learn_mode(Mode0,Mode,VT) :-
    % keep backward compatibility
    ( Mode0 = params  -> Mode1 = ml
    ; Mode0 = hparams -> Mode1 = vb
    ; Mode1 = Mode0
    ),
    ( Mode1 = ml      -> Mode = ml,   VT = 0
    ; Mode1 = vb      -> Mode = vb,   VT = 0
    ; Mode1 = both    -> Mode = both, VT = 0
    ; Mode1 = ml_vt   -> Mode = ml,   VT = 1
    ; Mode1 = vb_vt   -> Mode = vb,   VT = 1
    ; Mode1 = both_vt -> Mode = both, VT = 1
    ).

$pp_learn_reset_hparams(Mode) :-
    ( Mode == ml -> true
    ; get_prism_flag(reset_hparams,on) -> set_sw_all_a(_)
    ; true
    ).

$pp_print_num_goals(MsgS) :-
    ( MsgS == 0 -> true
    ; global_get($pg_num_goals,N),format("(~w)~n",[N]),flush_output
    ).

$pp_em(ml,Output) :-
    $pc_prism_em(Iterate,LogPost,LogLike,BIC,CS,ModeSmooth),
    Output = [Iterate,LogPost,LogLike,BIC,CS,ModeSmooth].
$pp_em(vb,Output) :-
    $pc_prism_vbem(IterateVB,FreeEnergy),
    Output = [IterateVB,FreeEnergy].
$pp_em(both,Output) :-
    $pc_prism_both_em(IterateVB,FreeEnergy),
    Output = [IterateVB,FreeEnergy].
    
$pp_assert_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared) :-
    NNodes is NGoalNodes + NSwNodes,
    assertz($ps_num_subgraphs(NSubgraphs)),
    assertz($ps_num_nodes(NNodes)),
    assertz($ps_num_goal_nodes(NGoalNodes)),
    assertz($ps_num_switch_nodes(NSwNodes)),
    assertz($ps_avg_shared(AvgShared)),!.

$pp_assert_learn_stats(Mode,Output,NSwitches,NSwVals,TableSpace,
                       Start,End,StartExpl,EndExpl,StartEM,EndEM,UnitsPerSec) :-
    assertz($ps_num_switches(NSwitches)),
    assertz($ps_num_switch_values(NSwVals)),
    ( integer(TableSpace) -> assertz($ps_learn_table_space(TableSpace)) ; true ),
    Time is (End - Start) / UnitsPerSec,
    assertz($ps_learn_time(Time)),
    TimeExpl is (EndExpl - StartExpl) / UnitsPerSec,
    assertz($ps_learn_search_time(TimeExpl)),
    TimeEM is (EndEM - StartEM) / UnitsPerSec,
    assertz($ps_em_time(TimeEM)),
    $pp_assert_learn_stats_aux(Mode,Output),!.

$pp_assert_learn_stats_aux(ml,Output) :-
    Output = [Iterate,LogPost,LogLike,BIC,CS,ModeSmooth],
    assertz($ps_num_iterations(Iterate)),
    ( ModeSmooth > 0 -> assertz($ps_log_post(LogPost)) ; true ),
    assertz($ps_log_likelihood(LogLike)),
    assertz($ps_bic_score(BIC)),
    ( ModeSmooth > 0 -> assertz($ps_cs_score(CS)) ; true ),!.

$pp_assert_learn_stats_aux(vb,Output) :-
    Output = [IterateVB,FreeEnergy],
    assertz($ps_num_iterations_vb(IterateVB)),
    assertz($ps_free_energy(FreeEnergy)),!.

$pp_assert_learn_stats_aux(both,Output) :-
    Output = [IterateVB,FreeEnergy],
    assertz($ps_num_iterations_vb(IterateVB)),
    assertz($ps_free_energy(FreeEnergy)),!.

$pp_print_learn_stats_message(Msg) :-
    ( Msg == 0 -> true
    ; format("Statistics on learning:~n",[]),
      ( $pp_print_learn_stats_message_aux,fail ; true )
    ),!.

$pp_print_learn_stats_message_aux :-
    ( $ps_num_nodes(L),
        format("~tGraph size: ~w~n",[L])
    ; $ps_num_switches(L),
        format("~tNumber of switches: ~w~n",[L])
    ; $ps_num_switch_values(L),
        format("~tNumber of switch instances: ~w~n",[L])
    ; $ps_num_iterations_vb(L),
        format("~tNumber of iterations: ~w~n",[L])
    ; $ps_num_iterations(L),
        format("~tNumber of iterations: ~w~n",[L])
    ; $ps_free_energy(L),
        format("~tFinal variational free energy: ~9f~n",[L])
    ; $ps_log_post(L),
        format("~tFinal log of a posteriori prob: ~9f~n",[L])
    ; $ps_log_likelihood(L), \+ $ps_log_post(_),
        format("~tFinal log likelihood: ~9f~n",[L])
    ; $ps_learn_time(L),
        format("~tTotal learning time: ~3f seconds~n",[L])
    ; $ps_learn_search_time(L),
        format("~tExplanation search time: ~3f seconds~n",[L])
    ; $ps_learn_table_space(L),
        format("~tTotal table space used: ~w bytes~n",[L])
    ).

$pp_print_learn_end_message(Msg,Mode) :-
    ( Msg == 0 -> true
    ; Mode == ml ->
        format("Type show_sw to show the probability distributions.~n",[])
    ; Mode == vb ->
        format("Type show_sw_a/show_sw_d to show the probability distributions.~n",[])
    ; Mode == both ->
        format("Type show_sw_pa/show_sw_pd to show the probability distributions.~n",[])
    ).

$pp_clean_graph_stats :-
    retractall($ps_num_subgraphs(_)),
    retractall($ps_num_nodes(_)),
    retractall($ps_num_goal_nodes(_)),
    retractall($ps_num_switch_nodes(_)),
    retractall($ps_avg_shared(_)),!.

$pp_clean_learn_stats :-
    retractall($ps_log_likelihood(_)),
    retractall($ps_log_post(_)),
    retractall($ps_num_switches(_)),
    retractall($ps_num_switch_values(_)),
    retractall($ps_num_iterations(_)),
    retractall($ps_num_iterations_vb(_)),
    retractall($ps_bic_score(_)),
    retractall($ps_cs_score(_)),
    retractall($ps_free_energy(_)),
    retractall($ps_learn_time(_)),
    retractall($ps_learn_search_time(_)),
    retractall($ps_em_time(_)),
    retractall($ps_learn_table_space(_)),!.

%% $pp_collect_sw_info/{1,3} are kept for backward compatibility

$pp_collect_sw_info(Sws) :-
    $pc_prism_sw_count(N),
    $pp_collect_sw_info(0,N,Sws).

$pp_collect_sw_info(Sid,N,[]) :- Sid >= N,!.
$pp_collect_sw_info(Sid,N,SwInsList) :-
    $pc_prism_sw_term(Sid,Sw),
    $pp_get_parameters(Sw,Values,Pbs),
    $pp_get_hyperparameters(Sw,Values,_,Deltas),
    ( $pd_fixed_parameters(Sw)      -> FixedP = 1 ; FixedP = 0 ),
    ( $pd_fixed_hyperparameters(Sw) -> FixedH = 1 ; FixedH = 0 ),
    SwInsList = [sw(Sid,Iids,Pbs,Deltas,FixedP,FixedH)|SwInsList1],!,
    $pp_collect_sw_ins_ids(Sw,Values,Iids),
    Sid1 is Sid + 1,!,
    $pp_collect_sw_info(Sid1,N,SwInsList1).

$pp_export_sw_info :-
    $pc_prism_sw_count(N),
    $pp_export_sw_info(0,N).

$pp_export_sw_info(Sid,N) :- Sid >= N,!.
$pp_export_sw_info(Sid,N) :-
    $pc_prism_sw_term_nocopy(Sid,Sw),
    $pp_get_parameters(Sw,Values,Pbs),
    $pp_get_hyperparameters(Sw,Values,_,Deltas),
    ( $pd_fixed_parameters(Sw)      -> FixedP = 1 ; FixedP = 0 ),
    ( $pd_fixed_hyperparameters(Sw) -> FixedH = 1 ; FixedH = 0 ),
    SwIns = sw(Sid,Iids,Pbs,Deltas,FixedP,FixedH),
    $pp_collect_sw_ins_ids(Sw,Values,Iids),
    $pc_export_sw_info(SwIns),!,
    ( Sid mod 1000 is 999 -> garbage_collect ; true ),  % workarond for performance degradation
    Sid1 is Sid + 1,!,
    $pp_export_sw_info(Sid1,N).

$pp_collect_sw_ins_ids(_Sw,[],[]).
$pp_collect_sw_ins_ids(Sw,[V|Vs],[I|Is]) :-
    $pc_prism_sw_ins_id_get(msw(Sw,V),I),!,
    $pp_collect_sw_ins_ids(Sw,Vs,Is).

$pp_decode_update_switches(ml,Sws) :-
    $pp_decode_update_switches_p(Sws).
$pp_decode_update_switches(vb,Sws) :-
    $pp_decode_update_switches_h(Sws).
$pp_decode_update_switches(both,Sws) :-
    $pp_decode_update_switches_b(Sws).

$pp_decode_update_switches_p([]).
$pp_decode_update_switches_p([sw(_,SwInstances)|Sws]) :-
    $pp_decode_switch_name(SwInstances,Sw),
    $pp_decode_switch_instances(SwInstances,Updates),
    get_values1(Sw,Values),
    $pp_separate_updates(Values,Probs,_Deltas,Es,Updates),
    ( retract($pd_parameters(Sw,_,_)) -> true ; true ),
    assert($pd_parameters(Sw,Values,Probs)),
    ( retract($pd_expectations(Sw,_,_)) -> true ; true),
    ( retract($pd_hyperexpectations(Sw,_,_)) -> true ; true),
    assert($pd_expectations(Sw,Values,Es)),!,
    $pp_decode_update_switches_p(Sws).

$pp_decode_update_switches_h([]).
$pp_decode_update_switches_h([sw(_,SwInstances)|Sws]) :-
    $pp_decode_switch_name(SwInstances,Sw),
    $pp_decode_switch_instances(SwInstances,Updates),
    get_values1(Sw,Values),
    $pp_separate_updates(Values,_Probs,Deltas,Es,Updates),
    ( retract($pd_hyperparameters(Sw,_,_,_)) -> true ; true ),
    $pp_delta_to_alpha(Deltas,Alphas),
    assert($pd_hyperparameters(Sw,Values,Alphas,Deltas)),
    ( retract($pd_expectations(Sw,_,_)) -> true ; true),
    ( retract($pd_hyperexpectations(Sw,_,_)) -> true ; true),
    assert($pd_hyperexpectations(Sw,Values,Es)),!,
    $pp_decode_update_switches_h(Sws).

$pp_decode_update_switches_b([]).
$pp_decode_update_switches_b([sw(_,SwInstances)|Sws]) :-
    $pp_decode_switch_name(SwInstances,Sw),
    $pp_decode_switch_instances(SwInstances,Updates),
    get_values1(Sw,Values),
    $pp_separate_updates(Values,Probs,Deltas,Es,Updates),
    ( retract($pd_parameters(Sw,_,_)) -> true ; true ),
    assert($pd_parameters(Sw,Values,Probs)),
    ( retract($pd_hyperparameters(Sw,_,_,_)) -> true ; true ),
    $pp_delta_to_alpha(Deltas,Alphas),
    assert($pd_hyperparameters(Sw,Values,Alphas,Deltas)),
    ( retract($pd_hyperexpectations(Sw,_,_)) -> true ; true),
    ( retract($pd_expectations(Sw,_,_)) -> true ; true),
    assert($pd_hyperexpectations(Sw,Values,Es)),!,
    $pp_decode_update_switches_b(Sws).

$pp_decode_switch_name([sw_ins(Sid,_,_,_)|_SwInstances],Sw) :-
    $pc_prism_sw_ins_term(Sid,msw(Sw,_)).  % only uses the first element

$pp_decode_switch_instances([],[]).
$pp_decode_switch_instances([sw_ins(Sid,Prob,Delta,Expect)|SwInstances],
                            [(V,Prob,Delta,Expect)|Updates]) :-
    $pc_prism_sw_ins_term(Sid,msw(_,V)),!,
    $pp_decode_switch_instances(SwInstances,Updates).

$pp_separate_updates([],[],[],[],_Updates).
$pp_separate_updates([V|Vs],[Prob|Probs],[Delta|Deltas],[E|Es],Updates) :-
    member((V,Prob,Delta,E),Updates),!,
    $pp_separate_updates(Vs,Probs,Deltas,Es,Updates).

%% [NOTE] Non-ground goals has already been replaced by dummy goals, so all
%%        goals are ground here.

$pp_observed_facts([],[],Len,Len,NGoals,NGoals,FailRootIndex,FailRootIndex).
$pp_observed_facts([goal(Goal,Count)|GoalCountPairs],GidCountPairs,
                   Len0,Len,NGoals0,NGoals,FailRootIndex0,FailRootIndex) :-
    % fails if the goal is ground but has no proof
    ( $pc_prism_goal_id_get(Goal,Gid) ->
        ( Goal == failure ->
            NGoals1 = NGoals0,
            FailRootIndex1 = Len0
        ; NGoals1 is NGoals0 + Count,
          FailRootIndex1 = FailRootIndex0
        ),
        GidCountPairs = [goal(Gid,Count)|GidCountPairs1],
        Len1 is Len0 + 1
    ; $pp_raise_unexpected_failure($pp_observed_facts/8)
    ),!,
    $pp_observed_facts(GoalCountPairs,GidCountPairs1,
                       Len1,Len,NGoals1,NGoals,FailRootIndex1,FailRootIndex).

%% Assumption: for any pair of terms F and F' (F's variant), hash codes for
%% F and F' are equal.
%%
%% For convenience on implementation of parallel learning, $pp_trans_goals/3
%% is (internally) split into two predicates $pp_build_count_pairs/2 and
%% $pp_trans_count_pairs/3.
%%
%% The order of goal-count pairs may differ at every run due to the way of
%% implemention of hashtables.

$pp_trans_goals(Goals,GoalCountPairs,AllGoals) :-
    $pp_build_count_pairs(Goals,Pairs),
    $pp_trans_count_pairs(Pairs,GoalCountPairs,AllGoals).

$pp_build_count_pairs(Goals,Pairs) :-
    new_hashtable(Table),
    $pp_count_goals(Goals,Table),
    hashtable_to_list(Table,Pairs0),
    sort(Pairs0,Pairs).

$pp_count_goals([],_).
$pp_count_goals([G0|Goals],Table) :-
    ( G0 = goal(Goal,Count)  -> true
    ; G0 = count(Goal,Count) -> true
    ; G0 = (Count times Goal) -> true
    ; Goal = G0, Count = 1
    ),
    ( ground(Goal) -> GoalCp = Goal
    ; copy_term(Goal,GoalCp)
    ),
    ( $pp_hashtable_get(Table,GoalCp,Count0) ->
        Count1 is Count0 + Count,
        $pp_hashtable_put(Table,GoalCp,Count1)
    ; $pp_hashtable_put(Table,GoalCp,Count)
    ),!,
    $pp_count_goals(Goals,Table).

$pp_trans_count_pairs([],[],[]).
$pp_trans_count_pairs([Goal=Count|Pairs],GoalCountPairs,AllGoals) :-
    $pp_build_dummy_goal(Goal,DummyGoal),
    GoalCountPairs = [goal(DummyGoal,Count)|GoalCountPairs1],
    AllGoals = [DummyGoal|AllGoals1],!,
    $pp_trans_count_pairs(Pairs,GoalCountPairs1,AllGoals1).

$pp_build_dummy_goal(Goal,DummyGoal) :-
    ( Goal = msw(I,V) ->
        ( ground(I) -> I = ICp ; copy_term(I,ICp) ),
        ( ground(V) -> V = VCp ; copy_term(V,VCp) ),
        $pp_create_dummy_goal(DummyGoal),
        $pp_assert_dummy_goal(DummyGoal,Goal),
        Clause = (DummyGoal :- $prism_expl_msw(ICp,VCp,Sid),
                               $pc_prism_goal_id_register(DummyGoal,Hid),
                               $prism_eg_path(Hid,[],[Sid])),
        Prog = [pred(DummyGoal,0,_,_,tabled(_,_,_,_),[Clause]),
                pred($damon_load,0,_,_,_,[($damon_load:-true)])],
        $pp_consult_preds_cond([],Prog)
    ; ground(Goal) ->
        DummyGoal = Goal        % don't create dummy goals (wrappers) for
    ;                           % ground goals to save memory.
        $pp_create_dummy_goal(DummyGoal),
        $pp_assert_dummy_goal(DummyGoal,Goal),
        ( $pp_trans_one_goal(Goal,CompGoal) -> BodyGoal = CompGoal
        ; BodyGoal = (savecp(CP),Depth=0,
                      $pp_expl_interp_goal(Goal,Depth,CP,[],_,[],_,[],_,[],_))
        ),
        Clause = (DummyGoal:-BodyGoal,
                             $pc_prism_goal_id_register(Goal,GId),
                             $pc_prism_goal_id_register(DummyGoal,HId),
                             $prism_eg_path(HId,[GId],[])),
        Prog = [pred(DummyGoal,0,_Mode,_Delay,tabled(_,_,_,_),[Clause]),
                pred($damon_load,0,_,_,_,[($damon_load:-true)])],
        $pp_consult_preds_cond([],Prog)
    ),!.

$pp_assert_dummy_goal(DummyGoal,OrigGoal) :-
    assertz($pd_dummy_goal_table(DummyGoal,OrigGoal)),!.

$pp_clean_dummy_goal_table :-
    retractall($pd_dummy_goal_table(_,_)).

%%----------------------------------------

% just make a simple check
$pp_require_observed_data(Gs,MsgID,Source) :-
    ( $pp_test_observed_data(Gs) -> true
    ; $pp_raise_on_require([Gs],MsgID,Source,$pp_error_observed_data)
    ).

$pp_test_observed_data(Gs) :-
    nonvar(Gs),
    ( Gs = [failure] -> fail
    ; Gs = [_|_]
    ).

$pp_error_observed_data(Gs,Error) :-
    $pp_error_nonvar(Gs,Error), !.
$pp_error_observed_data(Gs,domain_error(observed_data,Gs)) :-
    ( Gs = [failure] ; Gs \= [_|_] ), !.
vlearn_p :-
    $pp_vlearn_main(ml).
vlearn_p(Goals) :-
    $pp_vlearn_main(ml,Goals).
vlearn_h :-
    $pp_vlearn_main(vb).
vlearn_h(Goals) :-
    $pp_vlearn_main(vb,Goals).
vlearn_b :-
    $pp_vlearn_main(both).
vlearn_b(Goals) :-
    $pp_vlearn_main(both,Goals).

%% for the parallel version
$pp_vlearn_main(Mode) :- call($pp_vlearn_core(Mode)).
$pp_vlearn_main(Mode,Goals) :- call($pp_vlearn_core(Mode,Goals)).

$pp_vlearn_core(Mode) :-
    $pp_learn_data_file(FileName),
    load_clauses(FileName,Goals,[]),!,
    $pp_vlearn_core(Mode,Goals).

$pp_vlearn_core(Mode,Goals) :-
    $pp_vlearn_check_goals(Goals),
    $pp_learn_message(MsgS,MsgE,MsgT,MsgM),
    $pc_set_em_message(MsgE),
    cputime(Start),
    $pp_clean_learn_info,
    $pp_learn_reset_hparams(Mode),
    $pp_trans_goals(Goals,GoalCountPairs,AllGoals),!,
    global_set($pg_observed_facts,GoalCountPairs),
    cputime(StartExpl),
    global_set($pg_num_goals,0),
    $pp_find_explanations(AllGoals),!,
    $pp_print_num_goals(MsgS),
    cputime(EndExpl),
    statistics(table,[TableSpace,_]),
    $pp_format_if(MsgM,"Exporting switch information to the VT routine ... "),
    flush_output,
    $pp_export_sw_info,
    $pp_format_if(MsgM,"done~n"),
    $pp_observed_facts(GoalCountPairs,GidCountPairs,
                       0,Len,0,NGoals,-1,FailRootIndex),
    $pc_prism_prepare(GidCountPairs,Len,NGoals,FailRootIndex),
    cputime(StartVT),
    $pp_vt(Mode,Output),
    cputime(EndVT),
    $pc_import_occ_switches(NewSws,NSwitches,NSwVals),
    $pp_decode_update_switches(Mode,NewSws),
    $pc_import_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    cputime(End),
    $pp_assert_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    $pp_assert_vlearn_stats(Mode,Output,NSwitches,NSwVals,TableSpace,
                            Start,End,StartExpl,EndExpl,StartVT,EndVT,1000),
    $pp_print_learn_stats_message(MsgT),
    $pp_print_learn_end_message(MsgM,Mode),!.

$pp_vlearn_check_goals(Goals) :-
    $pp_require_observed_data(Goals,$msg(1302),$pp_vlearn_core/1),
    $pp_vlearn_check_goals1(Goals).

$pp_vlearn_check_goals1([]).
$pp_vlearn_check_goals1([G0|Gs]) :-
    ( (G0 = goal(G,Count) ; G0 = count(G,Count) ; G0 = (Count times G) ) ->
        $pp_require_positive_integer(Count,$msg(1306),$pp_vlearn_core/1)
    ; G = G0
    ),
    $pp_require_tabled_probabilistic_atom(G,$msg(1303),$pp_vlearn_core/1),!,
    $pp_learn_check_goals1(Gs).

$pp_vt(ml,Output) :-
    $pc_prism_vt(Iterate,LogPost,LogLike,ModeSmooth),
    Output = [Iterate,LogPost,LogLike,ModeSmooth].
$pp_vt(vb,Output) :-
    $pc_prism_vbvt(IterateVB,FreeEnergy),
    Output = [IterateVB,FreeEnergy].
$pp_vt(both,Output) :-
    $pc_prism_both_vt(IterateVB,FreeEnergy),
    Output = [IterateVB,FreeEnergy].

$pp_assert_vlearn_stats(Mode,Output,NSwitches,NSwVals,TableSpace,
                       Start,End,StartExpl,EndExpl,StartVT,EndVT,UnitsPerSec) :-
    assertz($ps_num_switches(NSwitches)),
    assertz($ps_num_switch_values(NSwVals)),
    ( integer(TableSpace) -> assertz($ps_learn_table_space(TableSpace)) ; true ),
    Time is (End - Start) / UnitsPerSec,
    assertz($ps_learn_time(Time)),
    TimeExpl is (EndExpl - StartExpl) / UnitsPerSec,
    assertz($ps_learn_search_time(TimeExpl)),
    TimeVT is (EndVT - StartVT) / UnitsPerSec,
    assertz($ps_em_time(TimeVT)),
    $pp_assert_vlearn_stats_sub(Mode,Output),!.

$pp_assert_vlearn_stats_sub(ml,Output) :-
    Output = [Iterate,LogPost,LogLike,ModeSmooth],
    assertz($ps_num_iterations(Iterate)),
    ( ModeSmooth > 0 -> assertz($ps_log_post(LogPost)) ; true ),
    assertz($ps_log_likelihood(LogLike)),!.

$pp_assert_vlearn_stats_sub(vb,Output) :-
    Output = [IterateVB,FreeEnergy],
    assertz($ps_num_iterations_vb(IterateVB)),
    assertz($ps_free_energy(FreeEnergy)),!.

$pp_assert_vlearn_stats_sub(both,Output) :-
    Output = [IterateVB,FreeEnergy],
    assertz($ps_num_iterations_vb(IterateVB)),
    assertz($ps_free_energy(FreeEnergy)),!.
%-------------[ Task : Metropolis-Hastings sampler ]----------------
% Usage: "mcmc"
%        "mcmc(Gs)"
%        "mcmc(Gs,Option)"

:- dynamic $pd_mcmc_free_energy/1.
:- dynamic $pd_viterbi_ranking/1.   % only supported informally

mcmc(Gs) :- mcmc(Gs,[]).

mcmc(Gs,Opts) :-                    % sample P(E_all|Gs) Max times
    $pp_mcmc_check_goals(Gs,mcmc/2),
    $pp_proc_mcmc_opts(Opts,[EndStep,BurnIn,Skip,Trace0,PostParam0],mcmc/2),
    $pp_clean_mcmc_sample_stats,
    $pp_mcmc_message(_,_,MsgC,MsgT,MsgM),
    $pc_set_mcmc_message(MsgC),
    $pp_format_if(MsgM,"~n<<Running VBEM for the initial state>>~n"),
    $pp_mcmc_vb_learn(both,Gs), % theta* affects init state & sampled value
    $pc_import_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    $pp_mcmc_replace_dummy_goals(Gs,DummyGoals),
    length(Gs,NumG),
    $pp_conv_trace_opts(Trace0,Trace,mcmc/2),
    $pp_conv_postparam_opts(PostParam0,PostParam,mcmc/2),
    $pp_format_if(MsgM,"~n<<Running MCMC sampling>>~n"),
    cputime(Start),
    $pc_mcmc_sample(DummyGoals,NumG,EndStep,BurnIn,Skip,Trace,PostParam),
    cputime(End),
    $pp_assert_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    $pp_assert_mcmc_sample_stats(Start,End,1000),
    $pp_print_mcmc_sample_stats_message(MsgT),!.

%% VB learning for obtaining the initial state
$pp_mcmc_vb_learn(Mode,Goals) :-
    $pp_learn_check_goals(Goals),
    $pp_mcmc_message(MsgS,MsgE,_,MsgT,_),
    $pp_clean_learn_stats,
    $pc_set_em_message(MsgE),
    cputime(Start),
    $pp_clean_dummy_goal_table,
    $pp_clean_graph_stats,
    $pp_init_tables_aux,
    $pp_init_tables_if_necessary,!,
    $pp_trans_goals(Goals,GoalCountPairs,AllGoals),!,
    global_set($pg_observed_facts,GoalCountPairs),
    cputime(StartExpl),
    $pp_print_search_progress(MsgS),
    $pp_find_explanations(AllGoals),!,
    $pp_print_num_goals(MsgS),
    cputime(EndExpl),
    statistics(table,[TableSpace,_]),
    $pp_export_sw_info,
    $pp_observed_facts(GoalCountPairs,GidCountPairs,
                       0,Len,0,NGoals,-1,FailRootIndex),
    $pp_check_failure_in_mcmc(Goals,FailRootIndex,mcmc/2),
    $pc_mcmc_prepare(GidCountPairs,Len,NGoals),
    cputime(StartEM),
    $pp_em(Mode,Output),
    cputime(EndEM),
    $pc_import_switch_stats(NSwitches,NSwVals),
    $pc_import_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    $pp_assert_mcmc_free_energy(Output),
    cputime(End),
    $pp_assert_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    $pp_assert_learn_stats(Mode,Output,NSwitches,NSwVals,TableSpace,
                           Start,End,StartExpl,EndExpl,StartEM,EndEM,1000),
    $pp_print_learn_stats_message(MsgT),!.

$pp_proc_mcmc_opts(Opts,[End,BurnIn,Skip,Trace,PostParam],Source) :-
    % default values
    get_prism_flag(mcmc_e,End0),
    get_prism_flag(mcmc_b,BurnIn0),
    get_prism_flag(mcmc_s,Skip0),
    Trace0     = none,
    PostParam0 = none,
    % parse the user options
    $pp_proc_opts(Opts,$pp_mcmc_option,
                  [End1,BurnIn1,Skip1,Trace1,PostParam1],
                  [End0,BurnIn0,Skip0,Trace0,PostParam0],
                  Source),
    % error check & additional processing:
    ( BurnIn1 > End1 ->
          $pp_raise_domain_error($msg(1501),[BurnIn1],[burn_in,BurnIn1],Source)
    ; true
    ),
    ( Skip1 > (End1 - BurnIn1) ->
          $pp_raise_domain_error($msg(1502),[Skip1],[skip,Skip1],Source)
    ; true
    ),
    End    = End1,
    BurnIn = BurnIn1,
    Skip   = Skip1,
    ( Trace1 = [TraceSw,TraceStep0] ->
        ( get_values0(TraceSw,_) -> 
            ( TraceStep0 = burn_in -> TraceStep = BurnIn
            ; TraceStep0 > End ->
                $pp_raise_domain_error($msg(1503),[TraceStep0],[trace_step,Skip1],Source)
            ; TraceStep = TraceStep0
            )
        ; $pp_raise_runtime_error($msg(1505),[TraceSw],switch_unavailable,Source)
        ),
      Trace = [TraceSw,TraceStep]
    ; Trace = none
    ),
    ( PostParam1 = [PParamSw,PParamStep0] ->
        ( get_values0(TraceSw,_) -> 
            ( PParamStep0 = burn_in -> PParamStep = BurnIn
            ; PParamStep0 > End ->
                $pp_raise_domain_error($msg(1503),[PParamStep0],[post_param_step,Skip1],Source)
            ; PParamStep = PParamStep0
            )
        ; $pp_raise_runtime_error($msg(1505),[PParamSw],switch_unavailable,Source)
        ),
      PostParam = [PParamSw,PParamStep]
    ; PostParam = none
    ),!.
   
% this should be called after VB learning
$pp_conv_trace_opts(Trace0,Trace,Source) :-
    ( Trace0 = [TraceSw,TraceStep] ->
        ( $pc_prism_sw_id_get(TraceSw,TraceSwID)
        ; $pp_raise_runtime_error($msg(1505),[TraceSw],switch_unavailable,Source)
        )
    ; TraceSwID = -1, TraceStep = -1
    ),
    Trace = [TraceSwID,TraceStep],!.

% this should be called after VB learning
$pp_conv_postparam_opts(PostParam0,PostParam,Source) :-
    ( PostParam0 = [PParamSw,PParamStep] ->
        ( $pc_prism_sw_id_get(PParamSw,PParamSwID)
        ; $pp_raise_runtime_error($msg(1505),[PParamSw],switch_unavailable,Source)
        )
    ; PParamSwID = -1, PParamStep = -1
    ),
    PostParam = [PParamSwID,PParamStep],!.

$pp_mcmc_option(end(N),1,N)                    :- integer(N),N>=0.
$pp_mcmc_option(burn_in(N),2,N)                :- integer(N),N>=0.
$pp_mcmc_option(skip(N),3,N)                   :- integer(N),N>=1.
$pp_mcmc_option(trace(Sw),4,[Sw,burn_in])      :- not(integer(Sw)).
$pp_mcmc_option(trace(Sw,N),4,[Sw,N])          :- integer(N),N>=1,not(integer(Sw)).
$pp_mcmc_option(post_param(Sw),5,[Sw,burn_in]) :- not(integer(Sw)).
$pp_mcmc_option(post_param(Sw,N),5,[Sw,N])     :- integer(N),N>=1,not(integer(Sw)).

$pp_assert_mcmc_free_energy(Output) :-
    Output = [_,FE],
    retractall($pd_mcmc_free_energy(_)),
    assert($pd_mcmc_free_energy(FE)),!.


%-------------[ Task : Estimate log-marginal-likelihood ]----------------
% Usage: "marg_mcmc"
%        "marg_mcmc([VFE,EML])"
%        "marg_mcmc_full(Gs)"
%        "marg_mcmc_full(Gs,Opts)"
%        "marg_mcmc_full(Gs,Opts,[VFE,EML])"

marg_mcmc:-
    marg_mcmc([VFE,EML]),
    format("~nFree energy = ~12f~n",[VFE]),
    format("Estimated log-marginal-likelihood = ~12f~n~n",[EML]).

marg_mcmc([VFE,EML]) :-
    $pp_mcmc_check_free_energy(VFE,marg_mcmc/1),
    $pp_clean_mcmc_marg_stats,
    cputime(Start),
    $pc_mcmc_marginal(EML),
    cputime(End),
    $pp_assert_mcmc_marg_stats(Start,End,1000),!.

marg_mcmc_full(Gs) :- marg_mcmc_full(Gs,[]).

marg_mcmc_full(Gs,Opts) :-
    marg_mcmc_full(Gs,Opts,[VFE,EML]),
    format("~nFree energy = ~12f~n",[VFE]),
    format("Estimated log-marginal-likelihood = ~12f~n~n",[EML]).

marg_mcmc_full(Gs,Opts,[VFE,EML]) :-
    $pp_proc_mcmc_opts(Opts,_,marg_mcmc_full/3), % just for checking the format
    $pp_mcmc_check_goals(Gs,marg_mcmc_full/3),
    $pp_clean_mcmc_marg_stats,
    $pp_disable_message(Msg),
    mcmc(Gs,Opts),
    $pp_enable_message(Msg),
    cputime(Start),
    $pp_mcmc_check_free_energy(VFE),
    $pc_mcmc_marginal(EML),
    cputime(End),
    $pp_assert_mcmc_marg_stats(Start,End,1000),!.

$pp_mcmc_check_free_energy(FE) :- $pd_mcmc_free_energy(FE).
$pp_mcmc_check_free_energy(FE,Source) :-
    ( $pd_mcmc_free_energy(FE) -> true
    ; $pp_raise_runtime_error($msg(1506),mcmc_unfinished,Source)
    ).


%-------------[ Task : Average log-marginal-likelihood ]----------------
% Usage: "ave_marg_mcmc(Iterate,Gs)"
%        "ave_marg_mcmc(Iterate,Gs,Opts)"
%        "ave_marg_mcmc(Iterate,Gs,Opts,[AvgEML,StdEML])"
%        "ave_marg_mcmc(Iterate,Gs,Opts,[AvgVFE,StdVFE],[AvgEML,StdEML])"

ave_marg_mcmc(Iterate,Gs) :-
    ave_marg_mcmc(Iterate,Gs,[]).

ave_marg_mcmc(Iterate,Gs,Opts) :-
    ave_marg_mcmc(Iterate,Gs,Opts,[AvgVFE,StdVFE],[AvgEML,StdEML]),
    format("~nIteration: ~d~n",[Iterate]),
    format("VFE: ave = ~12f, std = ~12f~n",[AvgVFE,StdVFE]),
    format("Estimated ML: ave = ~12f, std = ~12f~n~n",[AvgEML,StdEML]).    

ave_marg_mcmc(Iterate,Gs,Opts,[AvgEML,StdEML]) :-
    ave_marg_mcmc(Iterate,Gs,Opts,_,[AvgEML,StdEML]).

% some execution flags are forcedly modified while $pp_ave_marg_mcmc_aux/5 is called
ave_marg_mcmc(Iterate,Gs,Opts,[AvgVFE,StdVFE],[AvgEML,StdEML]) :-
    $pp_require_positive_integer(Iterate,$msg(1507),ave_marg_mcmc/5),
    $pp_proc_mcmc_opts(Opts,_,ave_marg_mcmc/5), % just for checking the format
    $pp_mcmc_check_goals(Gs,ave_marg_mcmc/5),
    $pp_clean_mcmc_marg_stats,
    numlist(1,Iterate,Ks),
    get_prism_flag(clean_table,CleanTable),
    $pp_ave_marg_mcmc_aux(Ks,Gs,Opts,VFEs,EMLs),
    set_prism_flag(clean_table,CleanTable),
    ( Iterate == 1 -> VFEs = [AvgVFE], StdVFE = 0, EMLs = [AvgEML], StdEML = 0
    ; avglist(VFEs,AvgVFE),
      stdlist(VFEs,StdVFE),
      avglist(EMLs,AvgEML),
      stdlist(EMLs,StdEML)
    ),!.

$pp_ave_marg_mcmc_aux([],_,_,[],[]).
$pp_ave_marg_mcmc_aux([K|Ks],Gs,Opts,[VFE|VFEs],[EML|EMLs]) :-
    marg_mcmc_full(Gs,Opts,[VFE,EML]),
    set_prism_flag(clean_table,off),
    format("[~d] VFE = ~12f, EML = ~12f~n",[K,VFE,EML]),!,
    $pp_ave_marg_mcmc_aux(Ks,Gs,Opts,VFEs,EMLs).


%----------------[ Task : Prediction by M-H sampling ]----------------
% Usage: "predict_mcmc(PGs,AnsL)" where Ans = [ViterbiG,ViterbiEG,LogP]
%        "predict_mcmc(M,PGs,AnsL)"
%        "predict_mcmc_full(OGs,PGs,AnsL)"
%        "predict_mcmc_full(OGs,Opts,PGs,AnsL)"
%        "predict_mcmc_full(OGs,Opts,M,PGs,AnsL)"
%
% use "print_predict_ans(AnsL)" to print tree of AnsL

predict_mcmc_full(OGs,PGs,AnsL) :-
    predict_mcmc_full(OGs,[],PGs,AnsL).

predict_mcmc_full(OGs,Opts,PGs,AnsL) :-
    get_prism_flag(rerank,M),
    predict_mcmc_full(OGs,Opts,M,PGs,AnsL).

predict_mcmc_full(OGs,Opts,M,PGs,AnsL) :-
    $pp_disable_message(Msg),
    mcmc(OGs,Opts),
    $pp_enable_message(Msg),
    predict_mcmc(M,PGs,AnsL).

predict_mcmc(PGs,AnsL) :-
    get_prism_flag(rerank,M),
    predict_mcmc(M,PGs,AnsL).

predict_mcmc(M,PGs,AnsL) :-
    $pp_require_positive_integer(M,$msg(1400),predict_mcmc/3),
    $pp_mcmc_check_goals(PGs,predict_mcmc/3),
    $pp_clean_mcmc_pred_stats,
    get_prism_flag(clean_table,CleanTable),
    set_prism_flag(clean_table,off),
    get_prism_flag(viterbi_mode,ViterbiMode),
    set_prism_flag(viterbi_mode,ml), % use ml, do not consider counts
    $pp_disable_message(Msg),
    cputime(Start),
    $pp_predict_mcmc_core(M,PGs,AnsL),
    cputime(End),
    set_prism_flag(clean_table,CleanTable),
    set_prism_flag(viterbi_mode,ViterbiMode),
    $pp_enable_message(Msg),
    $pp_assert_mcmc_pred_stats(Start,End,1000),
    garbage_collect,!.

$pp_predict_mcmc_core(M,Goals,AnsL) :-
    $pp_clean_dummy_goal_table,
    $pp_init_tables_aux,
    $pp_init_tables_if_necessary,!,
    $pp_trans_goals(Goals,GoalCountPairs,AllGoals),!,
    $pp_find_explanations(AllGoals),!,
    $pp_export_sw_info,
    $pp_observed_facts(GoalCountPairs,GidCountPairs,
                       0,Len,0,NGoals,-1,FailRootIndex),
    $pp_check_failure_in_mcmc(Goals,FailRootIndex,predict_mcmc/3),
    $pc_mcmc_prepare(GidCountPairs,Len,NGoals),
    $pp_mcmc_replace_dummy_goals(Goals,DummyGoals),
    $pc_mcmc_predict(DummyGoals,NGoals,M,AnsL0),
    $pp_mcmc_decode_ans(Goals,DummyGoals,AnsL0,AnsL,VRankLs),
    $pp_assert_mcmc_viterbi_ranking(VRankLs),!.

%% FIXME: isn't it better to remove?
$pp_assert_mcmc_viterbi_ranking(VRankLs) :-
    retractall($pd_viterbi_ranking(_)),
    assert($pd_viterbi_ranking(VRankLs)).

$pp_mcmc_replace_dummy_goals([],[]).
$pp_mcmc_replace_dummy_goals([Goal|Goals],[DummyGoal|DummyGoals]) :-
    ( Goal = msw(I,V) ->
        $pd_dummy_goal_table(DummyGoal,msw(I,V))
    ; ground(Goal) ->
        DummyGoal = Goal
    ; % else
        $pd_dummy_goal_table(DummyGoal,Goal)
    ),!,
    $pp_mcmc_replace_dummy_goals(Goals,DummyGoals).

$pp_mcmc_check_goals([],_).
$pp_mcmc_check_goals([Goal|Goals],Source) :-
    $pp_require_tabled_probabilistic_atom(Goal,$msg(0006),Source),!,
    $pp_mcmc_check_goals(Goals,Source).

$pp_mcmc_decode_ans([],[],[],[],[]).
$pp_mcmc_decode_ans([Goal|Goals],
                    [DummyGoal|DummyGoals],
                    [ans(Ans,LogP,VRankL)|AnsL0],
                    [[VG,VNodeL,LogP]|AnsL],[VRankL|VRankLs]) :-
    $pp_build_n_viterbi_path(Ans,VPathL0),
    $pp_replace_dummy_goal(Goal,DummyGoal,VPathL0,VPathL),
    ( ground(Goal) -> member(v_expl(J,Pmax,VNodeL),VPathL),VNodeL = [node(VG,_)|_]
    ; Goal = msw(_,_) ->
        member(v_expl(J,Pmax,VNodeL),VPathL),
        VNodeL = [node(VG,[path([],[SwIns])])|_],
        Goal = SwIns
    ; % else
        member(v_expl(J,Pmax,VNodeL),VPathL),
        VNodeL = [node(VG,[path([Goal1],[])])|_],
        Goal = Goal1
    ),!,
    $pp_mcmc_decode_ans(Goals,DummyGoals,AnsL0,AnsL,VRankLs).


%----------------[ Task : Compute exaxt log marginal likelihood ]----------------
% Usage: "marg_exact(Gs,LML)"
%        "marg_exact(Gs)"

marg_exact(Gs) :-
    marg_exact(Gs,LML),
    format("Exact log-marginal-likelihood = ~f~n",[LML]).

marg_exact(Gs,LML) :-
    $pp_mcmc_check_goals(Gs,marg_exact/2),
    $pp_clean_mcmc_exact_stats,
    cputime(Start),
    $pp_clean_dummy_goal_table,
    $pp_init_tables_aux,
    $pp_init_tables_if_necessary,!,
    $pp_trans_goals(Gs,GoalCountPairs,AllGoals),!,
    $pp_disable_message(Msg),
    $pp_find_explanations(AllGoals),!,
    $pp_export_sw_info,
    $pp_observed_facts(GoalCountPairs,GidCountPairs,
                       0,Len,0,NGoals,-1,FailRootIndex),
    $pp_check_failure_in_mcmc(Gs,FailRootIndex,marg_exact/2),
    $pc_mcmc_prepare(GidCountPairs,Len,NGoals),
    $pp_mcmc_replace_dummy_goals(Gs,DummyGoals),
    $pc_exact_marginal(DummyGoals,NGoals,LML),
    cputime(End),
    $pp_enable_message(Msg),
    $pp_assert_mcmc_exact_stats(Start,End,1000),!.


%-----------------------------------------------------------------------------------
% Miscellaneous routines

$pp_check_failure_in_mcmc(Gs,FailRootIndex,Source) :-
    ( FailRootIndex >= 0 ->
        $pp_raise_runtime_error($msg(1500),[Gs],failure_in_mcmc,Source)
    ; true
    ).

%-----------------------------------------------------------------------------------
% Statistics-related routines

$pp_clean_mcmc_sample_stats :- retractall($ps_mcmc_sample_time(_)).
$pp_clean_mcmc_marg_stats   :- retractall($ps_mcmc_marg_time(_)).
$pp_clean_mcmc_pred_stats   :- retractall($ps_mcmc_pred_time(_)).
$pp_clean_mcmc_exact_stats  :- retractall($ps_mcmc_exact_time(_)).


$pp_assert_mcmc_sample_stats(Start,End,UnitsPerSec) :-
    Time is (End - Start) / UnitsPerSec,
    assertz($ps_mcmc_sample_time(Time)).

$pp_assert_mcmc_marg_stats(Start,End,UnitsPerSec) :-
    Time is (End - Start) / UnitsPerSec,
    assertz($ps_mcmc_marg_time(Time)).

$pp_assert_mcmc_pred_stats(Start,End,UnitsPerSec) :-
    Time is (End - Start) / UnitsPerSec,
    assertz($ps_mcmc_pred_time(Time)).

$pp_assert_mcmc_exact_stats(Start,End,UnitsPerSec) :-
    Time is (End - Start) / UnitsPerSec,
    assertz($ps_mcmc_exact_time(Time)).


$pp_print_mcmc_sample_stats_message(Msg) :-
    ( Msg == 0 -> true
    ; format("Statistics on MCMC sampling:~n",[]),
      ( $pp_print_mcmc_sample_stats_message_aux,fail ; true ),nl
    ).
$pp_print_mcmc_sample_stats_message_aux :-
    $ps_mcmc_sample_time(L), format("~tMCMC sampling time: ~3f~n",[L]).

$pp_print_mcmc_marg_stats_message(Msg) :-
    ( Msg == 0 -> true
    ; format("Statistics on estimated log-marginal-likelihood:~n",[]),
      ( $pp_print_mcmc_marg_stats_message_aux,fail ; true ),nl
    ).
$pp_print_mcmc_marg_stats_message_aux :-
    $ps_mcmc_marg_time(L), format("~tTime for estimated log-marginal-likelihood: ~3f~n",[L]).

$pp_print_mcmc_pred_stats_message(Msg) :-
    ( Msg == 0 -> true
    ; format("Statistics on MCMC prediction:~n",[]),
      ( $pp_print_mcmc_pred_stats_message_aux,fail ; true ),nl
    ).
$pp_print_mcmc_pred_stats_message_aux :-
    $ps_mcmc_pred_time(L), format("~tTime for MCMC prediction: ~3f~n",[L]).

$pp_print_mcmc_exact_stats_message(Msg) :-
    ( Msg == 0 -> true
    ; format("Statistics on exact log-marginal-likelihood:~n",[]),
      ( $pp_print_mcmc_exact_stats_message_aux,fail ; true ),nl
    ).
$pp_print_mcmc_exact_stats_message_aux :-
    $ps_mcmc_exact_time(L), format("~tTime for exact log-marginal-likelihood: ~3f~n",[L]).

%-----------------------------------------------------------------------------------
% Message control

$pp_print_search_progress(MsgS) :-
    ( MsgS == 0 -> global_set($pg_num_goals,-1)
    ; global_set($pg_num_goals,0)
    ).

$pp_disable_message(Msg) :-
    $pp_print_search_progress(0),
    get_prism_flag(mcmc_message,Msg),
    set_prism_flag(mcmc_message,none).

$pp_enable_message(Msg) :-
    set_prism_flag(mcmc_message,Msg).
prob(Goal) :-
    prob(Goal,P),
    ( $pp_in_log_scale -> Text = 'Log-probability' ; Text = 'Probability' ),
    format("~w of ~w is: ~15f~n",[Text,Goal,P]).

prob(Goal,Prob) :-
    $pp_require_tabled_probabilistic_atom(Goal,$msg(0006),prob/2),
    $pp_prob(Goal,Prob).

$pp_prob(msw(Sw,V),Prob) :-
    $pp_require_ground(Sw,$msg(0101),prob/2),
    $pp_require_switch_outcomes(Sw,$msg(0102),prob/2),
    $pp_clean_infer_stats,
    ( var(V) ->
        cputime(T0),
        ( $pp_in_log_scale -> Prob = 0.0 ; Prob = 1.0 ),
        cputime(T1),
        InfTime is T1 - T0,
        $pp_assert_prob_stats1(InfTime)
    ; % else
        cputime(T0),
        $pp_get_value_prob(Sw,V,Prob0),
        ( $pp_in_log_scale -> Prob is log(Prob0) ; Prob = Prob0 ),
        cputime(T1),
        InfTime is T1 - T0,
        $pp_assert_prob_stats1(InfTime)
    ),
    $pp_assert_prob_stats2(0.0,0.0),!.

$pp_prob(Goal,Prob) :-
    $pp_clean_infer_stats,
    cputime(T0),
    $pp_prob_core(Goal,Prob),
    cputime(T1),
    InfTime is T1 - T0,
    $pp_assert_prob_stats1(InfTime),!.

log_prob(Goal) :-
    log_prob(Goal,P),format("Log-probability of ~w is: ~15f~n",[Goal,P]).
log_prob(Goal,P) :-
    $pp_prob(Goal,P0),( $pp_in_log_scale -> P = P0 ; P is log(P0) ).
    
$pp_in_log_scale :-
    get_prism_flag(log_scale,on).

$pp_prob_core(Goal,Prob) :-
    ground(Goal),
    $pp_is_tabled_probabilistic_atom(Goal),!,
    $pp_init_tables_aux,
    $pp_clean_graph_stats,
    $pp_init_tables_if_necessary,!,
    cputime(T1),
    $pp_find_explanations(Goal),
    cputime(T2),
    $pp_compute_inside(Goal,Prob),!,
    cputime(T3),
    $pc_import_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    $pp_assert_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    SearchTime  is T2 - T1,
    NumCompTime is T3 - T2,
    $pp_assert_prob_stats2(SearchTime,NumCompTime),!.

$pp_prob_core(Goal,Prob) :-
    $pp_init_tables_aux,
    $pp_clean_graph_stats,
    $pp_init_tables_if_necessary,!,
    copy_term(Goal,GoalCp),
    ( $pp_trans_one_goal(GoalCp,CompGoal) -> BodyGoal = CompGoal
    ; BodyGoal = (savecp(CP),Depth=0,
                  $pp_expl_interp_goal(GoalCp,Depth,CP,[],_,[],_,[],_,[],_))
    ),
    $pp_create_dummy_goal(DummyGoal),
    Clause = (DummyGoal:-BodyGoal,
                         $pc_prism_goal_id_register(GoalCp,GId),
                         $pc_prism_goal_id_register(DummyGoal,HId),
                         $prism_eg_path(HId,[GId],[])),
    Prog = [pred(DummyGoal,0,_Mode,_Delay,tabled(_,_,_,_),[Clause])],
    $pp_consult_preds_cond([],Prog),!,
    cputime(T1),
    $pp_find_explanations(DummyGoal),
    cputime(T2),
    $pp_compute_inside(DummyGoal,Prob),
    cputime(T3),
    $pc_import_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    $pp_assert_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    SearchTime  is T2 - T1,
    NumCompTime is T3 - T2,
    $pp_assert_prob_stats2(SearchTime,NumCompTime),!.

% Sws = [sw(Id,Instances,Probs,Deltas,FixedP,FixedH),...]
$pp_compute_inside(Goal,Prob) :-
    $pp_export_sw_info,
    $pc_prism_goal_id_get(Goal,Gid),
    $pc_compute_inside(Gid,Prob),!.

$pp_get_value_prob(Sw,V,Prob) :-
    $pp_get_parameters(Sw,Values,Probs),
    $pp_get_value_prob(Values,Probs,V,Prob).

$pp_get_value_prob([V|_],[Prob0|_],V,Prob) :- !, Prob = Prob0.
$pp_get_value_prob([_|Vs],[_|Probs],V,Prob) :- !,
    $pp_get_value_prob(Vs,Probs,V,Prob).

get_subgoal_hashtable(GTab) :-
    $pp_get_subgoal_hashtable(GTab).

$pp_get_subgoal_hashtable(GTab) :-
    $pc_prism_goal_count(GC),
    new_hashtable(GTab,GC),
    $pp_get_subgoal_hashtable(0,GC,GTab).

$pp_get_subgoal_hashtable(Gid,N,_) :- Gid >= N,!.
$pp_get_subgoal_hashtable(Gid,N,GTab) :-
    $pc_prism_goal_term(Gid,G),
    hashtable_put(GTab,Gid,G),
    Gid1 is Gid + 1,!,
    $pp_get_subgoal_hashtable(Gid1,N,GTab).

get_switch_hashtable(SwTab) :-
    $pp_get_switch_hashtable(SwTab).

$pp_get_switch_hashtable(SwTab) :-
    $pc_prism_sw_ins_count(IC),
    new_hashtable(SwTab,IC),
    $pp_get_switch_hashtable(0,IC,SwTab).

$pp_get_switch_hashtable(Sid,N,_) :- Sid >= N,!.
$pp_get_switch_hashtable(Sid,N,SwTab) :-
    $pc_prism_sw_ins_term(Sid,S),
    hashtable_put(SwTab,Sid,S),
    Sid1 is Sid + 1,!,
    $pp_get_switch_hashtable(Sid1,N,SwTab).

probf(Goal) :-
    $pp_probf(Goal,Expls,1,0), \+ \+ print_graph(Expls,[lr('<=>')]).
probfi(Goal) :-
    $pp_probf(Goal,Expls,1,1), \+ \+ print_graph(Expls,[lr('<=>')]).
probfo(Goal) :-
    $pp_probf(Goal,Expls,1,2), \+ \+ print_graph(Expls,[lr('<=>')]).
probfv(Goal) :-
    $pp_probf(Goal,Expls,1,3), \+ \+ print_graph(Expls,[lr('<=>')]).
probfio(Goal) :-
    $pp_probf(Goal,Expls,1,4), \+ \+ print_graph(Expls,[lr('<=>')]).

probf(Goal,Expls) :-
    $pp_probf(Goal,Expls,1,0).
probfi(Goal,Expls) :-
    $pp_probf(Goal,Expls,1,1).
probfo(Goal,Expls) :-
    $pp_probf(Goal,Expls,1,2).
probfv(Goal,Expls) :-
    $pp_probf(Goal,Expls,1,3).
probfio(Goal,Expls) :-
    $pp_probf(Goal,Expls,1,4).

probef(Goal) :-
    $pp_probf(Goal,Expls,0,0), \+ \+ print_graph(Expls,[lr('<=>')]).
probefi(Goal) :-
    $pp_probf(Goal,Expls,0,1), \+ \+ print_graph(Expls,[lr('<=>')]).
probefo(Goal) :-
    $pp_probf(Goal,Expls,0,2), \+ \+ print_graph(Expls,[lr('<=>')]).
probefv(Goal) :-
    $pp_probf(Goal,Expls,0,3), \+ \+ print_graph(Expls,[lr('<=>')]).
probefio(Goal) :-
    $pp_probf(Goal,Expls,0,4), \+ \+ print_graph(Expls,[lr('<=>')]).

probef(Goal,Expls) :-
    $pp_probf(Goal,Expls,0,0).
probefi(Goal,Expls) :-
    $pp_probf(Goal,Expls,0,1).
probefo(Goal,Expls) :-
    $pp_probf(Goal,Expls,0,2).
probefv(Goal,Expls) :-
    $pp_probf(Goal,Expls,0,3).
probefio(Goal,Expls) :-
    $pp_probf(Goal,Expls,0,4).

probef(Goal,Expls,GoalHashTab,SwHashTab) :-
    $pp_probf(Goal,Expls,0,0),
    $pp_get_subgoal_hashtable(GoalHashTab),
    $pp_get_switch_hashtable(SwHashTab).
probefi(Goal,Expls,GoalHashTab,SwHashTab) :-
    $pp_probf(Goal,Expls,0,1),
    $pp_get_subgoal_hashtable(GoalHashTab),
    $pp_get_switch_hashtable(SwHashTab).
probefo(Goal,Expls,GoalHashTab,SwHashTab) :-
    $pp_probf(Goal,Expls,0,2),
    $pp_get_subgoal_hashtable(GoalHashTab),
    $pp_get_switch_hashtable(SwHashTab).
probefv(Goal,Expls,GoalHashTab,SwHashTab) :-
    $pp_probf(Goal,Expls,0,3),
    $pp_get_subgoal_hashtable(GoalHashTab),
    $pp_get_switch_hashtable(SwHashTab).
probefio(Goal,Expls,GoalHashTab,SwHashTab) :-
    $pp_probf(Goal,Expls,0,4),
    $pp_get_subgoal_hashtable(GoalHashTab),
    $pp_get_switch_hashtable(SwHashTab).

%% PrMode is one of 0 (none), 1 (inside), 2 (outside), 3 (viterbi) and
%% 4 (inside-outside)

$pp_probf(Goal,Expls,Decode,PrMode) :-
    $pp_require_tabled_probabilistic_atom(Goal,$msg(0006),$pp_probf/4),
    $pp_compute_expls(Goal,Expls,Decode,PrMode),
    $pp_garbage_collect.

$pp_compute_expls(Goal,Expls,Decode,PrMode) :-
    Goal = msw(I,V),!,
    $pp_require_ground(I,$msg(0101),$pp_probf/4),
    $pp_require_switch_outcomes(I,$msg(0102),$pp_probf/4),
    $pp_clean_infer_stats,
    ( ground(V) -> V = VCp ; copy_term(V,VCp) ),
    $pp_create_dummy_goal(DummyGoal),
    DummyBody = ($prism_expl_msw(I,VCp,Sid),
                 $pc_prism_goal_id_register(DummyGoal,Hid),
                 $prism_eg_path(Hid,[],[Sid])),
    Prog = [pred(DummyGoal,0,_,_,tabled(_,_,_,_),[(DummyGoal:-DummyBody)])],
    $pp_consult_preds_cond([],Prog),
    cputime(T0),
    $pp_compute_expls(DummyGoal,Goal,Expls,Decode,PrMode,T0),!.

$pp_compute_expls(Goal,Expls,Decode,PrMode) :-
    $pp_is_tabled_probabilistic_atom(Goal),
    ground(Goal),!,
    $pp_clean_infer_stats,
    cputime(T0),
    $pp_compute_expls(Goal,_,Expls,Decode,PrMode,T0),!.

$pp_compute_expls(Goal,Expls,Decode,PrMode) :-
    $pp_clean_infer_stats,
    copy_term(Goal,GoalCp),
    ( $pp_trans_one_goal(GoalCp,CompGoal) ->
      BodyGoal = CompGoal
    ; BodyGoal = (savecp(CP),Depth=0,
                  $pp_expl_interp_goal(GoalCp,Depth,CP,[],_,[],_,[],_,[],_))
    ),
    $pp_create_dummy_goal(DummyGoal),
    DummyBody = (BodyGoal,
                 $pc_prism_goal_id_register(GoalCp,GId),
                 $pc_prism_goal_id_register(DummyGoal,HId),
                 $prism_eg_path(HId,[GId],[])),
    Prog = [pred(DummyGoal,0,_,_,tabled(_,_,_,_),[(DummyGoal:-DummyBody)])],
    $pp_consult_preds_cond([],Prog),
    cputime(T0),
    $pp_compute_expls(DummyGoal,Goal,Expls,Decode,PrMode,T0),!.

$pp_compute_expls(Goal,GLabel,Expls,Decode,PrMode,T0) :-
    $pp_init_tables_aux,
    $pp_clean_graph_stats,
    $pp_init_tables_if_necessary,!,
    cputime(T1),
    $pp_find_explanations(Goal),
    cputime(T2),
    $pc_prism_goal_id_get(Goal,Gid),
    $pc_alloc_sort_egraph(Gid),
    cputime(T3),
    (  PrMode == 0 -> true
    ;  $pp_export_sw_info,
       $pc_compute_probf(PrMode)
    ),
    cputime(T4),
    $pc_import_sorted_graph_size(Size),
    $pp_build_expls(Size,Decode,PrMode,GLabel,Expls),
    $pc_import_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    $pp_assert_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    cputime(T5),
    SearchTime  is T2 - T1,
    NumCompTime is T4 - T3,
    InfTime     is T5 - T0,
    ( PrMode == 0 -> $pp_assert_prob_stats2(SearchTime)
    ; $pp_assert_prob_stats2(SearchTime,NumCompTime)
    ),
    $pp_assert_prob_stats1(InfTime),!.

$pp_build_expls(I0,_,_,_,Expls), I0 =< 0 =>
    Expls = [].
$pp_build_expls(I0,Decode,PrMode,GLabel,Expls), I0 > 0 =>
    I is I0 - 1,
    $pc_import_sorted_graph_gid(I,Gid),
    $pc_import_sorted_graph_paths(I,Paths0),
    ( Decode == 0    -> Label = Gid
    ; nonvar(GLabel) -> Label = GLabel
    ; $pc_prism_goal_term(Gid,Label)
    ),
    ( PrMode == 0 -> Node = node(Label,Paths)  % probf
    ; PrMode == 4 ->                           % probfio
        $pp_get_gnode_probs(PrMode,Gid,Value),
        Node  = node(Label,Paths,Value),
        Value = [_,Vo]
    ; $pp_get_gnode_probs(PrMode,Gid,Value),
      Node  = node(Label,Paths,Value),
      Value = Vo
    ),
    $pp_decode_paths(Paths0,Paths,Decode,PrMode,Vo),
    Expls = [Node|Expls1],!,
    $pp_build_expls(I,Decode,PrMode,_,Expls1).



$pp_decode_paths([],[],_Decode,_PrMode,_Vo).
$pp_decode_paths([Pair|Pairs],[Path|Paths],Decode,PrMode,Vo) :-
    Pair = [Gids,Sids],
    $pp_decode_gnodes(Gids,GNodes,Decode,PrMode,Vg),
    $pp_decode_snodes(Sids,SNodes,Decode,PrMode,Vs),
    get_prism_flag(log_scale,LogScale),
    ( PrMode == 0 -> 
        Path = path(GNodes,SNodes)
    ; PrMode == 1 -> ( LogScale == on -> Vi is Vg + Vs ; Vi is Vg * Vs),
        Path = path(GNodes,SNodes,Vi)
    ; PrMode == 2 ->
        Path = path(GNodes,SNodes,Vo)
    ; PrMode == 3 -> ( LogScale == on -> Vi is Vg + Vs ; Vi is Vg * Vs),
        Path = path(GNodes,SNodes,Vi)      
    ; PrMode == 4 -> ( LogScale == on -> Vi is Vg + Vs ; Vi is Vg * Vs),
        Path = path(GNodes,SNodes,[Vi,Vo])
    ),!,
    $pp_decode_paths(Pairs,Paths,Decode,PrMode,Vo).

$pp_decode_gnodes(Gids,GNodes,Decode,PrMode,V) :-
    get_prism_flag(log_scale,LogScale),
    ( LogScale == on -> V0 = 0.0 ; V0 = 1.0 ),
    $pp_decode_gnodes(Gids,GNodes,Decode,PrMode,LogScale,V0,V).

$pp_decode_gnodes([],[],_Decode,_PrMode,_LogScale,V,V) :- !.
$pp_decode_gnodes([Gid|Gids],[GNode|GNodes],Decode,PrMode,LogScale,V0,V) :-
    ( Decode == 0 -> Gid = Label
    ; $pc_prism_goal_term(Gid,Label)
    ),
    ( PrMode == 0 -> GNode = Label
    ; $pp_get_gnode_probs(PrMode,Gid,Value),
      GNode = gnode(Label,Value),
      ( LogScale == on ->
        V1 is Value + V0
      ; V1 is Value * V0
      )
    ),!,
    $pp_decode_gnodes(Gids,GNodes,Decode,PrMode,LogScale,V1,V).

$pp_decode_snodes(Sids,SNodes,Decode,PrMode,V) :-
    get_prism_flag(log_scale,LogScale),
    ( LogScale == on -> V0 = 0.0 ; V0 = 1.0 ),
    $pp_decode_snodes(Sids,SNodes,Decode,PrMode,LogScale,V0,V).

$pp_decode_snodes([],[],_Decode,_PrMode,_LogScale,V,V) :- !.
$pp_decode_snodes([Sid|Sids],[SNode|SNodes],Decode,PrMode,LogScale,V0,V) :-
    ( Decode == 0 -> Sid = Label
    ; $pc_prism_sw_ins_term(Sid,Label)
    ),
    ( PrMode == 0 -> SNode = Label
    ; $pp_get_snode_probs(PrMode,Sid,Value),
      SNode = snode(Label,Value),
      ( LogScale == on ->
        V1 is Value + V0
      ; V1 is Value * V0
      )
    ),!,
    $pp_decode_snodes(Sids,SNodes,Decode,PrMode,LogScale,V1,V).

$pp_get_gnode_probs(1,Gid,Pi) :- $pc_get_gnode_inside(Gid,Pi),!.
$pp_get_gnode_probs(2,Gid,Po) :- $pc_get_gnode_outside(Gid,Po),!.
$pp_get_gnode_probs(3,Gid,Pv) :- $pc_get_gnode_viterbi(Gid,Pv),!.
$pp_get_gnode_probs(4,Gid,[Pi,Po]) :-
    $pc_get_gnode_inside(Gid,Pi),
    $pc_get_gnode_outside(Gid,Po),!.

$pp_get_snode_probs(1,Sid,Pi) :- $pc_get_snode_inside(Sid,Pi),!.
$pp_get_snode_probs(2,Sid,E)  :- $pc_get_snode_expectation(Sid,E),!.
$pp_get_snode_probs(3,Sid,Pi) :- $pc_get_snode_inside(Sid,Pi),!.
$pp_get_snode_probs(4,Sid,[Pi,Po]) :-
    $pc_get_snode_inside(Sid,Pi),
    $pc_get_snode_expectation(Sid,Po),!.

%%%% Statistics

$pp_assert_prob_stats1(InfTime0) :-
    InfTime is InfTime0 / 1000.0,
    assertz($ps_infer_time(InfTime)),!.

$pp_assert_prob_stats2(SearchTime0) :-
    SearchTime is SearchTime0 / 1000.0,
    assertz($ps_infer_search_time(SearchTime)),!.

$pp_assert_prob_stats2(SearchTime0,NumCompTime0) :-
    SearchTime  is SearchTime0 / 1000.0,
    NumCompTime is NumCompTime0 / 1000.0,
    assertz($ps_infer_search_time(SearchTime)),
    assertz($ps_infer_calc_time(NumCompTime)),!.

$pp_clean_infer_stats :-
    retractall($ps_infer_time(_)),
    retractall($ps_infer_search_time(_)),
    retractall($ps_infer_calc_time(_)),!.
%%%% Viterbi wrappers

viterbi(G) :-
    $pp_viterbi_wrapper(viterbi(G)).
viterbi(G,P) :-
    $pp_viterbi_wrapper(viterbi(G,P)).
viterbif(G) :-
    $pp_viterbi_wrapper(viterbif(G)).
viterbif(G,P,V) :-
    $pp_viterbi_wrapper(viterbif(G,P,V)).
viterbit(G) :-
    $pp_viterbi_wrapper(viterbit(G)).
viterbit(G,P,T) :-
    $pp_viterbi_wrapper(viterbit(G,P,T)).
n_viterbi(N,G) :-
    $pp_viterbi_wrapper(n_viterbi(N,G)).
n_viterbi(N,G,P) :-
    $pp_viterbi_wrapper(n_viterbi(N,G,P)).
n_viterbif(N,G) :-
    $pp_viterbi_wrapper(n_viterbif(N,G)).
n_viterbif(N,G,V) :-
    $pp_viterbi_wrapper(n_viterbif(N,G,V)).
n_viterbit(N,G) :-
    $pp_viterbi_wrapper(n_viterbit(N,G)).
n_viterbit(N,G,T) :-
    $pp_viterbi_wrapper(n_viterbit(N,G,T)).
viterbig(G) :-
    $pp_viterbi_wrapper(viterbig(G)).
viterbig(G,P) :-
    $pp_viterbi_wrapper(viterbig(G,P)).
viterbig(G,P,V) :-
    $pp_viterbi_wrapper(viterbig(G,P,V)).
n_viterbig(N,G) :-
    $pp_viterbi_wrapper(n_viterbig(N,G)).
n_viterbig(N,G,P) :-
    $pp_viterbi_wrapper(n_viterbig(N,G,P)).
n_viterbig(N,G,P,V) :-
    $pp_viterbi_wrapper(n_viterbig(N,G,P,V)).

$pp_viterbi_wrapper(Pred0) :-
    get_prism_flag(viterbi_mode,Mode0),
    $pp_conv_viterbi_mode(Mode0,Mode),
    ( Mode == ml -> Suffix = '_p'
    ; Mode == vb -> Suffix = '_h'
    ),!,
    Pred0 =.. [Name0|Args],
    atom_concat(Name0,Suffix,Name1),
    Pred1 =.. [Name1|Args],!,
    call(Pred1).  % do not add cut here (n_viterbig is non-deterministic)

% introduced just for backward compatibility
$pp_conv_viterbi_mode(Mode0,Mode) :-
    ( Mode0 = params  -> Mode = ml
    ; Mode0 = hparams -> Mode = vb
    ; Mode = Mode0
    ).

%%%% Viterbi routine with C interface
%%
%% viterbi_p(G) :- print the Viterbi prob
%% viterbi_p(G,P) :- output the Viterbi prob
%% viterbif_p(G) :- print the Viterbi path and the Viterbi prob
%% viterbif_p(G,P,VPath) :- output the Viterbi path and the Viterbi prob
%%
%% VPath is a list of node(G,Paths), where Paths is a list of
%% path(Gs,Sws), where Gs are subgoals of G and Sws are switches.
%%
%% Usually in VPath, node(msw(Sw,V),[]) is omitted, but optionally
%% it can be included in VPath.

% Main routine:

% viterbi family:

viterbi_p(Goal) :-
    viterbif_p(Goal,Pmax,_),
    $pp_print_viterbi_prob(Pmax).

viterbi_p(Goal,Pmax) :-
    viterbif_p(Goal,Pmax,_).

% viterbif family:

viterbif_p(Goal) :-
    viterbif_p(Goal,Pmax,VNodeL),
    format("~n",[]),
    print_graph(VNodeL,[lr('<=')]),
    $pp_print_viterbi_prob(Pmax).

viterbif_p(Goal,Pmax,VNodeL) :-
    $pp_require_tabled_probabilistic_atom(Goal,$msg(0006),viterbif_p/3),
    ( Goal = msw(I,_) ->
        $pp_require_ground(I,$msg(0101),viterbif_p/3),
        $pp_require_switch_outcomes(I,$msg(0102),viterbif_p/3)
    ; true
    ),
    $pp_viterbif_p(Goal,Pmax,VNodeL).

$pp_viterbif_p(Goal,Pmax,VNodeL) :-
    $pp_clean_infer_stats,
    cputime(T0),
    $pp_viterbi_core(Goal,Pmax,VNodeL),
    cputime(T1),
    $pp_garbage_collect,
    InfTime is T1 - T0,
    $pp_assert_viterbi_stats1(InfTime),!.

% viterbit family:

viterbit_p(Goal) :-
    viterbit_p(Goal,Pmax,VTreeL),
    format("~n",[]),
    print_tree(VTreeL),
    $pp_print_viterbi_prob(Pmax).

viterbit_p(Goal,Pmax,VTreeL) :-
    $pp_require_tabled_probabilistic_atom(Goal,$msg(0006),viterbit_p/3),
    $pp_viterbif_p(Goal,Pmax,VNodeL),
    viterbi_tree(VNodeL,VTreeL).

% viterbig family:

viterbig_p(Goal) :-
    ( ground(Goal) -> viterbi_p(Goal)
    ; viterbig_p(Goal,_,_)
    ).

viterbig_p(Goal,Pmax) :-
    ( ground(Goal) -> viterbi_p(Goal,Pmax)
    ; viterbig_p(Goal,Pmax,_)
    ).

viterbig_p(Goal,Pmax,VNodeL) :-
    $pp_require_tabled_probabilistic_atom(Goal,$msg(0006),viterbig_p/3),
    ( Goal = msw(I,_) ->
        $pp_require_ground(I,$msg(0101),viterbif_p/3),
        $pp_require_switch_outcomes(I,$msg(0102),viterbig_p/3)
    ; true
    ),
    $pp_viterbig_p(Goal,Pmax,VNodeL).

$pp_viterbig_p(Goal,Pmax,VNodeL) :-
    $pp_clean_infer_stats,
    cputime(T0),
    $pp_viterbi_core(Goal,Pmax,VNodeL),
    ( ground(Goal) -> true
    ; VNodeL = [node(_,[path([Goal1],[])])|_] -> Goal = Goal1
    ; VNodeL = [node(_,[path([],[SwIns])])|_] -> Goal = SwIns
    ),
    cputime(T1),
    InfTime is T1 - T0,
    $pp_garbage_collect,
    $pp_assert_viterbi_stats1(InfTime),!.

%% Common routine:

$pp_print_viterbi_prob(Pmax) :-
    ( get_prism_flag(log_scale,off) -> format("~nViterbi_P = ~15f~n",[Pmax])
    ; format("~nlog(Viterbi_P) = ~15f~n",[Pmax])
    ).

$pp_viterbi_core(Goal,Pmax,VNodeL) :-
    Goal = msw(I,V),!,
    $pp_require_ground(I,$msg(0101),$pp_viterbi_core/3),
    $pp_require_switch_outcomes(I,$msg(0102),$pp_viterbi_core/3),
    $pp_init_tables_aux,
    $pp_clean_graph_stats,
    $pp_init_tables_if_necessary,!,
    ( ground(V) -> V = VCp ; copy_term(V,VCp) ),
    $pp_create_dummy_goal(DummyGoal),
    DummyBody = ($prism_expl_msw(I,VCp,Sid),
                 $pc_prism_goal_id_register(DummyGoal,Hid),
                 $prism_eg_path(Hid,[],[Sid])),
    Prog = [pred(DummyGoal,0,_,_,tabled(_,_,_,_),[(DummyGoal:-DummyBody)])],
    $pp_consult_preds_cond([],Prog),!,
    cputime(T1),
    $pp_find_explanations(DummyGoal),
    cputime(T2),
    $pp_compute_viterbi_p(DummyGoal,Pmax,[node(DummyGoal,Paths)|VNodeL0]),!,
    cputime(T3),
    VNodeL = [node(msw(I,V),Paths)|VNodeL0],
    $pc_import_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    $pp_assert_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    SearchTime  is T2 - T1,
    NumCompTime is T3 - T2,
    $pp_assert_viterbi_stats2(SearchTime,NumCompTime),!.

$pp_viterbi_core(Goal,Pmax,VNodeL) :-
    ground(Goal),!,
    $pp_init_tables_aux,
    $pp_clean_graph_stats,
    $pp_init_tables_if_necessary,!,
    cputime(T1),
    $pp_find_explanations(Goal),
    cputime(T2),
    $pp_compute_viterbi_p(Goal,Pmax,VNodeL),!,
    cputime(T3),
    $pc_import_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    $pp_assert_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    SearchTime  is T2 - T1,
    NumCompTime is T3 - T2,
    $pp_assert_viterbi_stats2(SearchTime,NumCompTime),!.

$pp_viterbi_core(Goal,Pmax,VNodeL) :-
    $pp_init_tables_aux,
    $pp_clean_graph_stats,
    $pp_init_tables_if_necessary,!,
    copy_term(Goal,GoalCp),
    ( $pp_trans_one_goal(GoalCp,CompGoal) -> BodyGoal = CompGoal
    ; BodyGoal = (savecp(CP),Depth=0,
                  $pp_expl_interp_goal(GoalCp,Depth,CP,[],_,[],_,[],_,[],_))
    ),
    $pp_create_dummy_goal(DummyGoal),
    DummyBody = (BodyGoal,
                 $pc_prism_goal_id_register(GoalCp,GId),
                 $pc_prism_goal_id_register(DummyGoal,HId),
                 $prism_eg_path(HId,[GId],[])),
    Prog = [pred(DummyGoal,0,_Mode,_Delay,tabled(_,_,_,_),
                 [(DummyGoal:-DummyBody)])],
    $pp_consult_preds_cond([],Prog),!,
    cputime(T1),
    $pp_find_explanations(DummyGoal),
    cputime(T2),
    $pp_compute_viterbi_p(DummyGoal,Pmax,[node(DummyGoal,Paths)|VNodeL0]),!,
    cputime(T3),
    VNodeL = [node(Goal,Paths)|VNodeL0],
    $pc_import_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    $pp_assert_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    SearchTime  is T2 - T1,
    NumCompTime is T3 - T2,
    $pp_assert_viterbi_stats2(SearchTime,NumCompTime),!.

% Sws = [sw(Id,Instances,Probs,PseudoCs,Fixed,FixedH),...]
$pp_compute_viterbi_p(Goal,Pmax,VNodeL) :-
    $pp_export_sw_info,
    $pc_prism_goal_id_get(Goal,Gid),
    $pc_compute_viterbi(Gid,EGs,EGPaths,ESwPaths,Pmax),
    $pp_decode_viterbi_path(EGs,EGPaths,ESwPaths,VNodeL),!.

$pp_decode_viterbi_path([],[],[],[]) :- !.
$pp_decode_viterbi_path([Gid|Gids],
                        [GPath|GPaths],[SPath|SPaths],
                        [Node|Nodes]) :-
    $pc_prism_goal_term(Gid,G),
    ( GPath == [], SPath == [] ->
        get_prism_flag(explicit_empty_expls,V),
        ( V == off -> Node = node(G,[])
        ; Node = node(G,[path([],[])])
        )
    ; $pp_decode_gnodes(GPath,GPathDec,1,0,_Vg),
      $pp_decode_snodes(SPath,SPathDec,1,0,_Vs),
      Node = node(G,[path(GPathDec,SPathDec)])
    ),!,
    $pp_decode_viterbi_path(Gids,GPaths,SPaths,Nodes).


%%%%
%%%%  Top-N Viterbi
%%%%
%%%% n_viterbi_p(N,G) :- print the top-N Viterbi probs
%%%% n_viterbi_p(N,G,Ps) :- output the top-N Viterbi probs
%%%% n_viterbif_p(N,G) :- print the top-N Viterbi paths and the corresponding
%%%%                     Viterbi probs
%%%% n_viterbif_p(N,G,VPathL) :- output the list of top-N Viterbi paths and
%%%%                            the corresponding Viterbi probs
%%%%

% n_viterbi family

n_viterbi_p(N,Goal) :-
    n_viterbif_p(N,Goal,VPathL),
    ( member(v_expl(J,Pmax,_),VPathL),
      $pp_print_n_viterbi(J,Pmax),
      fail
    ; true
    ).

n_viterbi_p(N,Goal,Ps) :-
    n_viterbif_p(N,Goal,VPathL),!,
    findall(Pmax,member(v_expl(_,Pmax,_),VPathL),Ps).

% n_viterbif family

n_viterbif_p(N,Goal) :-
    n_viterbif_p(N,Goal,VPathL),!,
    $pp_print_n_viterbif(VPathL).

n_viterbif_p(N,Goal,VPathL) :-
    $pp_require_positive_integer(N,$msg(1400),n_viterbif_p/3),
    $pp_require_tabled_probabilistic_atom(Goal,$msg(0006),n_viterbif_p/3),
    $pp_n_viterbif_p(N,Goal,VPathL).

$pp_n_viterbif_p(N,Goal,VPathL) :-
    $pp_clean_infer_stats,
    cputime(T0),
    $pp_n_viterbi_p_core(N,Goal,VPathL),
    cputime(T1),
    InfTime is T1 - T0,
    $pp_garbage_collect,
    $pp_assert_viterbi_stats1(InfTime),!.

% n_viterbit family

n_viterbit_p(N,Goal) :-
    n_viterbif_p(N,Goal,VPathL),!,
    $pp_print_n_viterbit(VPathL).

n_viterbit_p(N,Goal,VPathL) :-
    n_viterbif_p(N,Goal,VPathL0),!,
    $pp_build_n_viterbit(VPathL0,VPathL).

%%%% 
%%%% $pp_n_viterbig_p(N,Goal) :- the same as $pp_n_viterbig_p(N,Goal,_,_)
%%%% $pp_n_viterbig_p(N,Goal,Pmax) :- the same as $pp_n_viterbig_p(N,Goal,Pmax,_)
%%%% $pp_n_viterbig_p(N,Goal,Pmax,VNodeL) :-
%%%%      if Goal is not ground, unify Goal with the first element in the K-th
%%%%      Viterbi path VNodeL (K=0,1,2,...,(N-1) on backtracking). Pmax is the
%%%%      probability of VNodeL.
%%%%

n_viterbig_p(N,Goal) :-
    ( ground(Goal) -> n_viterbi_p(N,Goal)
    ; n_viterbig_p(N,Goal,_,_)
    ).

n_viterbig_p(N,Goal,Pmax) :-
    ( ground(Goal) -> n_viterbi_p(N,Goal,Ps),!,member(Pmax,Ps)
    ; n_viterbig_p(N,Goal,Pmax,_)
    ).

n_viterbig_p(N,Goal,Pmax,VNodeL) :-
    $pp_require_positive_integer(N,$msg(1400),n_viterbi_p/3),
    $pp_require_tabled_probabilistic_atom(Goal,$msg(0006),n_viterbi_p/3),
    $pp_n_viterbig_p(N,Goal,Pmax,VNodeL).

$pp_n_viterbig_p(N,Goal,Pmax,VNodeL) :-
    $pp_clean_infer_stats,
    cputime(T0),
    $pp_n_viterbi_p_core(N,Goal,VPathL),!,
    cputime(T1),
    InfTime is T1 - T0,
    $pp_garbage_collect,
    $pp_assert_viterbi_stats1(InfTime),!,
    ( ground(Goal) -> member(v_expl(J,Pmax,VNodeL),VPathL)
    ; Goal = msw(_,_) ->
        member(v_expl(J,Pmax,VNodeL),VPathL),
        VNodeL = [node(_,[path([],[SwIns])])|_],
        Goal = SwIns
    ; % else
        member(v_expl(J,Pmax,VNodeL),VPathL),
        VNodeL = [node(_,[path([Goal1],[])])|_],
        Goal = Goal1
    ).

%% Common routines:

$pp_print_n_viterbi(J,Pmax) :-
    ( get_prism_flag(log_scale,off) ->
          format("#~w: Viterbi_P = ~15f~n",[J,Pmax])
    ; format("#~w: log(Viterbi_P) = ~15f~n",[J,Pmax])
    ).

$pp_print_n_viterbif([]).
$pp_print_n_viterbif([v_expl(J,Pmax,VNodeL)|VPathL]) :-
    format("~n#~w~n",[J]),
    print_graph(VNodeL,[lr('<=')]),
    ( get_prism_flag(log_scale,off) -> format("~nViterbi_P = ~15f~n",[Pmax])
    ; format("~nlog(Viterbi_P) = ~15f~n",[Pmax])
    ),!,
    $pp_print_n_viterbif(VPathL).

$pp_print_n_viterbit([]).
$pp_print_n_viterbit([v_expl(J,Pmax,VNodeL)|VPathL]) :-
    format("~n#~w~n",[J]),
    viterbi_tree(VNodeL,VTreeL),
    print_tree(VTreeL),
    $pp_print_viterbi_prob(Pmax),!,
    $pp_print_n_viterbit(VPathL).

$pp_build_n_viterbit([],[]).
$pp_build_n_viterbit([v_expl(J,Pmax,VNodeL)|VPathL0],
                     [v_tree(J,Pmax,VTreeL)|VPathL1]) :-
    viterbi_tree(VNodeL,VTreeL),!,
    $pp_build_n_viterbit(VPathL0,VPathL1).

$pp_n_viterbi_p_core(N,Goal,VPathL) :-
    Goal = msw(I,V),!,
    $pp_require_ground(I,$msg(0101),$pp_n_viterbi_p_core/3),
    $pp_require_switch_outcomes(I,$msg(0102),$pp_n_viterbi_p_core/3),
    $pp_init_tables_aux,
    $pp_clean_graph_stats,
    $pp_init_tables_if_necessary,!,
    ( ground(V) -> V = VCp ; copy_term(V,VCp) ),
    $pp_create_dummy_goal(DummyGoal),
    DummyBody = ($prism_expl_msw(I,VCp,Sid),
                 $pc_prism_goal_id_register(DummyGoal,Hid),
                 $prism_eg_path(Hid,[],[Sid])),
    Prog = [pred(DummyGoal,0,_Mode,_Delay,tabled(_,_,_,_),
                 [(DummyGoal:-DummyBody)])],
    $pp_consult_preds_cond([],Prog),!,
    cputime(T1),
    $pp_find_explanations(DummyGoal),
    cputime(T2),
    $pp_compute_n_viterbi_p(N,DummyGoal,VPathL0),!,
    cputime(T3),
    $pp_replace_dummy_goal(Goal,DummyGoal,VPathL0,VPathL),
    $pc_import_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    $pp_assert_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    SearchTime  is T2 - T1,
    NumCompTime is T3 - T2,
    $pp_assert_viterbi_stats2(SearchTime,NumCompTime),!.

$pp_n_viterbi_p_core(N,Goal,VPathL) :-
    ground(Goal),!,
    $pp_init_tables_aux,
    $pp_clean_graph_stats,
    $pp_init_tables_if_necessary,!,
    cputime(T1),
    $pp_find_explanations(Goal),
    cputime(T2),
    $pp_compute_n_viterbi_p(N,Goal,VPathL),!,
    cputime(T3),
    $pc_import_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    $pp_assert_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    SearchTime  is T2 - T1,
    NumCompTime is T3 - T2,
    $pp_assert_viterbi_stats2(SearchTime,NumCompTime),!.

$pp_n_viterbi_p_core(N,Goal,VPathL) :-
    $pp_init_tables_aux,
    $pp_clean_graph_stats,
    $pp_init_tables_if_necessary,!,
    copy_term(Goal,GoalCp),
    ( $pp_trans_one_goal(GoalCp,CompGoal) -> BodyGoal = CompGoal
    ; BodyGoal = (savecp(CP),Depth=0,
                  $pp_expl_interp_goal(GoalCp,Depth,CP,[],_,[],_,[],_,[],_))
    ),
    $pp_create_dummy_goal(DummyGoal),
    DummyBody = (BodyGoal,
                 $pc_prism_goal_id_register(GoalCp,GId),
                 $pc_prism_goal_id_register(DummyGoal,HId),
                 $prism_eg_path(HId,[GId],[])),
    Prog = [pred(DummyGoal,0,_Mode,_Delay,tabled(_,_,_,_),
                 [(DummyGoal:-DummyBody)])],
    $pp_consult_preds_cond([],Prog),!,
    cputime(T1),
    $pp_find_explanations(DummyGoal),
    cputime(T2),
    $pp_compute_n_viterbi_p(N,DummyGoal,VPathL0),!,
    cputime(T3),
    $pp_replace_dummy_goal(Goal,DummyGoal,VPathL0,VPathL),
    $pc_import_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    $pp_assert_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    SearchTime  is T2 - T1,
    NumCompTime is T3 - T2,
    $pp_assert_viterbi_stats2(SearchTime,NumCompTime),!.

$pp_compute_n_viterbi_p(N,Goal,VPathL) :-
    $pp_export_sw_info,!,
    $pc_prism_goal_id_get(Goal,Gid),
    $pc_compute_n_viterbi(N,Gid,VPathL0),
    $pp_build_n_viterbi_path(VPathL0,VPathL),!.

$pp_replace_dummy_goal(_,_,[],[]).
$pp_replace_dummy_goal(Goal,DummyGoal,
                       [v_expl(J,Pmax,VNodeL0)|VPathL0],
                       [v_expl(J,Pmax,VNodeL)|VPathL]) :-
    VNodeL0 = [node(DummyGoal,Paths)|VNodeL1],
    VNodeL  = [node(Goal,Paths)|VNodeL1],!,
    $pp_replace_dummy_goal(Goal,DummyGoal,VPathL0,VPathL).

$pp_build_n_viterbi_path([],[]).
$pp_build_n_viterbi_path([v_expl(J,EGs,EGPaths,ESwPaths,Pmax)|VPathL0],
                         [v_expl(J,Pmax,VNodeL)|VPathL]) :-
    $pp_decode_viterbi_path(EGs,EGPaths,ESwPaths,VNodeL),
    $pp_build_n_viterbi_path(VPathL0,VPathL).

%%  Viterbi with reranking based on VB
%%
%% viterbi_h(G) :- the same as n_viterbi_h([1,default],G)
%% viterbi_h(G,P) :- the same as n_viterbi_h([1,default],G,P)
%% viterbif_h(G) :- the same as n_viterbif_h([1,default],G)
%% viterbif_h(G,P,VPath) :- the same as
%%                          n_viterbif_h([1,default],[v_expl(0,P,VPath)])
%%
%% n_viterbi_h(N,G) :- the same as n_viterbi_h([N,default],G)
%% n_viterbi_h(N,G,Ps) :- the same as n_viterbi_h([N,default],G,Ps)
%% n_viterbi_h([N,M],G) :- print top-N Viterbi probs selected from top-M
%%                         Viterbi probs based on ML/MAP (M > N)
%% n_viterbi_h([N,M],G,Ps) :- output top-N Viterbi probs selected from top-M
%%                            Viterbi probs based on ML/MAP (M > N)
%% n_viterbif_h(N,G) :- the same as n_viterbif_h([N,default],G)
%% n_viterbif_h(N,G,VPathL) :- the same as n_viterbif_h([N,default],G,VPathL)
%% n_viterbif_h([N,M],G) :- print the top-N Viterbi paths and the corresponding
%%                         Viterbi probs selected from the top-N Viterbi paths
%%                         based on ML/MAP (M > N)
%% n_viterbif_h([N,M],G,VPathL) :-
%%         output the list of the top-N Viterbi paths and the corresponding
%%         Viterbi probs selected from top-N Viterbi paths based on ML/MAP
%%         (M =< N)
%%
%% viterbig_h(Goal) :- the same as n_viterbig_h(1,Goal)
%% viterbig_h(Goal,Pmax) :- the same as n_viterbig_h(1,Goal,Pmax)
%% viterbig_h(Goal,Pmax,VNodeL) :- the same as n_viterbig_h(1,Goal,Pmax,VNodeL)
%%
%% n_viterbig_h(N,Goal) :- the same as n_viterbig_h(N,Goal,_,_)
%% n_viterbig_h([N,M],Goal) :- the same as n_viterbig_h([N,M],Goal,_,_)
%% n_viterbig_h(N,Goal,Pmax) :- the same as n_viterbig_h(N,Goal,Pmax,_)
%% n_viterbig_h([N,M],Goal,Pmax) :- the same as n_viterbig_h([N,M],Goal,Pmax,_)
%% n_viterbig_h(N,Goal,Pmax) :-
%%         the same as n_viterbig_h([N,default],Goal,Pmax,_)
%% n_viterbig_h(N,Goal,Pmax,VNodeL) :-
%%         the same as n_viterbig_h([N,default],Goal,Pmax,VNodeL)
%% n_viterbig_h([N,M],Goal,Pmax,VNodeL) :-
%%         If Goal is not ground, unify Goal with the first element in the K-th
%%         Viterbi path VNodeL (K=1,2,... on backtracking). Pmax is the
%%         probability of VNodeL.

viterbi_h(G)   :- n_viterbi_h([1,default],G).
viterbi_h(G,P) :- n_viterbi_h([1,default],G,[P]).
viterbif_h(G)  :- n_viterbif_h([1,default],G).
viterbif_h(G,P,VPath) :- n_viterbif_h([1,default],G,[v_expl(0,P,VPath)]).
viterbit_h(G) :- n_viterbit_h([1,default],G).
viterbit_h(G,P,VTree) :-
    n_viterbif_h([1,default],G,[v_expl(0,P,VPath)]),!,
    viterbi_tree(VPath,VTree).

n_viterbi_h([N,M],G) :- !,
    n_viterbif_h([N,M],G,VPathL),!,
    ( member(v_expl(J,Pmax,_),VPathL),
      $pp_print_n_viterbi(J,Pmax),
      fail
    ; true
    ).
n_viterbi_h(N,G) :- n_viterbi_h([N,default],G).

n_viterbi_h([N,M],G,Ps) :- !,
    n_viterbif_h([N,M],G,VPathL),!,
    findall(Pmax,member(v_expl(_,Pmax,_),VPathL),Ps).
n_viterbi_h(N,G,Ps) :- n_viterbi_h([N,default],G,Ps).

n_viterbif_h([N,M],G) :- !,
    n_viterbif_h([N,M],G,VPathL),!,
    $pp_print_n_viterbif(VPathL).
n_viterbif_h(N,G) :-
    n_viterbif_h([N,default],G).

n_viterbif_h([N,M],Goal,VPathL) :- !,
    ( M == default ->
        get_prism_flag(rerank,M1),!,
        n_viterbif_h([N,M1],Goal,VPathL)
    ; % M \== default
        $pp_require_positive_integer(N,$msg(1400),n_viterbif_h/3),
        $pp_require_positive_integer(M,$msg(1401),n_viterbif_h/3),
        $pp_require_tabled_probabilistic_atom(Goal,$msg(0006),n_viterbif_h/3),
        ( N > M -> N1 = M ; N1 = N ),!,
        $pp_n_viterbif_h([N1,M],Goal,VPathL)
    ).

n_viterbif_h(N,G,VPathL) :-
    n_viterbif_h([N,default],G,VPathL).

$pp_n_viterbif_h([N,M],Goal,VPathL) :-
    $pp_clean_infer_stats,
    cputime(T0),
    $pp_n_viterbi_h_core(N,M,Goal,VPathL),
    cputime(T1),
    InfTime is T1 - T0,
    $pp_garbage_collect,
    $pp_assert_viterbi_stats1(InfTime),!.

n_viterbit_h([N,M],G) :- !,
    n_viterbif_h([N,M],G,VPathL),!,
    $pp_print_n_viterbit(VPathL).
n_viterbit_h(N,G) :-
    n_viterbit_h([N,default],G).

n_viterbit_h([N,M],G,VPathL) :- !,
    n_viterbif_h([N,M],G,VPathL0),!,
    $pp_build_n_viterbit(VPathL0,VPathL).
n_viterbit_h(N,G,VPathL) :-
    n_viterbit_h([N,default],G,VPathL).

viterbig_h(Goal) :- n_viterbig_h(1,Goal).
viterbig_h(Goal,Pmax) :- n_viterbig_h(1,Goal,Pmax).
viterbig_h(Goal,Pmax,VNodeL) :- n_viterbig_h(1,Goal,Pmax,VNodeL).

n_viterbig_h([N,M],Goal) :- !,
    ( ground(Goal) -> n_viterbi_h([N,M],Goal)
    ; n_viterbig_h([N,M],Goal,_,_)
    ).
n_viterbig_h(N,Goal) :-
    ( ground(Goal) -> n_viterbi_h(N,Goal)
    ; n_viterbig_h(N,Goal,_,_)
    ).

n_viterbig_h([N,M],Goal,Pmax) :- !,
    ( ground(Goal) -> n_viterbi_h([N,M],Goal,Ps),!,member(Pmax,Ps)
    ; n_viterbig_h([N,M],Goal,Pmax,_)
    ).
n_viterbig_h(N,Goal,Pmax) :-
    ( ground(Goal) -> n_viterbi_h(N,Goal,Ps),!,member(Pmax,Ps)
    ; n_viterbig_h(N,Goal,Pmax,_)
    ).

n_viterbig_h([N,default],Goal,Pmax,VNodeL) :- !,
    get_prism_flag(rerank,M),!,
    n_viterbig_h([N,M],Goal,Pmax,VNodeL).
n_viterbig_h([N,M],Goal,Pmax,VNodeL) :- !,
    $pp_require_positive_integer(N,$msg(1400),n_viterbig_h/3),
    $pp_require_positive_integer(M,$msg(1401),n_viterbig_h/3),
    $pp_require_tabled_probabilistic_atom(Goal,$msg(0006),n_viterbig_h/3),
    ( N > M -> N1 = M ; N1 = N ),!,
    $pp_n_viterbig_h([N1,M],Goal,Pmax,VNodeL).
n_viterbig_h(N,Goal,Pmax,VNodeL) :-
    n_viterbig_h([N,default],Goal,Pmax,VNodeL).

$pp_n_viterbig_h([N,M],Goal,Pmax,VNodeL) :- !,
    $pp_clean_infer_stats,
    cputime(T0),
    $pp_n_viterbi_h_core(N,M,Goal,VPathL),
    cputime(T1),
    InfTime is T1 - T0,
    $pp_garbage_collect,
    $pp_assert_viterbi_stats1(InfTime),!,
    ( ground(Goal) -> member(v_expl(J,Pmax,VNodeL),VPathL)
    ; Goal = msw(_,_) ->
        member(v_expl(J,Pmax,VNodeL),VPathL),
        VNodeL = [node(_,[path([],[SwIns])])|_],
        Goal = SwIns
    ; % else
        member(v_expl(J,Pmax,VNodeL),VPathL),
        VNodeL = [node(_,[path([Goal1],[])])|_],
        Goal = Goal1
    ).

%% Common routines:

$pp_n_viterbi_h_core(N,M,Goal,VPathL) :-
    Goal = msw(I,V),!,
    $pp_require_ground(I,$msg(0101),$pp_n_viterbi_h_core/3),
    $pp_require_switch_outcomes(I,$msg(0102),$pp_n_viterbi_h_core/3),
    $pp_init_tables_aux,
    $pp_clean_graph_stats,
    $pp_init_tables_if_necessary,!,
    ( ground(V) -> V = VCp ; copy_term(V,VCp) ),
    $pp_create_dummy_goal(DummyGoal),
    DummyBody = ($prism_expl_msw(I,VCp,Sid),
                 $pc_prism_goal_id_register(DummyGoal,Hid),
                 $prism_eg_path(Hid,[],[Sid])),
    Prog = [pred(DummyGoal,0,_Mode,_Delay,tabled(_,_,_,_),
                 [(DummyGoal:-DummyBody)])],
    $pp_consult_preds_cond([],Prog),!,
    cputime(T1),
    $pp_find_explanations(DummyGoal),
    cputime(T2),
    $pp_compute_n_viterbi_h(N,M,DummyGoal,VPathL0),!,
    cputime(T3),
    $pp_replace_dummy_goal(Goal,DummyGoal,VPathL0,VPathL),
    $pc_import_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    $pp_assert_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    SearchTime  is T2 - T1,
    NumCompTime is T3 - T2,
    $pp_assert_viterbi_stats2(SearchTime,NumCompTime),!.

$pp_n_viterbi_h_core(N,M,Goal,VPathL) :-
    ground(Goal),!,
    $pp_init_tables_aux,
    $pp_clean_graph_stats,
    $pp_init_tables_if_necessary,!,
    cputime(T1),
    $pp_find_explanations(Goal),
    cputime(T2),
    $pp_compute_n_viterbi_h(N,M,Goal,VPathL),!,
    cputime(T3),
    $pc_import_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    $pp_assert_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    SearchTime  is T2 - T1,
    NumCompTime is T3 - T2,
    $pp_assert_viterbi_stats2(SearchTime,NumCompTime),!.

$pp_n_viterbi_h_core(N,M,Goal,VPathL) :-
    $pp_init_tables_aux,
    $pp_clean_graph_stats,
    $pp_init_tables_if_necessary,!,
    copy_term(Goal,GoalCp),
    ( $pp_trans_one_goal(GoalCp,CompGoal) -> BodyGoal = CompGoal
    ; BodyGoal = (savecp(CP),Depth=0,
                  $pp_expl_interp_goal(GoalCp,Depth,CP,[],_,[],_,[],_,[],_))
    ),
    $pp_create_dummy_goal(DummyGoal),
    DummyBody = (BodyGoal,
                 $pc_prism_goal_id_register(GoalCp,GId),
                 $pc_prism_goal_id_register(DummyGoal,HId),
                 $prism_eg_path(HId,[GId],[])),
    Prog = [pred(DummyGoal,0,_Mode,_Delay,tabled(_,_,_,_),
                 [(DummyGoal:-DummyBody)])],
    $pp_consult_preds_cond([],Prog),!,
    cputime(T1),
    $pp_find_explanations(DummyGoal),
    cputime(T2),
    $pp_compute_n_viterbi_h(N,M,DummyGoal,VPathL0),!,
    cputime(T3),
    $pp_replace_dummy_goal(Goal,DummyGoal,VPathL0,VPathL),
    $pc_import_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    $pp_assert_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    SearchTime  is T2 - T1,
    NumCompTime is T3 - T2,
    $pp_assert_viterbi_stats2(SearchTime,NumCompTime),!.

$pp_compute_n_viterbi_h(N,M,Goal,VPathL) :-
    $pp_export_sw_info,
    $pc_prism_goal_id_get(Goal,Gid),
    $pc_compute_n_viterbi_rerank(N,M,Gid,VPathL0),
    $pp_build_n_viterbi_path(VPathL0,VPathL),!.

%% Statistics

$pp_assert_viterbi_stats1(InfTime0) :-
    InfTime is InfTime0 / 1000.0,
    assertz($ps_infer_time(InfTime)),!.

$pp_assert_viterbi_stats2(SearchTime0,NumCompTime0) :-
    SearchTime  is SearchTime0  / 1000.0,
    NumCompTime is NumCompTime0 / 1000.0,
    assertz($ps_infer_search_time(SearchTime)),
    assertz($ps_infer_calc_time(NumCompTime)),!.

%%----------------------------------------
%%  e-graph -> tree

viterbi_tree(EG,Tree) :-
    $pp_require_list(EG,$msg(2104),viterbi_tree/2),
    new_hashtable(HT),
    $pp_viterbi_tree(EG,Tree,HT).

$pp_viterbi_tree([],[],_).
$pp_viterbi_tree([Node|Nodes],Tree,HT), Node = node(Name,[]) =>
    Tree = Name,
    $pp_viterbi_tree_register(Name,Tree,HT),!,
    $pp_viterbi_tree(Nodes,_,HT).
$pp_viterbi_tree([Node|Nodes],Tree,HT), Node = node(Name,[path(Gs,Ss)]) =>
    Tree = [Name|L0],
    $pp_viterbi_tree_goals(Gs,L0,L1,HT),
    $pp_viterbi_tree_swits(Ss,L1,[],HT),
    $pp_viterbi_tree_register(Name,Tree,HT),!,
    $pp_viterbi_tree(Nodes,_,HT).

$pp_viterbi_tree_goals([],L,L,_).
$pp_viterbi_tree_goals([G|Gs],[Node|L0],L1,HT) :-
    $pp_viterbi_tree_register(G,Node,HT),!, % Node = free var.
    $pp_viterbi_tree_goals(Gs,L0,L1,HT).

$pp_viterbi_tree_swits([],L,L,_).
$pp_viterbi_tree_swits([S|Ss],[Node|L0],L1,HT) :-
    Node = S,!,
    $pp_viterbi_tree_swits(Ss,L0,L1,HT).

$pp_viterbi_tree_register(Name,Node,HT) :-
    hashtable_get(HT,Name,V),!,
    ( V = Node -> true
    ; $pp_raise_unmatched_branches($pp_viterbi_tree_register/3)
    ).
$pp_viterbi_tree_register(Name,Node,HT) :-
    hashtable_put(HT,Name,Node).

%%----------------------------------------
%%  e-graph -> list of subgoals, list of switches

viterbi_subgoals(VNodes,Goals) :-
    $pp_require_list(VNodes,$msg(2104),viterbi_subgoals/2),
    $pp_viterbi_subgoals(VNodes,Goals).

$pp_viterbi_subgoals([],[]).
$pp_viterbi_subgoals([node(_,[])|Nodes],Ys) :- !,
    $pp_viterbi_subgoals(Nodes,Ys).
$pp_viterbi_subgoals([node(_,[path(Xs,_)])|Nodes],Ys) :-
    append(Xs,Ys1,Ys),!,
    $pp_viterbi_subgoals(Nodes,Ys1).

viterbi_switches(VNodes,Goals) :-
    $pp_require_list(VNodes,$msg(2104),viterbi_switches/2),
    $pp_viterbi_switches(VNodes,Goals).

$pp_viterbi_switches([],[]).
$pp_viterbi_switches([node(_,[])|Nodes],Ys) :- !,
    $pp_viterbi_switches(Nodes,Ys).
$pp_viterbi_switches([node(_,[path(_,Xs)])|Nodes],Ys) :-
    append(Xs,Ys1,Ys),!,
    $pp_viterbi_switches(Nodes,Ys1).
<<<<<<< HEAD
free_energy([G], FreeEnergy, L0, L1, L2) :- 
    !,  free_energy(G, FreeEnergy, L0, L1, L2).
free_energy([G|Gs], FreeEnergy, L0, L1, L2) :- 
    !, 
    free_energy(G, _, _, _, L2_1), 
    free_energy(Gs, F1, L0, L1, L2_2),
    L2 is L2_1 + L2_2,
    FreeEnergy is F1 + L2_1.

free_energy(Goal) :- 
    free_energy(Goal, F, _, _, _), 
    format("Variational Free Energy of ~w is: ~15f~n", [Goal, F]).

free_energy(Goal, FreeEnergy, L0, L1, L2) :-
    $pp_require_tabled_probabilistic_atom(Goal,$msg(0006),prob/2),
    $pp_free_energy(Goal, FreeEnergy, L0, L1, L2).

$pp_free_energy(Goal, FreeEnergy, L0, L1, L2) :-
    $pp_require_ground(Goal,$msg(0101),$pp_n_viterbi_p_core/3),!,
    $pp_init_tables_aux,
    $pp_clean_graph_stats,
    $pp_init_tables_if_necessary,!,
    cputime(T1),
    $pp_find_explanations(Goal),
    cputime(T2),
    $pp_compute_free_energy(Goal,FreeEnergy, L0, L1, L2),!,
    cputime(T3),
    $pc_import_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    $pp_assert_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    SearchTime  is T2 - T1,
    NumCompTime is T3 - T2.

$pp_compute_free_energy(Goal, FreeEnergy, L0, L1, L2) :- 
    Goals = [Goal], 
    $pp_clean_learn_info,
    $pp_trans_goals(Goals,GoalCountPairs,AllGoals),!,
    global_set($pg_observed_facts,GoalCountPairs),
    global_set($pg_num_goals,0),
    $pp_find_explanations(AllGoals),!,
    $pp_export_sw_info, 
    $pp_observed_facts(GoalCountPairs, GidCountPairs, 
                      0, Len, 0, NGoals, -1, FailRootIndex),
    $pc_prism_prepare(GidCountPairs, Len, NGoals, FailRootIndex),
    %% $pc_prism_goal_id_get(Goal, Gid), 
    $pc_compute_free_energy(FreeEnergy, L0, L1, L2), !.

    
    
=======
>>>>>>> 8361929c6ea197ebfdc8dde113757218d9a29cce
%%%%
%%%% Hindsight routine with C interface
%%%%

%%
%% hindsight(G,SubG,HProbs) :-
%%   output hindsight probs of subgoals that matches with SubG given G
%%
%% hindsight(G,SubG) :- print hindsight probs of SubG given G
%%

hindsight(G) :- hindsight(G,_).

hindsight(G,SubG) :-
    hindsight(G,SubG,HProbs),
    ( HProbs == [] -> $pp_raise_warning($msg(1404))
    ; format("hindsight probabilities:~n",[]),
      $pp_print_hindsight_probs(HProbs)
    ).

hindsight(G,SubG,HProbs) :-
    $pp_require_tabled_probabilistic_atom(G,$msg(0006),hindsight/3),
    ( nonvar(SubG) -> $pp_require_callable(SubG,$msg(1403),hindsight/3)
    ; true
    ),
    $pp_clean_infer_stats,
    cputime(T0),
    $pp_hindsight_core(G,SubG,HProbs0),
    $pp_sort_hindsight_probs(HProbs0,HProbs),
    cputime(T1),
    InfTime is T1 - T0,
    $pp_garbage_collect,
    $pp_assert_hindsight_stats1(InfTime),!.

hindsight_agg(G,Agg) :-
    hindsight_agg(G,Agg,HProbs),
    ( HProbs == [] -> $pp_raise_warning($msg(1404))
    ; format("hindsight probabilities:~n",[]),
      $pp_print_hindsight_probs_agg(HProbs)
    ).

hindsight_agg(G,Agg,HProbs) :-
    $pp_require_tabled_probabilistic_atom(G,$msg(0006),hindsight_agg/3),
    $pp_require_hindsight_aggregate_pattern(Agg,$msg(1402),hindsight_agg/3),
    $pp_clean_infer_stats,
    cputime(T0),
    $pp_get_subgoal_from_agg(Agg,SubG),!,
    $pp_hindsight_core(G,SubG,HProbs0),
    $pp_aggregate_hindsight_probs(Agg,HProbs0,HProbs1),
    $pp_sort_hindsight_probs_agg(HProbs1,HProbs),
    cputime(T1),
    InfTime is T1 - T0,
    $pp_garbage_collect,
    $pp_assert_hindsight_stats1(InfTime),!.

$pp_hindsight_core(G,SubG,HProbs) :-
    ground(G),!,
    $pp_init_tables_aux,
    $pp_clean_graph_stats,
    $pp_init_tables_if_necessary,!,
    cputime(T0),
    $pp_find_explanations(G),!,
    cputime(T1),
    $pp_compute_hindsight(G,SubG,HProbs),
    cputime(T2),
    $pc_import_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    $pp_assert_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    SearchTime  is T1 - T0,
    NumCompTime is T2 - T1,
    $pp_assert_hindsight_stats2(SearchTime,NumCompTime),!.

$pp_hindsight_core(G,SubG,HProbs) :-
    $pp_init_tables_aux,
    $pp_clean_graph_stats,
    $pp_init_tables_if_necessary,!,
    copy_term(G,GoalCp),
    ( $pp_trans_one_goal(GoalCp,CompGoal) -> BodyGoal = CompGoal
    ; BodyGoal = (savecp(CP),Depth=0,
                  $pp_expl_interp_goal(GoalCp,Depth,CP,[],_,[],_,[],_,[],_))
    ),
    $pp_create_dummy_goal(DummyGoal),
    Clause = (DummyGoal:-BodyGoal,
                         $pc_prism_goal_id_register(GoalCp,GId),
                         $pc_prism_goal_id_register(DummyGoal,HId),
                         $prism_eg_path(HId,[GId],[])),
    Prog = [pred(DummyGoal,0,_Mode,_Delay,tabled(_,_,_,_),[Clause]),
            pred('$damon_load',0,_,_,_,[('$damon_load':-true)])],
    $pp_consult_preds_cond([],Prog),!,
    cputime(T0),
    $pp_find_explanations(DummyGoal),!,
    cputime(T1),
    $pp_compute_hindsight(DummyGoal,SubG,HProbs),
    cputime(T2),
    $pc_import_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    $pp_assert_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    SearchTime  is T1 - T0,
    NumCompTime is T2 - T1,
    $pp_assert_hindsight_stats2(SearchTime,NumCompTime),!.

% Sws = [sw(Id,Instances,Probs,PseudoCs,Fixed,FixedH),...]
$pp_compute_hindsight(Goal,SubG,HProbs) :-
    $pp_export_sw_info,
    $pc_prism_goal_id_get(Goal,Gid),
    $pc_compute_hindsight(Gid,SubG,0,HProbs0), % "0" indicates "unconditional"
    $pp_decode_hindsight(HProbs0,HProbs),!.

%%
%%  Conditional version of hindsight computation:
%%

chindsight(G) :- chindsight(G,_).

chindsight(G,SubG) :-
    chindsight(G,SubG,HProbs),
    ( HProbs == [] -> $pp_raise_warning($msg(1404))
    ; format("conditional hindsight probabilities:~n",[]),
      $pp_print_hindsight_probs(HProbs)
    ).

chindsight(G,SubG,HProbs) :-
    $pp_require_tabled_probabilistic_atom(G,$msg(0006),chindsight/3),
    ( nonvar(SubG) -> $pp_require_callable(SubG,$msg(1403),chindsight/3)
    ; true
    ),
    $pp_clean_infer_stats,
    cputime(T0),
    $pp_chindsight_core(G,SubG,HProbs0),
    $pp_sort_hindsight_probs(HProbs0,HProbs),
    cputime(T1),
    InfTime is T1 - T0,
    $pp_garbage_collect,
    $pp_assert_hindsight_stats1(InfTime),!.

chindsight_agg(G,Agg) :-
    chindsight_agg(G,Agg,HProbs),
    ( HProbs == [] -> $pp_raise_warning($msg(1404))
    ; format("conditional hindsight probabilities:~n",[]),
      $pp_print_hindsight_probs_agg(HProbs)
    ).

chindsight_agg(G,Agg,HProbs) :-
    $pp_require_tabled_probabilistic_atom(G,$msg(0006),chindsight_agg/3),
    $pp_require_hindsight_aggregate_pattern(Agg,$msg(1402),chindsight_agg/3),
    $pp_clean_infer_stats,
    cputime(T0),
    $pp_get_subgoal_from_agg(Agg,SubG),!,
    $pp_chindsight_core(G,SubG,HProbs0),
    $pp_aggregate_hindsight_probs(Agg,HProbs0,HProbs1),
    $pp_sort_hindsight_probs_agg(HProbs1,HProbs),
    cputime(T1),
    InfTime is T1 - T0,
    $pp_garbage_collect,
    $pp_assert_hindsight_stats1(InfTime),!.

$pp_chindsight_core(G,SubG,HProbs) :-
    ground(G),!,
    $pp_init_tables_aux,
    $pp_clean_graph_stats,
    $pp_init_tables_if_necessary,!,
    cputime(T0),
    $pp_find_explanations(G),!,
    cputime(T1),
    $pp_compute_chindsight(G,SubG,HProbs),
    cputime(T2),
    $pc_import_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    $pp_assert_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    SearchTime  is T1 - T0,
    NumCompTime is T2 - T1,
    $pp_assert_hindsight_stats2(SearchTime,NumCompTime),!.

$pp_chindsight_core(G,SubG,HProbs) :-
    $pp_init_tables_aux,
    $pp_clean_graph_stats,
    $pp_init_tables_if_necessary,!,
    copy_term(G,GoalCp),
    ( $pp_trans_one_goal(GoalCp,CompGoal) -> BodyGoal = CompGoal
    ; BodyGoal = (savecp(CP),Depth=0,
                  $pp_expl_interp_goal(GoalCp,Depth,CP,[],_,[],_,[],_,[],_))
    ),
    $pp_create_dummy_goal(DummyGoal),
    Clause = (DummyGoal:-BodyGoal,
                         $pc_prism_goal_id_register(GoalCp,GId),
                         $pc_prism_goal_id_register(DummyGoal,HId),
                         $prism_eg_path(HId,[GId],[])),
    Prog = [pred(DummyGoal,0,_Mode,_Delay,tabled(_,_,_,_),[Clause]),
            pred('$damon_load',0,_,_,_,[('$damon_load':-true)])],
    $pp_consult_preds_cond([],Prog),!,
    cputime(T0),
    $pp_find_explanations(DummyGoal),!,
    cputime(T1),
    $pp_compute_chindsight(DummyGoal,SubG,HProbs),
    cputime(T2),
    $pc_import_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    $pp_assert_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    SearchTime  is T1 - T0,
    NumCompTime is T2 - T1,
    $pp_assert_hindsight_stats2(SearchTime,NumCompTime),!.

$pp_compute_chindsight(Goal,SubG,HProbs) :-
    $pp_export_sw_info,
    $pc_prism_goal_id_get(Goal,Gid),
    $pc_compute_hindsight(Gid,SubG,1,HProbs0), % "1" indicates "conditional"
    $pp_decode_hindsight(HProbs0,HProbs),!.

$pp_decode_hindsight([],[]).
$pp_decode_hindsight([[Gid,P]|HProbs0],[[G,P]|HProbs]) :-
    $pc_prism_goal_term(Gid,G),!,
    $pp_decode_hindsight(HProbs0,HProbs).

$pp_get_subgoal_from_agg(Agg,SubG) :-
    Agg =.. [F|Args0],
    $pp_get_subgoal_from_agg1(Args0,Args1),
    SubG =.. [F|Args1].

$pp_get_subgoal_from_agg1([],[]).
$pp_get_subgoal_from_agg1([A0|Args0],[A1|Args1]) :-
    ( $pp_is_agg_patt(A0) -> A1 = _
    ; A1 = A0
    ),!,
    $pp_get_subgoal_from_agg1(Args0,Args1).

$pp_is_agg_patt(A) :-
    ( var(A) -> true
    ; member(A,[integer,atom,compound,length,d_length,depth,query,ignore])
    ).

$pp_aggregate_hindsight_probs(Agg,HProbs0,HProbs) :-
    $pp_group_hindsight_probs(Agg,HProbs0,HProbs1),!,
    $pp_aggregate_hindsight_probs1(Agg,HProbs1,HProbs).

$pp_group_hindsight_probs(Agg,HProbs0,HProbs) :-
    $pp_insert_group_patt(Agg,HProbs0,HProbs1),
    $pp_group_hindsight_probs1(HProbs1,HProbs2),
    $pp_delete_group_patt(HProbs2,HProbs).

$pp_insert_group_patt(_,[],[]).
$pp_insert_group_patt(Agg,[[G,P]|HProbs0],[[GPatt,G,P]|HProbs]) :-
    $pp_get_group_patt(Agg,G,GPatt),!,
    $pp_insert_group_patt(Agg,HProbs0,HProbs).

$pp_delete_group_patt([],[]).
$pp_delete_group_patt([Gr0|Groups0],[Gr|Groups]) :-
    $pp_delete_group_patt1(Gr0,Gr),!,
    $pp_delete_group_patt(Groups0,Groups).

$pp_delete_group_patt1([],[]).
$pp_delete_group_patt1([[_GPatt,G,P]|HProbs0],[[G,P]|HProbs]) :- !,
    $pp_delete_group_patt1(HProbs0,HProbs).

$pp_get_group_patt(Agg,G,GPatt) :-
    Agg =.. [F|AggArgs],
    G   =.. [F|Args],
    $pp_get_group_patt_args(AggArgs,Args,GPattArgs),
    GPatt =.. [F|GPattArgs].

$pp_get_group_patt_args([],[],[]).
$pp_get_group_patt_args([AggA|AggArgs],[A|Args],[GPA|GPattArgs]) :-
    ( nonvar(AggA) ->
      ( AggA = integer ->
          ( integer(A) -> GPA = A
          ; $pp_raise_domain_error($msg(1405),[A],[integer,A],
                                   $pp_group_hindsight_probs/3)
          )
      ; AggA = atom ->
          ( atom(A) -> GPA = A
          ; $pp_raise_domain_error($msg(1406),[A],[atom,A],
                                   $pp_group_hindsight_probs/3)
          )
      ; AggA = compound ->
          ( A = [] -> GPA = A
          ; \+ ground(A) ->
                $pp_raise_instanciation_error($msg(1407),[A],
                                              $pp_group_hindsight_probs/3)
          ; compound(A) -> GPA = A
          ; $pp_raise_domain_error($msg(1407),[A],[compound,A],
                                   $pp_group_hindsight_probs/3)
          )
      ; AggA = length ->
          ( (A = [] ; is_list(A)) -> length(A,L), GPA = length-L
          ; $pp_raise_domain_error($msg(1408),[A],[list,A],
                                   $pp_group_hindsight_probs/3)
          )
      ; AggA = d_length ->
          ( A = (D0-D1), is_list(D0), is_list(D1)
              -> length(D0,L0), length(D1,L1), L is L0 - L1, GPA = d_length-L
          ; $pp_raise_domain_error($msg(1409),[A],[d_list,A],
                                   $pp_group_hindsight_probs/3)
          )
      ; AggA = depth  -> $pc_get_term_depth(A,D), GPA = depth-D
      ; AggA = query  -> GPA = *
      ; AggA = ignore -> GPA = *
      ; GPA = A
      )
    ; GPA = *
    ),!,
    $pp_get_group_patt_args(AggArgs,Args,GPattArgs).

$pp_group_hindsight_probs1(HProbs0,HProbs) :-
    $pp_sort_remain_dup(HProbs0,HProbs1),!,
    $pp_group_hindsight_probs2(HProbs1,HProbs).

$pp_group_hindsight_probs2([],[]).
$pp_group_hindsight_probs2([U],[[U]]).
$pp_group_hindsight_probs2([U0|Us0],Us) :- !,
    $pp_group_hindsight_probs2(U0,[U0],Us0,Us).

$pp_group_hindsight_probs2(_,Us,[],[Us]).
$pp_group_hindsight_probs2(U0,Us0,[U1|Us1],Us) :-
    ( U0 = [GPatt,_,_], U1 = [GPatt,_,_] ->
        Us2 = [U1|Us0],!,
        $pp_group_hindsight_probs2(U1,Us2,Us1,Us)
    ; Us = [Us0|Us3],!,
      $pp_group_hindsight_probs2(U1,[U1],Us1,Us3)
    ).

$pp_aggregate_hindsight_probs1(Agg,HProbs0,HProbs) :-
    $pp_replace_agg_patt(Agg,HProbs0,HProbs1),!,
    $pp_aggregate_hindsight_probs2(HProbs1,HProbs).

$pp_replace_agg_patt(_,[],[]).
$pp_replace_agg_patt(Agg,[Gr0|Groups0],[Gr|Groups]) :-
    $pp_replace_agg_patt1(Agg,Gr0,Gr),!,
    $pp_replace_agg_patt(Agg,Groups0,Groups).

$pp_replace_agg_patt1(_,[],[]).
$pp_replace_agg_patt1(Agg,[[G,P]|HProbs0],[[APatt,P]|HProbs]) :-
    $pp_get_agg_patt(Agg,G,APatt),!,
    $pp_replace_agg_patt1(Agg,HProbs0,HProbs).

$pp_get_agg_patt(Agg,G,APatt) :-
    Agg =.. [F|AggArgs],
    G   =.. [F|Args],
    $pp_get_agg_patt_args(AggArgs,Args,APattArgs),
    APatt =.. [F|APattArgs].

$pp_get_agg_patt_args([],[],[]).
$pp_get_agg_patt_args([AggA|AggArgs],[A|Args],[APA|APattArgs]) :-
    ( nonvar(AggA) ->
      ( AggA = integer ->
          ( integer(A) -> APA = A
          ; $pp_raise_domain_error($msg(1405),[A],[integer,A],
                                   $pp_aggregate_hindsight_probs/3)
          )
      ; AggA = atom ->
          ( atom(A) -> APA = A
          ; $pp_raise_domain_error($msg(1406),[A],[atom,A],
                                   $pp_aggregate_hindsight_probs/3)
          )
      ; AggA = compound ->
          ( A = [] -> APA = A
          ; \+ ground(A) ->
                $pp_raise_instanciation_error($msg(1407),[A],
                                              $pp_aggregate_hindsight_probs/3)
          ; compound(A) -> APA = A
          ; $pp_raise_domain_error($msg(1407),[A],[compound,A],
                                   $pp_aggregate_hindsight_probs/3)
          )
      ; AggA = length ->
          ( (A = [] ; is_list(A)) -> length(A,L), APA = 'L'-L
          ; $pp_raise_domain_error($msg(1408),[A],[list,A],
                                   $pp_aggregate_hindsight_probs/3)
          )
      ; AggA = d_length ->
          ( A = (D0-D1), is_list(D0), is_list(D1)
              -> length(D0,L0), length(D1,L1), L is L0 - L1, APA = 'DL'-L
          ; $pp_raise_domain_error($msg(1409),[A],[d_list,A],
                                   $pp_aggregate_hindsight_probs/3)
          )
      ; AggA = depth  -> $pc_get_term_depth(A,D), APA = 'D'-D
      ; AggA = query  -> APA = A
      ; AggA = ignore -> APA = *
      ; APA = A
      )
    ; APA = *
    ),!,
    $pp_get_agg_patt_args(AggArgs,Args,APattArgs).

$pp_aggregate_hindsight_probs2([],[]).
$pp_aggregate_hindsight_probs2([Gr0|Groups0],[Gr|Groups]) :- !,
    $pp_aggregate_hindsight_probs3(Gr0,Gr),!,
    $pp_aggregate_hindsight_probs2(Groups0,Groups).

$pp_aggregate_hindsight_probs3(HProbs0,HProbs) :-
    $pp_sort_remain_dup(HProbs0,HProbs1),
    $pp_aggregate_hindsight_probs4(HProbs1,HProbs).

$pp_aggregate_hindsight_probs4(HProbs0,HProbs) :-
    ( get_prism_flag(log_scale,off) ->
        $pp_aggregate_hindsight_probs5(HProbs0,HProbs)
    ; $pp_aggregate_hindsight_probs5_log(HProbs0,HProbs)
    ).

$pp_aggregate_hindsight_probs5([],[]).
$pp_aggregate_hindsight_probs5([U],[U]).
$pp_aggregate_hindsight_probs5([[APatt,P]|Us0],Us) :- !,
    $pp_aggregate_hindsight_probs5(APatt,P,Us0,Us).

$pp_aggregate_hindsight_probs5(APatt,P,[],[[APatt,P]]).
$pp_aggregate_hindsight_probs5(APatt,P0,[[APatt1,P1]|Us1],Us) :-
    ( APatt = APatt1 ->
        P2 is P0 + P1,!,
        $pp_aggregate_hindsight_probs5(APatt,P2,Us1,Us)
    ; Us = [[APatt,P0]|Us2],!,
        $pp_aggregate_hindsight_probs5(APatt1,P1,Us1,Us2)
    ).

% log-scale computation for tiny probabilities
$pp_aggregate_hindsight_probs5_log([],[]).
$pp_aggregate_hindsight_probs5_log([U],[U]).
$pp_aggregate_hindsight_probs5_log([[APatt,P]|Us0],Us) :-
    $pp_aggregate_hindsight_probs5_log(APatt,P,1.0,Us0,Us).

$pp_aggregate_hindsight_probs5_log(APatt,P0,Q,[],[[APatt,P]]) :-
    P is P0 + log(Q),!.
$pp_aggregate_hindsight_probs5_log(APatt,P0,Q0,[[APatt1,P1]|Us1],Us) :-
    ( APatt = APatt1 ->
      ( P1 < -4096.0 ->           % P1 == -Inf, i.e. exp(P1) == 0
          Q is Q0,                % Note: exp(-4096) << Double.MIN_VALUE
          P2 = P0
      ; P0 < -4096.0 ->           % P0 == -Inf, i.e. exp(P0) == 0
          Q is 1.0,
          P2 = P1
      ; P1 - P0 > log(1.0e+280) ->
          Q is Q0 * exp(P0 - P1) + 1.0,
          P2 = P1
      ; Q is Q0 + exp(P1 - P0),
        P2 = P0
      ),!,
      $pp_aggregate_hindsight_probs5_log(APatt,P2,Q,Us1,Us)
    ; P is P0 + log(Q0),
      Us = [[APatt,P]|Us2],!,
      $pp_aggregate_hindsight_probs5_log(APatt1,P1,1.0,Us1,Us2)
    ).

$pp_sum_log_list([],0.0) :- !.
$pp_sum_log_list([LP],LP) :- !.
$pp_sum_log_list([LP|LPs],Sum) :-
    $pp_sum_log_list(LPs,LP,1.0,SumRest),!,
    Sum is LP + log(SumRest).

$pp_sum_log_list([],_,SumRest,SumRest).
$pp_sum_log_list([LP|LPs],FirstLP,SumRest0,SumRest) :-
    SumRest1 is SumRest0 + exp(LP - FirstLP),!,
    $pp_sum_log_list(LPs,FirstLP,SumRest1,SumRest).

%%%%
%%%% Sort hindsight proabilities
%%%%

$pp_sort_hindsight_probs(HProbs0,HProbs) :-
    ( get_prism_flag(sort_hindsight,by_goal) ->
        $pp_sort_remain_dup(HProbs0,HProbs)
    ; $pp_sort_hindsight_probs_by_prob(HProbs0,HProbs)
    ).

$pp_sort_hindsight_probs_by_prob(HProbs0,HProbs) :-
    $pp_swap_hindsight_pair(HProbs0,HProbs1),
    $pp_sort_remain_dup(HProbs1,HProbs2),
    reverse(HProbs2,HProbs3),
    $pp_swap_hindsight_pair(HProbs3,HProbs).

$pp_swap_hindsight_pair([],[]) :- !.
$pp_swap_hindsight_pair([[X,Y]|XYs],[[Y,X]|YXs]) :- !,
    $pp_swap_hindsight_pair(XYs,YXs).

$pp_sort_hindsight_probs_agg([],[]) :- !.
$pp_sort_hindsight_probs_agg([Gr0|Groups0],[Gr|Groups]) :-
    $pp_sort_hindsight_probs(Gr0,Gr),!,
    $pp_sort_hindsight_probs_agg(Groups0,Groups).

%%%%
%%%% Print hindsight probabilities
%%%%

$pp_print_hindsight_probs([]).
$pp_print_hindsight_probs([[G,P]|HProbs]) :-
    format("  ~w: ~15f~n",[G,P]),!,
    $pp_print_hindsight_probs(HProbs).

$pp_print_hindsight_probs_agg([]).
$pp_print_hindsight_probs_agg([Gr|Groups]) :-
    $pp_print_hindsight_probs(Gr),!,
    $pp_print_hindsight_probs_agg(Groups).

%%%% Statistics

$pp_assert_hindsight_stats1(InfTime0) :-
    InfTime is InfTime0 / 1000.0,
    assertz($ps_infer_time(InfTime)),!.

$pp_assert_hindsight_stats2(SearchTime0,NumCompTime0) :-
    SearchTime  is SearchTime0  / 1000.0,
    NumCompTime is NumCompTime0 / 1000.0,
    assertz($ps_infer_search_time(SearchTime)),
    assertz($ps_infer_calc_time(NumCompTime)),!.
%%
%%  expl.pl: routines for explanation search
%%
%%  $pp_find_explanations(Goals) constructs the explanation graphs for Goals.
%%  An explanation graph is a directed hype-graph where each hype-arc takes
%%  the form of:
%%
%%  $prism_eg_path(GoalId,Children,SWs)
%%  
%%  where 
%%    GoalId: 
%%         the id of the source node (all variant subgoals have the same ID)
%%    Children:
%%         the list of nodes that are connected by the hype-arc with GoalID
%%    SWs:
%%         the list of switches associated with the arc.
%%  
%%  consider the following PRISM program:
%%
%%    values(init,[s0,s1]).
%%    values(out(_),[a,b]).
%%    values(tr(_),[s0,s1]).
%%
%%    hmm(L) :- 
%%        msw(init,Si),
%%        hmm(1,Si,L).
%%
%%    hmm(T,S,[]) :- T>3.
%%    hmm(T,S,[C|L]) :-
%%        T=<3,
%%        msw(out(S),C),
%%        msw(tr(S),NextS),
%%        T1 is T + 1,
%%        hmm(T1,NextS,L).
%%
%%
%%  The relations for the goal hmm([a,b,a]) are as follows (where goals
%%  rather than their ids are shown for description purpose):
%%
%%    goal_id(hmm([a,b,a]),0),
%%    goal_id(hmm(1,s0,[a,b,a]),1)
%%    goal_id(hmm(2,s0,[b,a]),4)]
%%    goal_id(hmm(2,s1,[b,a]),11)
%%    goal_id(hmm(3,s0,[a]),7)
%%    goal_id(hmm(3,s1,[a]),9)
%%    goal_id(hmm(3,s2,[a]),14)
%%    goal_id(observe(1,s0,a),2)
%%    goal_id(observe(2,s0,b),5)
%%    goal_id(observe(2,s1,b),12)
%%    goal_id(observe(3,s0,a),8)
%%    goal_id(observe(3,s1,a),10)
%%    goal_id(observe(3,s2,a),15)
%%    goal_id(trans(1,s0,_5b0400),3)
%%    goal_id(trans(2,s0,_5b0480),6)
%%    goal_id(trans(2,s1,_5b04f0),13)
%%
%%    $prism_eg_path(3,[],[msw(trans(s0),1,s0)]),
%%    $prism_eg_path(6,[],[msw(trans(s0),2,s0)]),
%%    $prism_eg_path(12,[],[msw(obs(s1),2,b)]),
%%    $prism_eg_path(3,[],[msw(trans(s0),1,s1)]),
%%    $prism_eg_path(6,[],[msw(trans(s0),2,s1)]),
%%    $prism_eg_path(13,[],[msw(trans(s1),2,s1)]),
%%    $prism_eg_path(0,[1],[]),
%%    $prism_eg_path(7,[8],[]),
%%    $prism_eg_path(1,[4,3,2],[]),
%%    $prism_eg_path(13,[],[msw(trans(s1),2,s2)]),
%%    $prism_eg_path(4,[7,6,5],[]),
%%    $prism_eg_path(2,[],[msw(obs(s0),1,a)]),
%%    $prism_eg_path(8,[],[msw(obs(s0),3,a)]),
%%    $prism_eg_path(5,[],[msw(obs(s0),2,b)])]
%%
%%  One of the explanations for hmm([a,b,a]) is:
%%    
%%    [msw(init,once,s0),msw(out(s0),1,a),msw(tr(s0),1,s0),msw(out(s0),2,b),...]
%%

$pp_find_explanations(Goals) :-
    $pp_expl_goals_all(Goals).

$pp_expl_failure :-    
    $pp_trans_one_goal(failure,CompGoal),!,
    call(CompGoal).
$pp_expl_failure :-
    savecp(CP),
    Depth = 0,
    $pp_expl_interp_goal(failure,Depth,CP,[],_,[],_,[],_,[],_).
    
$pp_expl_goals_all(Goals) :-
    $pp_expl_goals(Goals).

$pp_expl_goals([]) => true.
$pp_expl_goals([Goal|Goals]) =>
    $pp_learn_message(MsgS,_,_,_),
    $pp_print_goal_message(MsgS),
    ( $pp_expl_one_goal(Goal) -> true
    ; $pp_raise_runtime_error($msg(1304),[Goal],explanation_not_found,
                              $pp_find_explanations/1)
    ), !,
    $pp_expl_goals(Goals).
$pp_expl_goals(Goal) =>
    $pp_expl_one_goal(Goal).

$pp_expl_one_goal(msw(Sw,V)) :- !,
    $prism_expl_msw(Sw,V,_Id).
$pp_expl_one_goal(failure) :- !,
    $pp_expl_failure.
$pp_expl_one_goal(Goal) :-
    $pp_is_dummy_goal(Goal),!,
    call(Goal).
$pp_expl_one_goal(Goal) :-
    % FIXME: handling non-tabled probabilistic predicate is future work
    $pp_require_tabled_probabilistic_atom(Goal,$msg(0006),$pp_expl_one_goal/1),
    ( ground(Goal) -> GoalCp = Goal
    ; copy_term(Goal,GoalCp)
    ),
    ( $pp_trans_one_goal(GoalCp,CompGoal) ->
        call(CompGoal)
    ; savecp(CP),
      Depth = 0,
      $pp_expl_interp_goal(GoalCp,Depth,CP,[],_,[],_,[],_,[],_)
    ).

% [Note] this predicate fails if Goal is not probabilistic
$pp_trans_one_goal(Goal,CompGoal) :-
    functor(Goal,F,N),
    name(F,FString),
    append("$pu_expl_",FString,NewFString),
    name(NewF,NewFString),
    N1 is N + 1,
    current_predicate(NewF/N1),!,
    Goal =.. [_|Args],
    CompGoal =.. [NewF,_|Args].

%%----------------------------------------------------------------------------

$pp_expl_interp_goal('!',_Depth,CP,
                     CIDs0,CIDs,SWs0,SWs,
                     SimCIDs0,SimCIDs,SimSWs0,SimSWs) =>
    cutto(CP),
    CIDs    = CIDs0,
    SWs     = SWs0,
    SimCIDs = SimCIDs0,
    SimSWs  = SimSWs0.
$pp_expl_interp_goal('_$savecp'(X),_Depth,_CP,
                     CIDs0,CIDs,SWs0,SWs,
                     SimCIDs0,SimCIDs,SimSWs0,SimSWs) =>
    savecp(X),
    CIDs    = CIDs0,
    SWs     = SWs0,
    SimCIDs = SimCIDs0,
    SimSWs  = SimSWs0.
$pp_expl_interp_goal('_$savepcp'(X),_Depth,_CP,
                     CIDs0,CIDs,SWs0,SWs,
                     SimCIDs0,SimCIDs,SimSWs0,SimSWs) =>
    '_$savepcp'(X),
    CIDs    = CIDs0,
    SWs     = SWs0,
    SimCIDs = SimCIDs0,
    SimSWs  = SimSWs0.
$pp_expl_interp_goal('_$cutto'(X),_Depth,_CP,
                     CIDs0,CIDs,SWs0,SWs,
                     SimCIDs0,SimCIDs,SimSWs0,SimSWs) =>
    cutto(X),
    CIDs    = CIDs0,
    SWs     = SWs0,
    SimCIDs = SimCIDs0,
    SimSWs  = SimSWs0.
$pp_expl_interp_goal('_$initialize_var'(_Vars),_Depth,_CP,
                     CIDs0,CIDs,SWs0,SWs,
                     SimCIDs0,SimCIDs,SimSWs0,SimSWs) =>
    CIDs    = CIDs0,
    SWs     = SWs0,
    SimCIDs = SimCIDs0,
    SimSWs  = SimSWs0.
$pp_expl_interp_goal(Goal,Depth,_CP,
                     CIDs0,CIDs,SWs0,SWs,
                     SimCIDs0,SimCIDs,SimSWs0,SimSWs), Goal = msw(I,V) =>
    CIDs    = CIDs0,
    SWs     = [SwId|SWs0],
    SimCIDs = SimCIDs0,
    SimSWs  = [Goal|SimSWs0],
    c_SAVE_AR(AR),
    c_next_global_call_number(CallNo),
    $eval_and_monitor_call($prism_expl_msw(I,V,SwId),Depth,CallNo,AR).
$pp_expl_interp_goal((G1,G2),Depth,CP,
                     CIDs0,CIDs,SWs0,SWs,
                     SimCIDs0,SimCIDs,SimSWs0,SimSWs) =>
    $pp_expl_interp_goal(G1,Depth,CP,
                         CIDs0,CIDs1,SWs0,SWs1,
                         SimCIDs0,SimCIDs1,SimSWs0,SimSWs1),
    $pp_expl_interp_goal(G2,Depth,CP,
                         CIDs1,CIDs,SWs1,SWs,
                         SimCIDs1,SimCIDs,SimSWs1,SimSWs).
$pp_expl_interp_goal((C->A;B),Depth,CP,
                     CIDs0,CIDs,SWs0,SWs,
                     SimCIDs0,SimCIDs,SimSWs0,SimSWs) =>
    '_$savecp'(NewCP),
    ( eval_debug_call(C,Depth,NewCP) ->
        $pp_expl_interp_goal(A,Depth,CP,
                             CIDs0,CIDs,SWs0,SWs,
                             SimCIDs0,SimCIDs,SimSWs0,SimSWs)
    ; $pp_expl_interp_goal(B,Depth,CP,
                           CIDs0,CIDs,SWs0,SWs,
                           SimCIDs0,SimCIDs,SimSWs0,SimSWs)
    ).
$pp_expl_interp_goal((C->A),Depth,CP,
                     CIDs0,CIDs,SWs0,SWs,
                     SimCIDs0,SimCIDs,SimSWs0,SimSWs) =>
    '_$savecp'(NewCP),
    ( eval_debug_call(C,Depth,NewCP) ->
        $pp_expl_interp_goal(A,Depth,CP,
                             CIDs0,CIDs,SWs0,SWs,
                             SimCIDs0,SimCIDs,SimSWs0,SimSWs)
    ).
$pp_expl_interp_goal((A;B),Depth,CP,
                     CIDs0,CIDs,SWs0,SWs,
                     SimCIDs0,SimCIDs,SimSWs0,SimSWs) =>
     ( $pp_expl_interp_goal(A,Depth,CP,
                            CIDs0,CIDs,SWs0,SWs,
                            SimCIDs0,SimCIDs,SimSWs0,SimSWs)
     ; $pp_expl_interp_goal(B,Depth,CP,
                            CIDs0,CIDs,SWs0,SWs,
                            SimCIDs0,SimCIDs,SimSWs0,SimSWs)
     ).
$pp_expl_interp_goal(not(A),Depth,_CP,
                     CIDs0,CIDs,SWs0,SWs,
                     SimCIDs0,SimCIDs,SimSWs0,SimSWs) =>
    '_$savecp'(NewCP),
    ( $pp_expl_interp_goal(A,Depth,NewCP,
                           CIDs0,CIDs,SWs0,SWs,
                           SimCIDs0,SimCIDs,SimSWs0,SimSWs) -> fail
    ; CIDs    = CIDs0,
      SWs     = SWs0,
      SimCIDs = SimCIDs0,
      SimSWs  = SimSWs0
    ).
$pp_expl_interp_goal((\+ A),Depth,_CP,
                     CIDs0,CIDs,SWs0,SWs,
                     SimCIDs0,SimCIDs,SimSWs0,SimSWs) =>
    '_$savecp'(NewCP),
    ( $pp_expl_interp_goal(A,Depth,NewCP,
                           CIDs0,CIDs,SWs0,SWs,
                           SimCIDs0,SimCIDs,SimSWs0,SimSWs) -> fail
    ; CIDs    = CIDs0,
      SWs     = SWs0,
      SimCIDs = SimCIDs0,
      SimSWs  = SimSWs0
    ).
$pp_expl_interp_goal('_$if_then_else'(C,A,B),Depth,CP,
                     CIDs0,CIDs,SWs0,SWs,
                     SimCIDs0,SimCIDs,SimSWs0,SimSWs) =>
    '_$savecp'(NewCP),
    ( eval_debug_call(C,Depth,NewCP) ->
        $pp_expl_interp_goal(A,Depth,CP,
                             CIDs0,CIDs,SWs0,SWs,
                             SimCIDs0,SimCIDs,SimSWs0,SimSWs)
    ; $pp_expl_interp_goal(B,Depth,CP,
                           CIDs0,CIDs,SWs0,SWs,
                           SimCIDs0,SimCIDs,SimSWs0,SimSWs)
    ).
$pp_expl_interp_goal(write_call(A),Depth,CP,
                     CIDs0,CIDs,SWs0,SWs,
                     SimCIDs0,SimCIDs,SimSWs0,SimSWs) =>
    $pp_expl_interp_goal(write_call([],A),Depth,CP,
                         CIDs0,CIDs,SWs0,SWs,
                         SimCIDs0,SimCIDs,SimSWs0,SimSWs).
$pp_expl_interp_goal(write_call(Opts,A),Depth,CP,
                     CIDs0,CIDs,SWs0,SWs,
                     SimCIDs0,SimCIDs,SimSWs0,SimSWs) =>
    B = $pp_expl_interp_goal(A,Depth,CP,
                             CIDs0,CIDs,SWs0,SWs,
                             SimCIDs0,SimCIDs,SimSWs0,SimSWs),
    $pp_write_call_core(Opts,A,B).
$pp_expl_interp_goal((?? A),Depth,CP,
                     CIDs0,CIDs,SWs0,SWs,
                     SimCIDs0,SimCIDs,SimSWs0,SimSWs) =>
    $pp_expl_interp_goal(write_call([],A),Depth,CP,
                         CIDs0,CIDs,SWs0,SWs,
                         SimCIDs0,SimCIDs,SimSWs0,SimSWs).
$pp_expl_interp_goal((??* A),Depth,CP,
                     CIDs0,CIDs,SWs0,SWs,
                     SimCIDs0,SimCIDs,SimSWs0,SimSWs) =>
    $pp_expl_interp_goal(write_call([all],A),Depth,CP,
                         CIDs0,CIDs,SWs0,SWs,
                         SimCIDs0,SimCIDs,SimSWs0,SimSWs).
$pp_expl_interp_goal((??> A),Depth,CP,
                     CIDs0,CIDs,SWs0,SWs,
                     SimCIDs0,SimCIDs,SimSWs0,SimSWs) =>
    $pp_expl_interp_goal(write_call([call],A),Depth,CP,
                         CIDs0,CIDs,SWs0,SWs,
                         SimCIDs0,SimCIDs,SimSWs0,SimSWs).
$pp_expl_interp_goal((??< A),Depth,CP,
                     CIDs0,CIDs,SWs0,SWs,
                     SimCIDs0,SimCIDs,SimSWs0,SimSWs) =>
    $pp_expl_interp_goal(write_call([exit+fail],A),Depth,CP,
                         CIDs0,CIDs,SWs0,SWs,
                         SimCIDs0,SimCIDs,SimSWs0,SimSWs).
$pp_expl_interp_goal((??+ A),Depth,CP,
                     CIDs0,CIDs,SWs0,SWs,
                     SimCIDs0,SimCIDs,SimSWs0,SimSWs) =>
    $pp_expl_interp_goal(write_call([exit],A),Depth,CP,
                         CIDs0,CIDs,SWs0,SWs,
                         SimCIDs0,SimCIDs,SimSWs0,SimSWs).
$pp_expl_interp_goal((??- A),Depth,CP,
                     CIDs0,CIDs,SWs0,SWs,
                     SimCIDs0,SimCIDs,SimSWs0,SimSWs) =>
    $pp_expl_interp_goal(write_call([fail],A),Depth,CP,
                         CIDs0,CIDs,SWs0,SWs,
                         SimCIDs0,SimCIDs,SimSWs0,SimSWs).
$pp_expl_interp_goal(Goal,Depth,_CP,
                     CIDs0,CIDs,SWs0,SWs,
                     SimCIDs0,SimCIDs,SimSWs0,SimSWs) :-
    functor(Goal,F,N),
    $pd_is_prob_pred(F,N),!,
    CIDs    = [Gid|CIDs0],
    SWs     = SWs0,
    SimCIDs = [Goal|SimCIDs0],
    SimSWs  = SimSWs0,
    c_SAVE_AR(AR),
    c_next_global_call_number(CallNo),
    $expl_interp_and_monitor_prob_goal(Goal,Depth,Gid,CallNo,AR).
$pp_expl_interp_goal(Goal,Depth,CP,
                     CIDs0,CIDs,SWs0,SWs,
                     SimCIDs0,SimCIDs,SimSWs0,SimSWs) :-
    CIDs    = CIDs0,
    SWs     = SWs0,
    SimCIDs = SimCIDs0,
    SimSWs  = SimSWs0,
    ( c_is_debug_mode ->
        eval_debug_call(Goal,Depth,CP)
    ; eval_call(Goal,CP)
    ).

%%----------------------------------------------------------------------------

$expl_interp_and_monitor_prob_goal(Call,Depth,Gid,CallNo,AR) ?=>
    c_get_dg_flag(Flag),
    $print_call(Flag,'   Call: ',Call,Depth,CallNo,AR),
    Depth1 is Depth + 1,
    $expl_interp_single_call(Call,Depth1,Gid),
    $switch_skip_off(AR),
    $eval_call_exit(Call,Depth,CallNo,AR).
$expl_interp_and_monitor_prob_goal(Call,Depth,_Gid,CallNo,AR) =>
    c_get_dg_flag(Flag),
    $print_call(Flag,'   Fail: ',Call,Depth,CallNo,AR),
    fail.

$expl_interp_single_call(Goal,Depth,Gid) :- % suppress re-computation
    savecp(CP1),
    clause(Goal,Body), 
    $pp_expl_interp_goal(Body,Depth,CP1,
                         [],BodyCIDs,[],BodySWs,
                         [],SimCIDs,[],SimSWs), 
                     % BodyCIDs is a list of children in Body
                     % BodySWs is a list of switches in Body
    $pc_prism_goal_id_register(Goal,Gid),
    ( (BodyCIDs == [], BodySWs == []) -> true
    ; c_get_dg_flag(Flag),
      c_next_global_call_number(CallNo),
      $print_call(Flag,'   Add: ',path(Goal,SimCIDs,SimSWs),Depth,CallNo,0),
      $prism_eg_path(Gid,BodyCIDs,BodySWs)
    ).

%%----------------------------------------------------------------------------

$prism_eg_path(Pid,CIDs,SWs) :- $pc_add_egraph_path(Pid,CIDs,SWs).

$prism_expl_msw(Sw,V,SwInsId) :-
    get_values1(Sw,Values),
    ( $pc_prism_sw_id_get(Sw,SwId) -> true
    ; $pc_prism_sw_id_register(Sw,SwId),
      $pp_export_switch(SwId,Sw,Values)
    ),!,
    member(V,Values),
    $pc_prism_sw_ins_id_get(msw(Sw,V),SwInsId).

%%----------------------------------------------------------------------------

$pp_export_switch(SwId,Sw,Values) :-
    $pp_encode_switch_instances(Sw,Values,SwInsIds),
    $pc_export_switch(SwId,SwInsIds).
    
$pp_encode_switch_instances(_Sw,[],[]).
$pp_encode_switch_instances(Sw,[V|Vs],[Id|Ids]) :-
    $pc_prism_sw_ins_id_register(msw(Sw,V),Id),!,
    $pp_encode_switch_instances(Sw,Vs,Ids).

%%----------------------------------------------------------------------------

$pp_print_goal_message(MsgS) :-
    MsgS > 0,
    get_prism_flag(search_progress,Ival),
    Ival > 0, !,
    global_get($pg_num_goals,N),
    ( N =:= 0 ->
        format("#goals: 0",[]),flush_output,
        N1 is N + 1,
        global_set($pg_num_goals,N1)
    ; N > 0 ->
        ( N mod (Ival * 10) =:= 0 -> format("~w",[N]),flush_output
        ; N mod Ival =:= 0 -> format(".",[]),flush_output
        ; true
        ),
        N1 is N + 1,
        global_set($pg_num_goals,N1)
    ; true
    ).
$pp_print_goal_message(_).
%%
%% sample.pl: routines for sampling execution
%%
%% <ex.>
%%     | ?- sample(bloodtype(X)).
%%  
%%     X = a ?
%%
%% Also available for Utility program.
%% <ex.>
%% go(Loc,Dir) :-
%%     ( is_wall(forward,Loc),
%%       sample(coin(X)),
%%       ( X = head,!,Dir = right
%%       ; Dir = left
%%       )
%%     ; Dir = forward
%%     ).

sample(Goal) :-
    $pp_require_probabilistic_atom(Goal,$msg(1201),sample/1),
    $trace_call(Goal).   % just calls call(Goal) if not in debug mode

%%----------------------------------------------------------------------------

msw(Sw,V) :-
    $pp_require_ground(Sw,$msg(0101),msw/2),
    $prism_sample_msw(Sw,V).

% Sw is assumed to be ground in $prism_sample_msw/{2,5}.

$prism_sample_msw(Sw,V) :-
    $pp_get_parameters(Sw,Values,Pbs),!,
    sumlist(Pbs,Sum),
    random_uniform(Sum,R),
    $pp_choose(Pbs,R,Values,V,_P).

$prism_sample_msw(Sw,V,Depth,_CP,CallNo,AR) :-
    $pp_get_parameters(Sw,Values,Pbs),!,
    c_get_dg_flag(Flag),
    $print_call(Flag,'   Call: ',(msw(Sw,V):P),Depth,CallNo,AR),
    sumlist(Pbs,Sum),
    random_uniform(Sum,R),
    ( $pp_choose(Pbs,R,Values,V,P) ->
          $print_call(Flag,'   Exit: ',(msw(Sw,V):P),Depth,CallNo,AR)
    ; $print_call(Flag,'   Fail: ',msw(Sw,V),Depth,CallNo,AR),
      fail
    ).

$pp_choose(Pbs,R,Vs,X,P) :- $pp_choose(0,Pbs,R,Vs,X,P).
$pp_choose(CPb,[Pb|Pbs],R,[V|Vs],X,P) :-
    CPb1 is CPb+Pb,
    ( R < CPb1 -> X = V, P = Pb
    ; Pbs = [] -> X = V, P = Pb
    ; $pp_choose(CPb1,Pbs,R,Vs,X,P)
    ).

%%----------------------------------------
%%  backtrackable msw (the original definition provided by Jon Sneyers)

soft_msw(Sw,V) :-
    $pp_require_ground(Sw,$msg(0101),msw/2),
    $prism_sample_soft_msw(Sw,V).

b_msw(Sw,V) :- soft_msw(Sw,V).

$prism_sample_soft_msw(Sw,Val) :-
    $pp_get_parameters(Sw,Values,Pbs),!,
    $pp_zip_vp(Values,Pbs,Candidates),
    $pp_soft_choose(Candidates,Val).

% value-prob pairs
$pp_zip_vp([],[],[]).
$pp_zip_vp([Val|Vals],[Prob|Probs],[Val-Prob|Rest]) :- !,
    $pp_zip_vp(Vals,Probs,Rest).

$pp_soft_choose([],_V) :- !, fail.
$pp_soft_choose(Candidates,V) :-
    $pp_zip_vp(Vals,Probs,Candidates),
    sumlist(Probs,Sum),
    Sum > 0,
    random_uniform(Sum,R),
    $pp_choose(Probs,R,Vals,Val,Prob),
    delete(Candidates,Val-Prob,OtherOptions),
    (V=Val ; $pp_soft_choose(OtherOptions,V)).

%%----------------------------------------
%%  sampling utils

get_samples(N,G,Gs) :-           % G assumed to never fail
    $pp_require_positive_integer(N,$msg(1203),get_samples/3),
    $pp_require_probabilistic_atom(G,$msg(1201),get_samples/3),	
    $pp_get_samples(0,N,G,Gs).

$pp_get_samples(N,N,_,[]) :- !.
$pp_get_samples(N0,N,G,[G1|Gs]) :-
    copy_term(G,G1),!,
    sample(G1),
    N1 is N0 + 1,!,
    $pp_get_samples(N1,N,G,Gs).

get_samples_c(N,G,Gs) :- get_samples_c(N,G,true,Gs).

get_samples_c(N,G,C,Gs) :-
    get_samples_c(N,G,C,Gs,[NS,NF]),
    format("sampling -- #success = ~w~n",[NS]), 
    format("sampling -- #failure = ~w~n",[NF]).

get_samples_c(PairN,PairG,C,Gs,[NS,NF]) :-
    ( [N,M] = PairN -> true ; N = PairN, M = PairN ),
    ( [S,G] = PairG -> true ; S = PairG, G = PairG ),
    $pp_require_positive_integer_or_infinity(N,$msg(1204),get_samples_c/5),
    $pp_require_positive_integer(M,$msg(1203),get_samples_c/5),
    $pp_require_probabilistic_atom(S,$msg(1201),get_samples_c/5),
    $pp_require_callable(C,$msg(1202),get_samples_c/5),
    $pp_get_samples_c(0,N,M,S,G,C,Gs,0,NS,0,NF).

$pp_get_samples_c(N,N,_ ,_,_,_,[],NS,NS,NF,NF) :- !.
$pp_get_samples_c(_,_,NS,_,_,_,[],NS,NS,NF,NF) :- !.

$pp_get_samples_c(N0,N,M,S,G,C,Gs,NS0,NS,NF0,NF) :-
    copy_term([S,G,C],[S1,G1,C1]),!,
    ( sample(S1),!,call(C1) ->
        Gs = [G1|Gs1], NS1 is NS0 + 1, NF1 is NF0
    ;   Gs = Gs1,      NS1 is NS0,     NF1 is NF0 + 1
    ),
    N1 is N0 + 1,!,
    $pp_get_samples_c(N1,N,M,S,G,C,Gs1,NS1,NS,NF1,NF).

%%----------------------------------------

$pp_require_positive_integer_or_infinity(X,MsgID,Source) :-
    ( ( X == inf ; integer(X), X > 0 ) ->
      true
    ; $pp_raise_on_require([X],MsgID,Source,$pp_error_positive_integer_or_infinity)
    ).

$pp_error_positive_integer_or_infinity(X,Error) :-
    X \== inf,
    ( $pp_error_integer(X,Error)
    ; X =< 0 -> Error = domain_error(infinity_or_greater_than_zero,X)
    ).
%%----------------------------------------

expand_probs(Dist,Probs) :-
    $pp_expand_probs(Dist,Probs,expand_probs/2).

expand_probs(Dist,N,Probs) :-
    $pp_expand_probs(Dist,N,Probs,expand_probs/3).

$pp_expand_probs(Dist,Probs,Source) :-
    $pp_require_fixed_size_distribution(Dist,$msg(0200),Source),
    $pp_spec_to_ratio(Dist,_,Ratio,Source),
    $pp_normalize_ratio(Ratio,Probs).

$pp_expand_probs(Dist,N,Probs,Source) :-
    $pp_require_distribution(Dist,$msg(0200),Source),
    $pp_require_positive_integer(N,$msg(0204),Source),
    $pp_spec_to_ratio(Dist,N,Ratio,Source),
    $pp_check_expanded_prob_size(Ratio,N,Source),
    $pp_normalize_ratio(Ratio,Probs).

$pp_normalize_ratio(Ratio,Probs) :-
    sumlist(Ratio,Denom),
    $pp_ratio_to_probs(Ratio,Denom,Probs).

$pp_ratio_to_probs([],_,[]) :- !.
$pp_ratio_to_probs([X|Xs],Denom,[Y|Ys]) :-
    get_prism_flag(error_on_invalid_distribution,F),
    (F=on->(Y is X / Denom);(Y is X)),!,
    $pp_ratio_to_probs(Xs,Denom,Ys).

$pp_check_expanded_prob_size(List,N,Source) :-
    length(List,N1),
    ( N = N1 -> true
    ; $pp_raise_runtime_error($msg(0211),[List,N],unmatched_distribution,
                              Source)
    ),!.

%%----------------------------------------

$pp_spec_to_ratio(Dist,N,Ratio,Source) :-
    ( Dist = default,
      get_prism_flag(default_sw,none)
          -> $pp_raise_runtime_error($msg(0202),
                                     default_distribution_unavailable,
                                     Source)
    ; true
    ),
    $pp_spec_to_ratio1(Dist,N,Ratio,Source).

$pp_spec_to_ratio1(Dist,_N,Ps,_Source), Dist = [_|_] => Ps = Dist.

$pp_spec_to_ratio1(Dist,_N,Ps,_Source), Dist = (_+_) =>
    $pp_expr_to_list('+',Dist,Ps).

$pp_spec_to_ratio1(Dist,_N,Ratio,_Source), Dist = (_:_) =>
    $pp_expr_to_list(':',Dist,Ratio).

$pp_spec_to_ratio1(uniform,N,Ratio,_Source) =>
    $pp_gen_geom_list(N,1,1,Ratio).

$pp_spec_to_ratio1(f_geometric,N,Ratio,_Source) =>
    $pp_spec_to_ratio_fgeom(2,desc,N,Ratio).

$pp_spec_to_ratio1(f_geometric(Base),N,Ratio,_Source) =>
    $pp_spec_to_ratio_fgeom(Base,desc,N,Ratio).

$pp_spec_to_ratio1(f_geometric(Base,Type),N,Ratio,_Source) =>
    $pp_spec_to_ratio_fgeom(Base,Type,N,Ratio).

$pp_spec_to_ratio1(random,N,Ratio,_Source) =>
    $pp_gen_rand_list(N,Ratio).

$pp_spec_to_ratio1(noisy_u,N,Ratio,_Source) =>
    $pp_gen_noisy_u_list(N,Ratio).

$pp_spec_to_ratio1(default,N,Ratio,Source) =>
    get_prism_flag(default_sw,Flag),
    $pp_require_distribution(Flag,$msg(0200),Source),!,
    $pp_spec_to_ratio1(Flag,N,Ratio,Source).

%%----------------------------------------

expand_pseudo_counts(Spec,Cs) :-
    $pp_require_fixed_size_hyperparameters(Spec,$msg(0201),
                                           expand_pseudo_counts/2),
    $pp_expand_pseudo_counts(Spec,_,Cs,expand_pseudo_counts/2).

expand_pseudo_counts(Spec,N,Cs) :-
    Source = expand_pseudo_counts/3,
    $pp_require_hyperparameters(Spec,$msg(0201),Source),
    $pp_require_positive_integer(N,$msg(0204),Source),
    $pp_expand_pseudo_counts(Spec,N,Cs,Source),
    $pp_check_expanded_pseudo_count_size(Cs,N,Source).

$pp_expand_pseudo_counts(Spec,N,Cs,Source) :-
    ( Spec = default,
      $pp_get_default_pseudo_counts(none)
          -> $pp_raise_runtime_error($msg(0202),
                                     default_hyperparameters_unavailable,
                                     Source)
    ; true
    ),
    $pp_spec_to_pseudo_counts(Spec,N,Cs,Source).

$pp_spec_to_pseudo_counts(Spec,_N,Cs,_Source), Spec = [_|_] =>  Cs = Spec.

$pp_spec_to_pseudo_counts(Spec,N,Cs,_Source), number(Spec) =>
    C = Spec,
    $pp_gen_dup_list(N,C,Cs).

$pp_spec_to_pseudo_counts(uniform,N,Cs,Source) =>
    $pp_spec_to_pseudo_counts(uniform(1.0),N,Cs,Source).

$pp_spec_to_pseudo_counts(uniform(U),N,Cs,_Source) =>
    C is U / N,
    $pp_gen_dup_list(N,C,Cs).

$pp_spec_to_pseudo_counts(f_geometric,N,Cs,Source) =>
    $pp_spec_to_pseudo_counts(f_geometric(1.0,2.0,desc),N,Cs,Source).

$pp_spec_to_pseudo_counts(f_geometric(Base),N,Cs,Source) =>
    $pp_spec_to_pseudo_counts(f_geometric(1.0,Base,desc),N,Cs,Source).

$pp_spec_to_pseudo_counts(f_geometric(Init,Base),N,Cs,Source) =>
    $pp_spec_to_pseudo_counts(f_geometric(Init,Base,desc),N,Cs,Source).

$pp_spec_to_pseudo_counts(f_geometric(Init,Base,Type),N,Cs,_Source) =>
    $pp_spec_to_ratio_fgeom(Init,Base,Type,N,Cs).

$pp_spec_to_pseudo_counts(default,N,Cs,Source) =>
    $pp_get_default_pseudo_counts(Spec),   % get hyperparameters anyway
    $pp_require_hyperparameters(Spec,$msg(0201),Source),!,
    $pp_spec_to_pseudo_counts(Spec,N,Cs,Source).

$pp_get_default_pseudo_counts(Spec) :-
    ( get_prism_flag(default_sw_a,$disabled) ->
          get_prism_flag(default_sw_d,Spec)
    ; get_prism_flag(default_sw_a,Spec)
    ).

$pp_check_expanded_pseudo_count_size(List,N,Source) :-
    length(List,N1),
    ( N = N1 -> true
    ; $pp_raise_runtime_error($msg(0211),[List,N],unmatched_pseudo_counts,
                              Source)
    ),!.

%%----------------------------------------

$pp_spec_to_ratio_fgeom(Base,Type,N,Ratio) :-
    $pp_spec_to_ratio_fgeom(1.0,Base,Type,N,Ratio).

$pp_spec_to_ratio_fgeom(Init,Base,Type,N,Ratio) :-
    $pp_gen_geom_list(N,Init,Base,Ratio0),
    ( Type == asc -> Ratio0 = Ratio ; reverse(Ratio0,Ratio) ).

%%----------------------------------------

$pp_expr_to_list(Op,Expr,List) :-
    current_op(_,yfx,Op),!,
    $pp_expr_to_list_yfx(Op,Expr,List,[]).
$pp_expr_to_list(Op,Expr,List) :-
    current_op(_,xfy,Op),!,
    $pp_expr_to_list_xfy(Op,Expr,List,[]).

$pp_expr_to_list_yfx(Op,Expr,L0,L1), functor(Expr,Op,2) =>
    Expr =.. [Op,Expr1,X],
    L2 = [X|L1], !,
    $pp_expr_to_list_yfx(Op,Expr1,L0,L2).
$pp_expr_to_list_yfx(_ ,Expr,L0,L1) =>
    L0 = [Expr|L1].

$pp_expr_to_list_xfy(Op,Expr,L0,L1), functor(Expr,Op,2) =>
    Expr =.. [Op,X,Expr1],
    L0 = [X|L2], !,
    $pp_expr_to_list_xfy(Op,Expr1,L2,L1).
$pp_expr_to_list_xfy(_ ,Expr,L0,L1) =>
    L0 = [Expr|L1].

%%----------------------------------------

$pp_gen_geom_list(0,_,_,[]) :- !.
$pp_gen_geom_list(N,X,Base,[X|Xs1]) :-
    X1 is X * Base,
    N1 is N - 1,!,
    $pp_gen_geom_list(N1,X1,Base,Xs1).

$pp_gen_rand_list(0,[]) :- !.
$pp_gen_rand_list(N,[X|Xs1]) :-
    random_uniform(X),
    N1 is N - 1,!,
    $pp_gen_rand_list(N1,Xs1).

$pp_gen_noisy_u_list(N,Xs) :-
    get_prism_flag(std_ratio,StdRatio),
    Mu is 1.0 / N,
    Sigma is Mu * StdRatio,
    $pp_gen_noisy_u_list(N,Mu,Sigma,Xs).

$pp_gen_noisy_u_list(0,_,_,[]) :- !.
$pp_gen_noisy_u_list(N,Mu,Sigma,[X|Xs]) :-
    random_gaussian(Mu,Sigma,X0),
    ( X0 < 1.0e-9 -> X = 1.0e-9    % there would be a better way
    ; X = X0
    ),
    N1 is N - 1,!,
    $pp_gen_noisy_u_list(N1,Mu,Sigma,Xs).

$pp_gen_dup_list(0,_,[]) :- !.
$pp_gen_dup_list(N,C,[C|Cs]) :-
    N1 is N - 1,!,
    $pp_gen_dup_list(N1,C,Cs).

%%--------------------------------
%%  Temporary Clauses

:- dynamic $pd_temp_clause/2.
:- dynamic $pd_temp_clause/3.
:- dynamic $pd_temp_clause/4.

:- global_set($pg_temp_clause_num,0).

$pp_create_temp_clause_1(ID,X,Body) :-
    $pp_create_temp_clause_num(ID),
    assert(($pd_temp_clause(ID,X) :- Body)), !.

$pp_create_temp_clause_2(ID,X,Y,Body) :-
    $pp_create_temp_clause_num(ID),
    assert(($pd_temp_clause(ID,X,Y) :- Body)), !.

$pp_create_temp_clause_3(ID,X,Y,Z,Body) :-
    $pp_create_temp_clause_num(ID),
    assert(($pd_temp_clause(ID,X,Y,Z) :- Body)), !.

$pp_delete_temp_clause_1(ID) :-
    retractall($pd_temp_clause(ID,_)),
    $pp_delete_temp_clause_num(ID), !.

$pp_delete_temp_clause_2(ID) :-
    retractall($pd_temp_clause(ID,_,_)),
    $pp_delete_temp_clause_num(ID), !.

$pp_delete_temp_clause_3(ID) :-
    retractall($pd_temp_clause(ID,_,_,_)),
    $pp_delete_temp_clause_num(ID), !.

$pp_create_temp_clause_num(N) :-
    global_get($pg_temp_clause_num,M),
    N is M + 1,
    global_set($pg_temp_clause_num,N), !.

$pp_delete_temp_clause_num(N) :-
    global_get($pg_temp_clause_num,N),
    M is N - 1,
    global_set($pg_temp_clause_num,M), !.
$pp_delete_temp_clause_num(_).
    

%%--------------------------------
%%  Base Predicates

$pp_length(Xs,N) :-
    $pp_length(Xs,0,N).

$pp_length(Xs0,N0,N), Xs0 = [] =>
    N0 = N.
$pp_length(Xs0,N0,N), Xs0 = [_|Xs1] =>
    N1 is N0 + 1,
    $pp_length(Xs1,N1,N).

$pp_match(Patt,X) :-
    \+ \+ ( number_vars(X,0,_), Patt ?= X ).

$pp_copy_term(X0,X) :-
    ground(X0) -> X0 = X ; copy_term(X0,X).

$pp_count(Table,Key,N) :-
    ( $pp_hashtable_get(Table,Key,N0) -> N is N0 + 1 ; N is 1 ),
    $pp_hashtable_put(Table,Key,N).

%%--------------------------------
%%  Stat: Means

avglist(List,Mean) :-
    $pp_meanlist(List,_,Mean,avglist/2).

meanlist(List,Mean) :-
    $pp_meanlist(List,_,Mean,meanlist/2).

gmeanlist(List,Mean) :-
    $pp_gmeanlist(List,_,Mean,gmeanlist/2).

hmeanlist(List,Mean) :-
    $pp_hmeanlist(List,_,Mean,hmeanlist/2).

$pp_meanlist(List,N,M,Source) :-
    $pp_require_list_not_shorter_than(List,1,$msg(2103),Source),
    $pp_require_numbers(List,$msg(2108),Source),
    ( $pp_meanlist(List,0,N0,0,M0) ->
      N0 = N,
      M0 = M
    ; throw(error(type_error(list,List),Source))
    ).

$pp_meanlist(Xs,N0,N,M0,M), Xs = [] =>
    N0 = N,
    M0 = M.
$pp_meanlist(Xs,N0,N,M0,M), Xs = [X|Xs1] =>
    N1 is N0 + 1,
    M1 is M0 + (X - M0) / N1,
    $pp_meanlist(Xs1,N1,N,M1,M).

$pp_gmeanlist(List,N,M,Source) :-
    $pp_require_list_not_shorter_than(List,1,$msg(2103),Source),
    $pp_require_numbers(List,$msg(2108),Source),
    ( $pp_gmeanlist(List,0,N0,0,M0) ->
      N0 = N,
      M0 = M
    ; throw(error(type_error(list,List),Source))
    ).

$pp_gmeanlist(Xs,N0,N,M0,M), Xs = [] =>
    N = N0, M is exp(M0).
$pp_gmeanlist(Xs,N0,N,M0,M), Xs = [X|Xs1] =>
    N1 is N0 + 1,
    M1 is M0 + (log(X) - M0) / N1,
    $pp_gmeanlist(Xs1,N1,N,M1,M).

$pp_hmeanlist(List,N,M,Source) :-
    $pp_require_list_not_shorter_than(List,1,$msg(2103),Source),
    $pp_require_numbers(List,$msg(2108),Source),
    ( $pp_hmeanlist(List,0,N0,0,M0) ->
      N0 = N, M0 = M
    ; throw(error(type_error(list,List),Source))
    ).

$pp_hmeanlist(Xs,N0,N,M0,M), Xs = [] =>
    N = N0, M is 1 / M0.
$pp_hmeanlist(Xs,N0,N,M0,M), Xs = [X|Xs1] =>
    N1 is N0 + 1,
    M1 is M0 + (1 / X  - M0) / N1,
    $pp_hmeanlist(Xs1,N1,N,M1,M).


%%--------------------------------
%%  Stat: Variance etc.

varlistp(List,Var) :-
    $pp_moment2(List,1,N,_,M2,varlistp/2),
    Var is M2 / N.

varlist(List,Var) :-
    $pp_moment2(List,2,N,_,M2,varlist/2),
    Var is M2 / (N - 1).

stdlistp(List,Std) :-
    $pp_moment2(List,1,N,_,M2,stdlistp/2),
    Std is sqrt(M2 / N).

stdlist(List,Std) :-
    $pp_moment2(List,2,N,_,M2,stdlist/2),
    Std is sqrt(M2 / (N - 1)).

semlistp(List,Sem) :-
    $pp_moment2(List,1,N,_,M2,semlistp/2),
    Sem is sqrt(M2) / N.

semlist(List,Sem) :-
    $pp_moment2(List,2,N,_,M2,semlist/2),
    Sem is sqrt(M2 / (N - 1) / N).

skewlistp(List,Skew) :-
    $pp_moment3(List,1,N,_,M2,M3,skewlistp/2),
    $pp_compute_skew0(Skew,N,M2,M3).

skewlist(List,Skew) :-
    $pp_moment3(List,3,N,_,M2,M3,skewlist/2),
    $pp_compute_skew1(Skew,N,M2,M3).

kurtlistp(List,Kurt) :-
    $pp_moment4(List,1,N,_,M2,_,M4,kurtlistp/2),
    $pp_compute_kurt0(Kurt,N,M2,M4).

kurtlist(List,Kurt) :-
    $pp_moment4(List,4,N,_,M2,_,M4,kurtlist/2),
    $pp_compute_kurt1(Kurt,N,M2,M4).

$pp_moment2(List,MinN,N,M,M2,Source) :-
    $pp_require_list_not_shorter_than(List,1,$msg(2103),Source),
    $pp_require_numbers(List,$msg(2108),Source),
    $pp_moment2(List,0,N0,0,TmpM,0,TmpM2),
    ( N0 >= MinN -> true
    ; $pp_require_list_not_shorter_than(List,MinN,$msg(2103),Source)
    ),
    N0 = N, TmpM = M, TmpM2 = M2.

$pp_moment3(List,MinN,N,M,M2,M3,Source) :-
    $pp_require_list_not_shorter_than(List,1,$msg(2103),Source),
    $pp_require_numbers(List,$msg(2108),Source),
    $pp_moment3(List,0,N0,0,TmpM,0,TmpM2,0,TmpM3),
    ( N0 >= MinN -> true
    ; $pp_require_list_not_shorter_than(List,MinN,$msg(2103),Source)
    ),
    N0 = N, TmpM = M, TmpM2 = M2, TmpM3 = M3.

$pp_moment4(List,MinN,N,M,M2,M3,M4,Source) :-
    $pp_require_list_not_shorter_than(List,1,$msg(2103),Source),
    $pp_require_numbers(List,$msg(2108),Source),
    $pp_moment4(List,0,N0,0,TmpM,0,TmpM2,0,TmpM3,0,TmpM4),
    ( N0 >= MinN -> true
    ; $pp_require_list_not_shorter_than(List,MinN,$msg(2103),Source)
    ),
    N0 = N, TmpM = M, TmpM2 = M2, TmpM3 = M3, TmpM4 = M4.

$pp_moment2(Xs,TmpN,N,TmpM,M,TmpM2,M2), Xs = [] =>
    TmpN = N,
    TmpM = M,
    TmpM2 = M2.
$pp_moment2(Xs,OldN,N,OldM,M,OldM2,M2), Xs = [X|Xs1] =>
    NewN is OldN + 1,
    D is X - OldM,
    E is D / NewN,
    F is D * E * OldN,          % == (X - OldM) * (X - NewM)
    NewM is OldM + E,
    NewM2 is OldM2 + F,
    $pp_moment2(Xs1,NewN,N,NewM,M,NewM2,M2).

$pp_moment3(Xs,TmpN,N,TmpM,M,TmpM2,M2,TmpM3,M3), Xs = [] =>
    TmpN = N,
    TmpM = M,
    TmpM2 = M2,
    TmpM3 = M3.
$pp_moment3(Xs,OldN,N,OldM,M,OldM2,M2,OldM3,M3), Xs = [X|Xs1] =>
    NewN is OldN + 1,
    D is X - OldM,
    E is D / NewN,
    F is D * E * OldN,          % == (X - OldM) * (X - OldN)
    NewM is OldM + E,
    NewM2 is OldM2 + F,
    NewM3 is OldM3 + E * (F * (NewN - 2) - 3 * OldM2),
    $pp_moment3(Xs1,NewN,N,NewM,M,NewM2,M2,NewM3,M3).

$pp_moment4(Xs,TmpN,N,TmpM,M,TmpM2,M2,TmpM3,M3,TmpM4,M4), Xs = [] =>
    TmpN = N,
    TmpM = M,
    TmpM2 = M2,
    TmpM3 = M3,
    TmpM4 = M4.
$pp_moment4(Xs,OldN,N,OldM,M,OldM2,M2,OldM3,M3,OldM4,M4), Xs = [X|Xs1] =>
    NewN is OldN + 1,
    D is X - OldM,
    E is D / NewN,
    F is D * E * OldN,          % == (X - OldM) * (X - OldN)
    NewM is OldM + E,
    NewM2 is OldM2 + F,
    NewM3 is OldM3 + E * (F * (NewN - 2) - 3 * OldM2),
    NewM4 is OldM4 + E * (E * F * (NewN ** 2 - (NewN + 1)) - 2 * (OldM3 + NewM3)),
    $pp_moment4(Xs1,NewN,N,NewM,M,NewM2,M2,NewM3,M3,NewM4,M4).

$pp_compute_skew0(Skew,N,M2,M3) :-
    Skew is M3 / M2 * sqrt(N / M2).

$pp_compute_skew1(Skew,N,M2,M3) :-
    Skew is M3 / M2 * sqrt((N - 1) / M2) * N / (N - 2).

$pp_compute_kurt0(Kurt,N,M2,M4) :-
    Kurt is M4 / (M2 * M2) * N - 3.

$pp_compute_kurt1(Kurt,N,M2,M4) :-
    F is M4 / (M2 * M2) * N * (N + 1),
    G is 3 * (N - 1),
    H is (N - 1) / (float(N - 2) * (N - 3)), % float(*) avoids overflow
    Kurt is (F - G) * H.


%%--------------------------------
%%  Stat: Mode

modelist(List,Mode) :-
    $pp_modelist(List,Mode,modelist/2).

amodelist(List,Modes) :-
    $pp_amodelist(List,Modes,amodelist/2).

rmodelist(List,Mode) :-
    $pp_amodelist(List,Modes,rmodelist/2),
    $pp_pmodelist(Modes,Mode).

pmodelist(List,Mode) :-
    $pp_pmodelist(List,Mode,pmodelist/2).

$pp_modelist(List,Mode,Source) :-
    $pp_require_list_not_shorter_than(List,1,$msg(2103),Source),
    $pp_require_nonvars(List,$msg(2110),Source),
    new_hashtable(Table),
    ( $pp_modelist(List,Table,_,0,Mode0) ->
      $pp_copy_term(Mode0,Mode)
    ; throw(error(type_error(list,List),Source))
    ).

$pp_modelist(Xs,_,Y,_,Mode), Xs = [] =>
    Y = Mode.
$pp_modelist(Xs,Table,Y0,N0,Mode), Xs = [X|Xs1] =>
    $pp_count(Table,X,N),
    ( $pp_modelist_cmp(N0,N,Y0,X) -> Y1 = X, N1 = N ; Y1 = Y0, N1 = N0 ),
    $pp_modelist(Xs1,Table,Y1,N1,Mode).

$pp_modelist_cmp(N0,N,_,_), N0 < N => true.
$pp_modelist_cmp(N0,N,_,_), N0 > N => fail.
$pp_modelist_cmp(_,_,X0,X) =>
    X0 @> X.

$pp_amodelist(List,Modes,Source) :-
    $pp_require_list_not_shorter_than(List,1,$msg(2103),Source),
    $pp_require_nonvars(List,$msg(2110),Source),
    new_hashtable(Table),
    ( $pp_amodelist(List,Table,_,0,Modes0) ->
      $pp_copy_term(Modes0,Modes1),
      sort(Modes1,Modes)
    ; throw(error(type_error(list,List),Source))
    ).

$pp_amodelist(Xs,_,Ys,_,Modes), Xs = [] =>
    Ys = Modes.
$pp_amodelist(Xs,Table,Ys0,N0,Modes), Xs = [X|Xs1] =>
    $pp_count(Table,X,N),
    ( N0 < N ->
      Ys1 = [X], N1 = N
    ; N0 > N ->
      Ys1 = Ys0, N1 = N0
    ; %% else
      Ys1 = [X|Ys0], N1 = N0
    ),
    $pp_amodelist(Xs1,Table,Ys1,N1,Modes).

$pp_pmodelist(List,Mode,Source) :-
    $pp_require_list_not_shorter_than(List,1,$msg(2103),Source),
    $pp_require_nonvars(List,$msg(2110),Source),
    ( $pp_pmodelist(List,Mode0) ->
      Mode0 = Mode
    ; throw(error(type_error(list,List),Source))
    ).

$pp_pmodelist(List,Mode) :-
    $pp_length(List,L), $pc_random_int(L,I), nth0(I,List,Mode).


%%--------------------------------
%%  Stat: Median

medianlist(List,Median) :-
    $pp_medianlist(List,Median,medianlist/2).

$pp_medianlist(List,Median,Source) :-
    $pp_require_list_not_shorter_than(List,1,$msg(2103),Source),
    $pp_require_numbers(List,$msg(2108),Source),
    ( $pp_medianlist(List,Median0) ->
      Median0 = Median
    ; throw(error(type_error(list,List),Source))
    ).

$pp_medianlist(List,Median) :-
    $pp_length(List,L),
    N is L // 2,
    $pp_mergesort(0,L,List,_,Temp),
    ( L mod 2 is 0 ->
      nth1(N,Temp,A),
      nth0(N,Temp,B),
      Median is A + (B - A) / 2         % avoids overflow
    ; nth0(N,Temp,Median)
    ).


%%--------------------------------
%%  Stat: Min/Max

minlist(List,Min) :-
    $pp_require_list_not_shorter_than(List,1,$msg(2103),minlist/2),
    $pp_require_numbers(List,$msg(2108),minlist/2),
    Min is min(List).

maxlist(List,Max) :-
    $pp_require_list_not_shorter_than(List,1,$msg(2103),maxlist/2),
    $pp_require_numbers(List,$msg(2108),maxlist/2),
    Max is max(List).


%%--------------------------------
%%  Stat: agglist/2

agglist(List,Dest) :-
    $pp_require_list_not_shorter_than(Dest,1,$msg(2103),agglist/2),
    Flag = $aggop(0,0,0),
    $pp_agglist_1(Dest,Flag),
    $pp_agglist_2(List,Flag,N,M,M2,M3,M4,Modes),
    $pp_agglist_3(List,Dest,N,M,M2,M3,M4,Modes).

$pp_agglist_1(Dest,_), Dest = [] => true.
$pp_agglist_1(Dest,Flag), Dest = [Op=_|Dest1] =>
    $pp_require_agglist_operation(Op,$msg(2107),agglist/2),
    %%  X = none(0)/len(1)/mean(2)/var(3)/skew(4)/kurt(5)
    %%  Y = none(0)/mode(1)/amode(2)
    ( Op == sum    -> X = 0, Y = 0, N = 0
    ; Op == avg    -> X = 2, Y = 0, N = 1
    ; Op == mean   -> X = 2, Y = 0, N = 1
    ; Op == gmean  -> X = 0, Y = 0, N = 1
    ; Op == hmean  -> X = 0, Y = 0, N = 1
    ; Op == varp   -> X = 3, Y = 0, N = 1
    ; Op == var    -> X = 3, Y = 0, N = 2
    ; Op == stdp   -> X = 3, Y = 0, N = 1
    ; Op == std    -> X = 3, Y = 0, N = 2
    ; Op == semp   -> X = 3, Y = 0, N = 1
    ; Op == sem    -> X = 3, Y = 0, N = 2
    ; Op == skewp  -> X = 4, Y = 0, N = 1
    ; Op == skew   -> X = 4, Y = 0, N = 3
    ; Op == kurtp  -> X = 5, Y = 0, N = 1
    ; Op == kurt   -> X = 5, Y = 0, N = 4
    ; Op == mode   -> X = 0, Y = 1, N = 1
    ; Op == amode  -> X = 0, Y = 2, N = 1
    ; Op == rmode  -> X = 0, Y = 2, N = 1
    ; Op == pmode  -> X = 0, Y = 0, N = 1
    ; Op == median -> X = 0, Y = 0, N = 1
    ; Op == min    -> X = 0, Y = 0, N = 1
    ; Op == max    -> X = 0, Y = 0, N = 1
    ; Op == len    -> X = 1, Y = 0, N = 0
    ),
    Flag = $aggop(X0,Y0,N0),
    ( X0 < X -> setarg(1,Flag,X) ; true ),
    ( Y0 < Y -> setarg(2,Flag,Y) ; true ),
    ( N0 < N -> setarg(3,Flag,N) ; true ), !,
    $pp_agglist_1(Dest1,Flag).

$pp_agglist_2(List,Flag,N,M,M2,M3,M4,Modes) :-
    Flag = $aggop(X,Y,MinN),
    ( X == 0 ->
      true
    ; X == 1 -> $pp_length(List,N)
    ; X == 2 ->
      $pp_meanlist(List,N,M,agglist/2)
    ; X == 3 ->
      $pp_moment2(List,MinN,N,M,M2,agglist/2)
    ; X == 4 ->
      $pp_moment3(List,MinN,N,M,M2,M3,agglist/2)
    ; X == 5 ->
      $pp_moment4(List,MinN,N,M,M2,M3,M4,agglist/2)
    ; %% else
      $pp_unmatched_branches($pp_agglist_2/8,first_arg)
    ),
    ( Y == 0 ->
      true
    ; Y == 1 ->
      $pp_modelist(List,Mode,agglist/2), Modes = [Mode]
    ; Y == 2 ->
      $pp_amodelist(List,Modes,agglist/2)
    ; %% else
      $pp_unmatched_branches($pp_agglist_2/8,second_arg)
    ).

$pp_agglist_3(_,Dest,_,_,_,_,_,_), Dest = [] => true.
$pp_agglist_3(List,Dest,N,M,M2,M3,M4,Mode), Dest = [Op=Y|Dest1] =>
    ( Op == sum    -> Y is sum(List)
    ; Op == avg    -> Y = M
    ; Op == mean   -> Y = M
    ; Op == gmean  -> $pp_gmeanlist(List,_,Y,agglist/2)
    ; Op == hmean  -> $pp_hmeanlist(List,_,Y,agglist/2)
    ; Op == varp   -> Y is M2 / N
    ; Op == var    -> Y is M2 / (N - 1)
    ; Op == stdp   -> Y is sqrt(M2 / N)
    ; Op == std    -> Y is sqrt(M2 / (N - 1))
    ; Op == semp   -> Y is sqrt(M2) / N
    ; Op == sem    -> Y is sqrt(M2 / (N - 1) / N)
    ; Op == skewp  -> $pp_compute_skew0(Y,N,M2,M3)
    ; Op == skew   -> $pp_compute_skew1(Y,N,M2,M3)
    ; Op == kurtp  -> $pp_compute_kurt0(Y,N,M2,M4)
    ; Op == kurt   -> $pp_compute_kurt1(Y,N,M2,M4)
    ; Op == mode   -> [Y|_] = Mode
    ; Op == amode  -> Y = Mode
    ; Op == rmode  -> $pp_pmodelist(Mode,Y)
    ; Op == pmode  -> $pp_pmodelist(List,Y,agglist/2)
    ; Op == median -> $pp_medianlist(List,Y,agglist/2)
    ; Op == min    -> Y is min(List)
    ; Op == max    -> Y is max(List)
    ; Op == len    -> Y = N
    ; $pp_raise_unmatched_branches($pp_agglist_3/8,operation)
    ), !,
    $pp_agglist_3(List,Dest1,N,M,M2,M3,M4,Mode).
$pp_agglist_3(_,_,_,_,_,_) =>
    $pp_raise_unmatched_branches($pp_agglist_3/8,list).


%%--------------------------------
%%  Map

maplist(X,Clause,Xs) :-
    $pp_create_temp_clause_1(ID,X,Clause),
    ( $pp_maplist(ID,Xs) -> R = true ; R = fail ),
    $pp_delete_temp_clause_1(ID), R.

maplist(X,Y,Clause,Xs,Ys) :-
    $pp_create_temp_clause_2(ID,X,Y,Clause),
    ( $pp_maplist(ID,Xs,Ys) -> R = true ; R = fail ),
    $pp_delete_temp_clause_2(ID), R.

maplist(X,Y,Z,Clause,Xs,Ys,Zs) :-
    $pp_create_temp_clause_3(ID,X,Y,Z,Clause),
    ( $pp_maplist(ID,Xs,Ys,Zs) -> R = true ; R = fail ),
    $pp_delete_temp_clause_3(ID), R.

$pp_maplist(_,[]).
$pp_maplist(ID,[X|Xs]) :-
    $pd_temp_clause(ID,X), !, $pp_maplist(ID,Xs).

$pp_maplist(_,[],[]).
$pp_maplist(ID,[X|Xs],[Y|Ys]) :-
    $pd_temp_clause(ID,X,Y), !, $pp_maplist(ID,Xs,Ys).

$pp_maplist(_,[],[],[]).
$pp_maplist(ID,[X|Xs],[Y|Ys],[Z|Zs]) :-
    $pd_temp_clause(ID,X,Y,Z), !, $pp_maplist(ID,Xs,Ys,Zs).

maplist_func(F,Xs) :-
    $pp_require_atom(F,$msg(2100),maplist_func/2),
    $pp_maplist_func(F,Xs).

maplist_func(F,Xs,Ys) :-
    $pp_require_atom(F,$msg(2100),maplist_func/3),
    $pp_maplist_func(F,Xs,Ys).

maplist_func(F,Xs,Ys,Zs) :-
    $pp_require_atom(F,$msg(2100),maplist_func/4),
    $pp_maplist_func(F,Xs,Ys,Zs).

$pp_maplist_func(_,[]).
$pp_maplist_func(F,[X|Xs]) :-
    call(F,X), !, $pp_maplist_func(F,Xs).

$pp_maplist_func(_,[],[]).
$pp_maplist_func(F,[X|Xs],[Y|Ys]) :-
    call(F,X,Y), !, $pp_maplist_func(F,Xs,Ys).

$pp_maplist_func(_,[],[],[]).
$pp_maplist_func(F,[X|Xs],[Y|Ys],[Z|Zs]) :-
    call(F,X,Y,Z), !, $pp_maplist_func(F,Xs,Ys,Zs).

maplist_math(Op,Xs,Ys) :-
    $pp_require_atom(Op,$msg(2101),maplist_math/3),
    $pp_require_list_or_nil(Xs,$msg(2109),maplist_math/3),
    functor(Expr,Op,1),
    $pp_maplist_math(Expr,Xs,Ys).

maplist_math(Op,Xs,Ys,Zs) :-
    $pp_require_atom(Op,$msg(2102),maplist_math/4),
    $pp_require_list_or_nil(Xs,$msg(2109),maplist_math/3),
    $pp_require_list_or_nil(Ys,$msg(2109),maplist_math/3),
    functor(Expr,Op,2),
    $pp_maplist_math(Expr,Xs,Ys,Zs).

$pp_maplist_math(_,[],[]).
$pp_maplist_math(Expr,[X|Xs],[Y|Ys]) :-
    setarg(1,Expr,X),
    Y is Expr, !,
    $pp_maplist_math(Expr,Xs,Ys).

$pp_maplist_math(_,[],[],[]).
$pp_maplist_math(Expr,[X|Xs],[Y|Ys],[Z|Zs]) :-
    setarg(1,Expr,X),
    setarg(2,Expr,Y),
    Z is Expr, !,
    $pp_maplist_math(Expr,Xs,Ys,Zs).


%%--------------------------------
%%  Reduction

reducelist(A,B,C,Body,Xs,Y0,Y) :-
    $pp_require_callable(Body,$msg(2111),reducelist/7),
    $pp_require_list_or_nil(Xs,$msg(2109),reducelist/7),
    $pp_create_temp_clause_3(ID,A,B,C,Body),
    ( $pp_reducelist(ID,Xs,Y0,Y) -> R = true ; R = fail ),
    $pp_delete_temp_clause_3(ID), R.

$pp_reducelist(_,[],Y,Y).
$pp_reducelist(ID,[X|Xs],Y0,Y) :-
    $pd_temp_clause(ID,Y0,X,Y1), !, $pp_reducelist(ID,Xs,Y1,Y).

reducelist_func(F,Xs,Y0,Y) :-
    $pp_require_atom(F,$msg(2100),reducelist_func/4),
    $pp_require_list_or_nil(Xs,$msg(2109),reducelist_func/4),
    $pp_reducelist_func(F,Xs,Y0,Y).

$pp_reducelist_func(_,[],Y,Y).
$pp_reducelist_func(F,[X|Xs],Y0,Y) :-
    call(F,Y0,X,Y1), !, $pp_reducelist_func(F,Xs,Y1,Y).

reducelist_math(Op,Xs,Y0,Y) :-
    $pp_require_atom(Op,$msg(2102),reducelist_math/4),
    $pp_require_list_or_nil(Xs,$msg(2109),reducelist_math/4),
    functor(Expr,Op,2),
    $pp_reducelist_math(Expr,Xs,Y0,Y).

$pp_reducelist_math(_,[],Y,Y).
$pp_reducelist_math(Expr,[X|Xs],Y0,Y) :-
    setarg(1,Expr,Y0),
    setarg(2,Expr, X),
    Y1 is Expr, !,    % added by kameya (Nov 05, 2010)
    $pp_reducelist_math(Expr,Xs,Y1,Y).

%%--------------------------------
%%  Sublists

sublist(Sub,Lst) :-
    $pp_sublist1(I,_,Lst,Tmp),
    $pp_sublist2(I,_,Tmp,Sub).

sublist(Sub,Lst,I,J) :-
    $pp_sublist1(I,J,Lst,Tmp),
    $pp_sublist2(I,J,Tmp,Sub).

$pp_sublist1(I,J,Xs,Ys) :- var(I), !,
    $pp_sublist1_var(0,I,J,Xs,Ys).
$pp_sublist1(I,J,Xs,Ys) :- var(J), !,
    $pp_sublist1_det(I,Xs,Ys).
$pp_sublist1(I,J,Xs,Ys) :- I =< J, !,
    $pp_sublist1_det(I,Xs,Ys).

%%  [03 Dec 2008, by yuizumi]
%%  This predicate would cause infinite loops without (I0 < J) for queries
%%  such as ( sublist(_,_,I,0), I > 0 ).

$pp_sublist1_var(I0,I,_,Xs,Ys) :-
    I0 = I,
    Xs = Ys.
$pp_sublist1_var(I0,I,J,Xs,Ys) :- var(J),!,
    I1 is I0 + 1,
    Xs = [_|Xs1],
    $pp_sublist1_var(I1,I,J,Xs1,Ys).
$pp_sublist1_var(I0,I,J,Xs,Ys) :- I0 < J, !,
    I1 is I0 + 1,
    Xs = [_|Xs1],
    $pp_sublist1_var(I1,I,J,Xs1,Ys).

$pp_sublist1_det(I,Xs,Ys) :- I =:= 0, !,
    Xs = Ys.
$pp_sublist1_det(I,Xs,Ys) :- I  >  0, !,
    I1 is I - 1,
    Xs = [_|Xs1],
    $pp_sublist1_det(I1,Xs1,Ys).

$pp_sublist2(I,J,Xs,Ys) :- var(J), !,
    $pp_sublist2_var(I,J,Xs,Ys).
$pp_sublist2(I,J,Xs,Ys) :- nonvar(J), !,
    N is J - I,
    $pp_sublist2_det(N,Xs,Ys).

$pp_sublist2_var(J0,J,_ ,Ys) :-
    J0 = J,
    Ys = [].
$pp_sublist2_var(J0,J,Xs,Ys) :-
    J1 is J0 + 1,
    Xs = [X|Xs1],
    Ys = [X|Ys1],
    $pp_sublist2_var(J1,J,Xs1,Ys1).

$pp_sublist2_det(N,_ ,Ys) :- N =:= 0, !,
    Ys = [].
$pp_sublist2_det(N,Xs,Ys) :- N  >  0, !,
    N1 is N - 1,
    Xs = [X|Xs1],
    Ys = [X|Ys1],
    $pp_sublist2_det(N1,Xs1,Ys1).


%%--------------------------------
%%  Splitting

splitlist(Prefix,Suffix,List,N) :-
    $pp_splitlist(N,List,Prefix,Suffix,splitlist/4).

grouplist(List,N,Sizes,Dest) :-
    $pp_require_positive_integer(N,$msg(2106),grouplist/4),
    $pp_grouplist(N,Sizes,List,Dest).

egrouplist(List,N,Dest) :-
    ( $pp_length(List,L) -> true
    ; $pp_raise_type_error($msg(2104),[List],[list,List],egrouplist/4)
    ),
    $pp_require_positive_integer(N,$msg(2106),egrouplist/4),!,
    $pp_egrouplist(N,L,List,Dest).

$pp_splitlist(N,Xs,Ys,Zs,_), var(N) =>
    $pp_splitlist_var(0,N,Xs,Ys,Zs).
$pp_splitlist(N,Xs,Ys,Zs,Source) :-
    $pp_require_non_negative_integer(N,$msg(2105),Source),
    $pp_splitlist_det(0,N,Xs,Ys,Zs).

$pp_splitlist_var(N0,N,Xs,Ys,Zs) ?=>
    N0 = N,
    Xs = Zs,
    Ys = [].
$pp_splitlist_var(N0,N,Xs,Ys,Zs) =>
    N1 is N0 + 1,
    Xs = [X|Xs1],
    Ys = [X|Ys1],
    $pp_splitlist_var(N1,N,Xs1,Ys1,Zs).

$pp_splitlist_det(N0,N,Xs,Ys,Zs), N0 =:= N =>
    Xs = Zs,
    Ys = [].
$pp_splitlist_det(N0,N,Xs,Ys,Zs), N0  <  N =>
    N1 is N0 + 1,
    Xs = [X|Xs1],
    Ys = [X|Ys1],
    $pp_splitlist_det(N1,N,Xs1,Ys1,Zs).

$pp_grouplist(N,Ls,Xs,Ys), N =:= 0 =>
    Ls = [],
    Xs = [],
    Ys = [].
$pp_grouplist(N,Ls,Xs,Ys), N > 0 =>
    Ls = [L|Ls1],
    Ys = [Y|Ys1],
    $pp_splitlist(L,Xs,Y,Xs1,grouplist/4),
    N1 is N - 1,
    $pp_grouplist(N1,Ls1,Xs1,Ys1).

$pp_egrouplist(N,_,_ ,Ys), N =:= 0 =>
    Ys = [].
$pp_egrouplist(N,L,Xs,Ys), N > 0 =>
    M is (L + N - 1) // N,
    Ys = [Y|Ys1],
    $pp_splitlist_det(0,M,Xs,Y,Xs1),
    N1 is N - 1,
    L1 is L - M,
    $pp_egrouplist(N1,L1,Xs1,Ys1).


%%--------------------------------
%%  Filtering

filter(Patt,Xs,Ys) :-
    ( $pp_filter(Patt,Xs,Ys) -> true
    ; $pp_raise_type_error($msg(2104),[Xs],[list,Xs],filter/3)
    ).

filter(Patt,Xs,Ys,Count) :-
    ( $pp_filter(Patt,Xs,Ys) -> true
    ; $pp_raise_type_error($msg(2104),[Xs],[list,Xs],filter/4)
    ),
    length(Ys,Count).

$pp_filter(_,Xs,Ys), Xs = [] =>
    Ys = [].
$pp_filter(Patt,Xs,Ys), Xs = [X|Xs1] =>
    ( $pp_match(Patt,X) -> Ys = [X|Ys1] ; Ys = Ys1 ),
    $pp_filter(Patt,Xs1,Ys1).

filter_not(Patt,Xs,Ys) :-
    ( $pp_filter_not(Patt,Xs,Ys) -> true
    ; $pp_raise_type_error($msg(2104),[Xs],[list,Xs],filter/4)
    ).

filter_not(Patt,Xs,Ys,Count) :-
    ( $pp_filter_not(Patt,Xs,Ys) -> true
    ; $pp_raise_type_error($msg(2104),[Xs],[list,Xs],filter_not/4)
    ),
    length(Ys,Count).

$pp_filter_not(_,Xs,Ys), Xs = [] =>
    Ys = [].
$pp_filter_not(Patt,Xs,Ys), Xs = [X|Xs1] =>
    ( $pp_match(Patt,X) -> Ys = Ys1 ; Ys = [X|Ys1] ),
    $pp_filter_not(Patt,Xs1,Ys1).


%%--------------------------------
%%  Counting

countlist(List,Counts) :-
    new_hashtable(Table),
    ( $pp_countlist(List,Table) -> true
    ; $pp_raise_type_error($msg(2104),[List],[list,List],countlist/2)
    ),
    hashtable_to_list(Table,Counts1),
    $pp_countlist_copy(Counts1,0,N),
    $pp_mergesort($pp_compare_eqpair(_,_),N,Counts1,_,Counts).

$pp_countlist(Xs,_), Xs = [] => true.
$pp_countlist(Xs,Table), Xs = [X|Xs1] =>
    $pp_count(Table,X,_), $pp_countlist(Xs1,Table).

countlist(Patt,List,Count) :-
    ( $pp_countlist(Patt,List,0,Count) -> true
    ; $pp_raise_type_error($msg(2104),[List],[list,List],countlist/3)
    ).

$pp_countlist(_,Xs,N0,N), Xs = [] => N0 = N.
$pp_countlist(Patt,Xs,N0,N), Xs = [X|Xs1] =>
    ( variant(X,Patt) -> N1 is N0 + 1 ; N1 is N0 ),
    $pp_countlist(Patt,Xs1,N1,N).

$pp_countlist_copy(KVs,N0,N), KVs = [] => N0 = N.
$pp_countlist_copy(KVs,N0,N), KVs = [KV|KVs1] =>
    KV = (Key=_),
    ( ground(Key) ->
      true
    ; copy_term(Key,KeyCp), setarg(1,KV,KeyCp) % overwrite
    ),
    N1 is N0 + 1,
    $pp_countlist_copy(KVs1,N1,N).

$pp_compare_eqpair((_=A2),(_=B2)), A2 > B2 => true.
$pp_compare_eqpair((A1=A2),(B1=B2)), A2 =:= B2 => A1 @< B1.


%%--------------------------------
%%  Sorting

number_sort(Xs,Ys) :-
    $pp_custom_sort(0,Xs,Ys,number_sort/2).

custom_sort(Op,Xs,Ys), Op == '<'  => $pp_custom_sort(0,Xs,Ys,custom_sort/3).
custom_sort(Op,Xs,Ys), Op == '@<' => $pp_custom_sort(1,Xs,Ys,custom_sort/3).
custom_sort(Op,Xs,Ys), atom(Op) =>
    functor(Term,Op,2),
    $pp_custom_sort(Term,Xs,Ys,custom_sort/3).
custom_sort(Op,_,_) =>
    $pp_require_atom(Op,$msg(2102),custom_sort/3).

custom_sort(A,B,Body,Xs,Ys) :-
    $pp_custom_sort($cmp(A,B,Body),Xs,Ys,custom_sort/5).

$pp_custom_sort(Cmp,Xs,Ys,Source) :-
    ( $pp_length(Xs,L) -> true
    ; $pp_raise_type_error($msg(2104),[Xs],[list,Xs],Source)
    ),
    $pp_mergesort(Cmp,L,Xs,_,Ys).

$pp_mergesort(_,N,Xs0,Xs1,Ys), N == 0 => Xs0 = Xs1, Ys = [].
$pp_mergesort(_,N,Xs0,Xs1,Ys), N == 1 => Xs0 = [X|Xs1], Ys = [X].
$pp_mergesort(Cmp,N,Xs0,Xs1,Ys) =>
    NL is N // 2,
    NR is N - NL,
    $pp_mergesort(Cmp,NL,Xs0,Xs2,Ys0),
    $pp_mergesort(Cmp,NR,Xs2,Xs1,Ys1),
    $pp_mergelist(Cmp,Ys0,Ys1,Ys).

$pp_mergelist(_,Xs,Ys,Zs), Xs == [] => Ys = Zs.
$pp_mergelist(_,Xs,Ys,Zs), Ys == [] => Xs = Zs.
$pp_mergelist(Cmp,Xs0,Ys0,Zs0), Cmp == 0 =>
    Xs0 = [X|Xs1],
    Ys0 = [Y|Ys1],
    ( Y < X ->
      Zs0 = [Y|Zs1], $pp_mergelist(Cmp,Xs0,Ys1,Zs1)
    ; Zs0 = [X|Zs1], $pp_mergelist(Cmp,Xs1,Ys0,Zs1)
    ).
$pp_mergelist(Cmp,Xs0,Ys0,Zs0), Cmp == 1 =>
    Xs0 = [X|Xs1],
    Ys0 = [Y|Ys1],
    ( Y @< X ->
      Zs0 = [Y|Zs1], $pp_mergelist(Cmp,Xs0,Ys1,Zs1)
    ; Zs0 = [X|Zs1], $pp_mergelist(Cmp,Xs1,Ys0,Zs1)
    ).
$pp_mergelist(Cmp,Xs0,Ys0,Zs0), functor(Cmp,_,2) =>
    Xs0 = [X|Xs1],
    Ys0 = [Y|Ys1],
    setarg(1,Cmp,Y),
    setarg(2,Cmp,X),
    ( Cmp ->
      Zs0 = [Y|Zs1], $pp_mergelist(Cmp,Xs0,Ys1,Zs1)
    ; Zs0 = [X|Zs1], $pp_mergelist(Cmp,Xs1,Ys0,Zs1)
    ).
$pp_mergelist(Cmp,Xs0,Ys0,Zs0) =>
    Xs0 = [X|Xs1],
    Ys0 = [Y|Ys1],
    ( \+ \+ ( Cmp = $cmp(Y,X,Body), Body ) ->
      Zs0 = [Y|Zs1], $pp_mergelist(Cmp,Xs0,Ys1,Zs1)
    ; Zs0 = [X|Zs1], $pp_mergelist(Cmp,Xs1,Ys0,Zs1)
    ).


%%--------------------------------

$pp_require_agglist_operation(Op,MsgID,Source) :-
    ( $pp_test_agglist_operation(Op) -> true
    ; $pp_raise_on_require([Op],MsgID,Source,$pp_error_agglist_operation)
    ).

$pp_test_agglist_operation(Op) :-
    atom(Op),
    membchk(Op,[sum,avg,mean,gmean,hmean,varp,var,
                stdp,std,semp,sem,skewp,skew,kurtp,kurt,
                mode,amode,rmode,pmode,median,min,max,len]).

$pp_error_agglist_operation(Op,instanciation_error) :-
    var(Op), !.
$pp_error_agglist_operation(Op,Error) :-
   \+ $pp_error_atom(Op,Error), !.
$pp_error_agglist_operation(Op,domain_error(agglist_operation,Op)) :-
   \+ $pp_test_agglist_operation(Op), !.
%%  Assumption:
%%    h(F) = h(G) where F and G are variants and h is the hash function

$pp_hashtable_get(T,K,V), T = $hshtb(_,_) => hashtable_get(T,K,V).
$pp_hashtable_get(T,_,_) => $pp_hashtable_throw(T,$pp_hashtable_get/3).

$pp_hashtable_put(T,K,V), T = $hshtb(N0,A) =>
    hash_code(K,H),
    functor(A,_,M),
    I is (H mod M) + 1,
    arg(I,A,L),
    member(KV,L),
    ( var(KV) ->
        KV = (K = V),
        N1 is N0 + 1,
        setarg(1,T,N1),
        ( N1 > 2 * M + 1, M < 32700 -> $hashtable_expand_buckets(T)
        ; true                     % #buckets should not exceed 65536
        )
    ; KV = (Key = _),
      variant(Key,K) -> setarg(2,KV,V)
    ), !.
$pp_hashtable_put(T,_,_) =>
    $pp_hashtable_throw(T,$pp_hashtable_put/3).

$pp_hashtable_throw(T,Source) :-
    ( nonvar(T) -> true
    ; throw(error(instantiation_error,Source))
    ),
    ( T ?= $hshtb(_,_) -> true
    ; throw(error(type_error(hashtable,T),Source))
    ), !,
    fail.                       % should not reach here
%% -*- Prolog -*-

%% prism_flag(Name,Type,Init,Pred) defines a new Prism flag where each
%% argument indicates:
%% 
%%   Name : the flag name
%%   Type : the domain of possible values
%%   Init : the default value
%%   Pred : the auxiliary predicate (see below) or `$none'.
%% 
%% Type should be one of the followings:
%% 
%%   bool:
%%     boolean value taking either `on' or `off'
%% 
%%   enum(Cands):
%%     atom occuring in Cands
%% 
%%   term(Cands):
%%     term matching one of patterns in Cands
%% 
%%   integer(Min,Max):
%%     integral value from Min to Max (Min/Max can be -inf/+inf)
%% 
%%   float(Min,Max):
%%     floating value from Min to Max (Min/Max can be -inf/+inf)
%% 
%% 
%% Declaring Auxiliary Predicates
%% ------------------------------
%% 
%% An auxiliary predicate is called just after a new value is set to
%% the corresponding flag.  A typical purpose of auxiliary predicates
%% is to have the new value notified to the C routines.
%% 
%% Auxiliary predicates must be of the arity one, and are called with
%% the argument indicating the new value set to the flag as described
%% below (depending on Type):
%% 
%%   bool:
%%     an integer 1 (on) or 0 (off).
%% 
%%   enum(Cands):
%%     an integer representing the index (starting at 0) at which the
%%     specified atom exists in Cands
%% 
%%   term(Cands):
%%     the specified term
%% 
%%   integer(Min,Max):
%%     the specified integral value
%% 
%%   float(Min,Max):
%%     the specified floating value
%% 
%% [TODO: describe open/half-open ranges of floating values]
%% [TODO: describe special(PredName)]
%% 
%% [Note] Make sure to declare flags in alphabetical order.

$pp_prism_flag(clean_table,bool,on,$none).
$pp_prism_flag(daem,bool,off,$pc_set_daem).
$pp_prism_flag(data_source,term([none,data/1,file(_)]),data/1,$none).
$pp_prism_flag(default_sw,special($pp_check_default_sw),uniform,$none).
$pp_prism_flag(default_sw_a,special($pp_check_default_sw_a),1,$none).
$pp_prism_flag(default_sw_d,special($pp_check_default_sw_d),0,$none).
$pp_prism_flag(em_progress,integer(1,+inf),10,$pc_set_em_progress).
$pp_prism_flag(epsilon,float(@0,+inf),0.0001,$pc_set_prism_epsilon).
$pp_prism_flag(error_on_cycle,bool,on,$pc_set_error_on_cycle).
$pp_prism_flag(explicit_empty_expls,bool,on,$pc_set_explicit_empty_expls).
$pp_prism_flag(fix_init_order,bool,on,$pc_set_fix_init_order).
$pp_prism_flag(force_gc,bool,on,$none).
$pp_prism_flag(init,enum([none,noisy_u,random]),random,$pc_set_init_method).
$pp_prism_flag(itemp_init,float(@0,1),0.1,$pc_set_itemp_init).
$pp_prism_flag(itemp_rate,float(@1,+inf),1.5,$pc_set_itemp_rate).
$pp_prism_flag(learn_message,special($pp_check_learn_message),all,$none).
$pp_prism_flag(learn_mode,enum([params,hparams,ml,vb,both,ml_vt,vb_vt,both_vt]),ml,$none).
$pp_prism_flag(log_scale,bool,off,$pc_set_log_scale).
$pp_prism_flag(max_iterate,special($pp_check_max_iterate),default,$pc_set_max_iterate).
$pp_prism_flag(mcmc_b,integer(0,+inf),1000,$none).
$pp_prism_flag(mcmc_e,integer(0,+inf),2000,$none).
$pp_prism_flag(mcmc_message,special($pp_check_mcmc_message),all,$none).
$pp_prism_flag(mcmc_progress,integer(1,+inf),100,$pc_set_mcmc_progress).
$pp_prism_flag(mcmc_s,integer(1,+inf),5,$none).
$pp_prism_flag(reset_hparams,bool,on,$none).
$pp_prism_flag(restart,integer(1,+inf),1,$pc_set_num_restart).
$pp_prism_flag(rerank,integer(1,+inf),10,$none).
$pp_prism_flag(search_progress,integer(1,+inf),10,$none).
$pp_prism_flag(show_itemp,bool,off,$pc_set_show_itemp).
$pp_prism_flag(sort_hindsight,enum([by_goal,by_prob]),by_goal,$none).
$pp_prism_flag(std_ratio,float(@0,+inf),0.2,$pc_set_std_ratio).
$pp_prism_flag(verb,special($pp_check_verb),none,$pp_set_verb).
$pp_prism_flag(viterbi_mode,enum([params,hparams,ml,vb]),ml,$none).
$pp_prism_flag(warn,bool,off,$pc_set_warn).
$pp_prism_flag(write_call_events,special($pp_check_write_call_events),all,$none).
$pp_prism_flag(error_on_invalid_distribution,bool,on,$none).
$pp_prism_flag(crf_enable,bool,on,$none).
$pp_prism_flag(crf_annealing_weight,float(1,+inf),1.0,$pc_set_annealing_weight).
$pp_prism_flag(crf_learning_rate,enum([none,annealing,backtrack,golden]),backtrack,$pc_set_crf_learning_rate).
$pp_prism_flag(crf_golden_b,float(@0,+inf),1.0,$pc_set_crf_golden_b).
$pp_prism_flag(crf_epsilon,float(0,+inf),1.0,$pc_set_crf_epsilon).
$pp_prism_flag(crf_init,enum([none,noisy_u,random,zero]),zero,$pc_set_crf_init_method).
$pp_prism_flag(crf_learn_mode,enum([fg,lbfgs]),fg,$pc_set_crf_learn_mode).
$pp_prism_flag(crf_ls_rho,float(@0,@1),0.5,$pc_set_crf_ls_rho).
$pp_prism_flag(crf_ls_c1,float(@0,@1),0.5,$pc_set_crf_ls_c1).
$pp_prism_flag(crf_penalty,float(-inf,+inf),0.0,$pc_set_crf_penalty).

% first flag is enabled by default
$pp_prism_flag_exclusive([default_sw_d,default_sw_a]).

$pp_prism_flag_renamed(default_sw_h,default_sw_d).

$pp_prism_flag_deleted(avg_branch,'1.11').
$pp_prism_flag_deleted(em_message,'2.0.1').
$pp_prism_flag_deleted(layer_check,'1.11').
$pp_prism_flag_deleted(log_viterbi,'2.0').
$pp_prism_flag_deleted(dynamic_default_sw,'2.0').
$pp_prism_flag_deleted(dynamic_default_sw_h,'2.0').
$pp_prism_flag_deleted(params_after_vbem,'2.0').
$pp_prism_flag_deleted(reduce_copy,'2.0').
$pp_prism_flag_deleted(scaling,'2.0').
$pp_prism_flag_deleted(scaling_factor,'2.0').
$pp_prism_flag_deleted(smooth,'2.0').

%%----------------------------------------

get_prism_flag(Name,Value) :-
    $pp_prism_flag(Name,_,_,_),
    $pp_variable_prism_flag(Name,VarName),
    global_get(VarName,Value).
get_prism_flag(Name,Value) :-
    current_predicate($pp_prism_flag_renamed/2),
    $pp_prism_flag_renamed(Name0,Name1),
    Name == Name0,!,
    $pp_raise_warning($msg(3102),[Name,Name1]),
    $pp_variable_prism_flag(Name1,VarName),
    global_get(VarName,Value).

%%----------------------------------------

set_prism_flag(Name,Value) :-
    ( current_predicate($pp_prism_flag_deleted/2),
      $pp_prism_flag_deleted(Name,Version)
        -> $pp_raise_domain_error($msg(3103),[Name,Version],[prism_flag,Name],
                                  set_prism_flag/2)
    ; current_predicate($pp_prism_flag_deleted/3),
      $pp_prism_flag_deleted(Name,Value,Version)
        -> $pp_raise_domain_error($msg(3104),[Name,Value,Version],
                                  [prism_flag_value(Name),Value],set_prism_flag/2)
    ; true
    ),
    $pp_require_prism_flag(Name,$msg(3100),set_prism_flag/2),
    $pp_require_prism_flag_value(Name,Value,$msg(3101),set_prism_flag/2),
    ( $pp_prism_flag(Name,Type,_,Pred) ->
          Name1 = Name
    ; current_predicate($pp_prism_flag_renamed/2),
      $pp_prism_flag_renamed(Name,Name1),
      $pp_prism_flag(Name1,Type,_,Pred) ->
          $pp_raise_warning($msg(3102),[Name,Name1])
    ),
    $pp_check_prism_flag(Type,Value,SValue,IValue),
    $pp_disable_prism_flag(Name1),
    $pp_variable_prism_flag(Name1,VarName),
    global_set(VarName,SValue),
    ( Pred == $none -> true
    ; Term =.. [Pred,IValue], call(Term)
    ),!.

%%----------------------------------------

reset_prism_flags :-
    $pp_set_default_prism_flags,
    $pp_disable_exclusive_prism_flags.

$pp_set_default_prism_flags :-
    $pp_prism_flag(Name,_,Value,_),
    set_prism_flag(Name,Value),
    fail.
$pp_set_default_prism_flags.
    
$pp_disable_exclusive_prism_flags :-
    ( current_predicate($pp_prism_flag_exclusive/1),
      $pp_prism_flag_exclusive([_|Names]),
      $pp_disable_prism_flag1(Names),
      fail
    ; true
    ).

%%----------------------------------------

show_prism_flags :-
    get_prism_flag(Name,Value),
    ( Value = $disabled -> Value1 = '(disabled)'
    ; Value1 = Value
    ),
    format("~w~22|: ~w~n",[Name,Value1]),
    fail.
show_prism_flags.

%%----------------------------------------
%% aliases 

current_prism_flag(Name,Value) :- get_prism_flag(Name,Value).

show_prism_flag :- show_prism_flags.
show_flags      :- show_prism_flags.
show_flag       :- show_prism_flags.

$pp_variable_prism_flag(Name,VarName) :-
    atom_chars(Name,Name1),
    VarName1 = [$,p,g,'_',f,l,a,g,'_'|Name1],
    atom_chars(VarName,VarName1).

%%----------------------------------------

$pp_check_prism_flag(Type,Value,SValue,IValue) :- Type = bool ->
    nth0(IValue,[off,on],Value),!,
    SValue = Value.
$pp_check_prism_flag(Type,Value,SValue,IValue) :- Type = enum(Cands) ->
    nth0(IValue,Cands,Value),!,
    SValue = Value.
$pp_check_prism_flag(Type,Value,SValue,IValue) :- Type = term(Patts) ->
    member(Value,Patts),!,
    SValue = Value,
    IValue = Value.
$pp_check_prism_flag(Type,Value,SValue,IValue) :- Type = integer(Min,Max) ->
    integer(Value),
    $pp_check_min_max(Value,Min,Max),!,
    SValue = Value,
    IValue = Value.
$pp_check_prism_flag(Type,Value,SValue,IValue) :- Type = float(Min,Max) ->
    number(Value),
    $pp_check_min_max(Value,Min,Max),!,
    SValue = Value,
    IValue is float(Value).
$pp_check_prism_flag(Type,Value,SValue,IValue) :- Type = special(PredName) ->
    call(PredName,Value,SValue,IValue). % B-Prolog extension

$pp_check_min_max(Value,Min,Max) :-
    ( Min = -inf -> true
    ; Min = @Min0 -> Min0 < Value
    ; Min =< Value
    ),!,
    ( Max = +inf -> true
    ; Max = @Max0 -> Max0 > Value
    ; Max >= Value
    ),!.

$pp_check_max_iterate(0,inf,0) :- $pp_raise_warning($msg(3105),[0,inf]).
$pp_check_max_iterate(inf,inf,0).
$pp_check_max_iterate(default,default,-1).
$pp_check_max_iterate(N,N,N) :- integer(N), N > 0.

$pp_check_default_sw(V,V,V) :-
    ( V = f_geometric(B),   number(B), B > 1.0
    ; V = f_geometric(B,T), number(B), B > 1.0, member(T,[asc,desc])
    ; member(V,[none,uniform,f_geometric,random,noisy_u])
    ).

$pp_check_default_sw_a(V,V,V) :-
    ( number(V), V > 0.0
    ; V = uniform(U), number(U), U > 0.0
    ; member(V,[none,uniform])
    ).

$pp_check_default_sw_d(V,V,V) :-
    ( number(V), V >= 0.0
    ; V = uniform(U), number(U), U >= 0.0
    ; member(V,[none,uniform])
    ).

$pp_check_verb(none,none,[0,0]).
$pp_check_verb(em,em,[1,0]).
$pp_check_verb(graph,graph,[0,1]).
$pp_check_verb(full,full,[1,1]).
$pp_check_verb(off,none,[0,0]) :- $pp_raise_warning($msg(3105),[off,none]).
$pp_check_verb(on,full,[1,1])  :- $pp_raise_warning($msg(3105),[on,full]).

$pp_check_write_call_events(X,Y,Y) :- $pp_write_call_events(X,Y),!.
$pp_check_write_call_events(off,off,off) :- !.

$pp_check_learn_message(X,Y,Y) :- $pp_learn_message_events(X,Y),!.

$pp_check_mcmc_message(X,Y,Y)  :- $pp_mcmc_message_events(X,Y),!.

%% disable competitors

$pp_disable_prism_flag(Name) :-
    ( current_predicate($pp_prism_flag_exclusive/1),
      $pp_prism_flag_exclusive(Competitors),
      select(Name,Competitors,Names),  % B-Prolog's built-in
      $pp_disable_prism_flag1(Names),
      fail
    ; true
    ).

$pp_disable_prism_flag1([]).
$pp_disable_prism_flag1([Name|Names]) :-
    $pp_variable_prism_flag(Name,VarName),
    global_set(VarName,$disabled),!,
    $pp_disable_prism_flag1(Names).

%% check the availability of the flag (Note: Name must be ground)
$pp_is_enabled_flag(Name) :-
    \+ get_prism_flag(Name,$disabled).

%%----------------------------------------
%% auxiliary predicates

$pp_set_verb([EM,Graph]) :-
    $pc_set_verb_em(EM),
    $pc_set_verb_graph(Graph).
%%----------------------------------------
%%  error/warning (obsolete)

$pp_err_msg(Msg) :-
    format("{PRISM ERROR: ",[]),write(Msg),format("}~n",[]),!,
    abort.
$pp_err_msg(Msg,Vars) :-
    format("{PRISM ERROR: ",[]),format(Msg,Vars),format("}~n",[]),!,
    abort.

$pp_warn_msg(Msg) :-
    ( get_prism_flag(warn,on) ->
        format("{PRISM WARNING: ",[]),write(Msg),format("}~n",[])
    ; true
    ).
$pp_warn_msg(Msg,Vars) :-
    ( get_prism_flag(warn,on) ->
        format("{PRISM WARNING: ",[]),format(Msg,Vars),format("}~n",[])
    ; true
    ).


%%----------------------------------------
%%  internal utils

%% probabilistic formulas

$pp_is_user_probabilistic_atom(Goal) :-
    callable(Goal),
    functor(Goal,F,N),
    $pd_is_prob_pred(F,N),!.

$pp_is_probabilistic_atom(Goal) :-
    ( nonvar(Goal),
        ( Goal ?= msw(_,_)
        ; Goal ?= soft_msw(_,_)
        ; Goal ?= b_msw(_,_)
        )
    ; $pp_is_user_probabilistic_atom(Goal)
    ),!.

$pp_is_extended_probabilistic_atom(Goal) :-
    ( $pp_is_probabilistic_atom(Goal)
    ; $pp_is_dummy_goal(Goal)
    ),!.

$pp_is_probabilistic_callable(Goal) :-
    callable(Goal),
    $pp_is_probabilistic_callable_aux(Goal),!.

$pp_is_probabilistic_callable_aux((G1,G2)) =>
    ( $pp_is_probabilistic_callable_aux(G1),callable(G2)
    ; callable(G1),$pp_is_probabilistic_callable_aux(G2)
    ).
$pp_is_probabilistic_callable_aux((G1;G2)) =>
    ( $pp_is_probabilistic_callable_aux(G1),callable(G2)
    ; callable(G1),$pp_is_probabilistic_callable_aux(G2)
    ).
$pp_is_probabilistic_callable_aux((C->A;B)) =>
    ( $pp_is_probabilistic_callable_aux(C),callable(A),callable(B)
    ; callable(C),$pp_is_probabilistic_callable_aux(A),callable(B)
    ; callable(C),callable(A),$pp_is_probabilistic_callable_aux(B)
    ).
$pp_is_probabilistic_callable_aux(not(G)) =>
    $pp_is_probabilistic_callable_aux(G).
$pp_is_probabilistic_callable_aux(\+(G)) =>
    $pp_is_probabilistic_callable_aux(G).
$pp_is_probabilistic_callable_aux((C->A)) =>
    ( $pp_is_probabilistic_callable_aux(C),callable(A)
    ; callable(C),$pp_is_probabilistic_callable_aux(A)
    ).
$pp_is_probabilistic_callable_aux(write_call(G)) =>
    $pp_is_probabilistic_callable_aux(G).
$pp_is_probabilistic_callable_aux(write_call(_Opts,G)) =>
    $pp_is_probabilistic_callable_aux(G).
$pp_is_probabilistic_callable_aux((?? G)) =>
    $pp_is_probabilistic_callable_aux(G).
$pp_is_probabilistic_callable_aux((??* G)) =>
    $pp_is_probabilistic_callable_aux(G).
$pp_is_probabilistic_callable_aux((??> G)) =>
    $pp_is_probabilistic_callable_aux(G).
$pp_is_probabilistic_callable_aux((??< G)) =>
    $pp_is_probabilistic_callable_aux(G).
$pp_is_probabilistic_callable_aux((??+ G)) =>
    $pp_is_probabilistic_callable_aux(G).
$pp_is_probabilistic_callable_aux((??- G)) =>
    $pp_is_probabilistic_callable_aux(G).
$pp_is_probabilistic_callable_aux(G) :- 
    $pp_is_extended_probabilistic_atom(G).

%% tabled probabilistic formulas

$pp_is_tabled_probabilistic_atom(Goal) :-
    callable(Goal),
    functor(Goal,F,N),
    $pd_is_tabled_pred(F,N),!.

%% goals that can be handled with the write_call predicates

$pp_is_write_callable(Goal) :-
   ( Goal = '!'    -> fail
   ; Goal = (A,B)  -> $pp_is_write_callable(A), $pp_is_write_callable(B)
   ; Goal = (_;_)  -> fail
   ; Goal = \+(_)  -> fail
   ; Goal = not(_) -> fail
   ; Goal = (_->_) -> fail
   ; true
   ).

%% dummy goals

$pp_create_dummy_goal(DummyGoal) :-
    global_get($pg_dummy_goal_count,N0),
    N1 is N0 + 1,
    global_set($pg_dummy_goal_count,N1),!,
    $pp_create_dummy_goal(N0,DummyGoal),!.

$pp_create_dummy_goal(N,DummyGoal) :-
    number_chars(N,NChars),
    append(['$',p,d,'_',d,u,m,m,y],NChars,DummyGoalChars),
    atom_chars(DummyGoal,DummyGoalChars).

$pp_is_dummy_goal(G) :-
    atom(G),
    atom_chars(G,GChars),
    GChars = ['$',p,d,'_',d,u,m,m,y|_].

%% option analyzer

$pp_proc_opts(Opts,Pred,Vars,Defaults,Source) :-
    $pp_require_list_or_nil(Opts,$msg(2109),Source),
    $pp_proc_opts_core(Opts,Pred,Vars,Defaults,Source).

$pp_proc_opts_core([],_,[],[],_Source) :- !.
$pp_proc_opts_core([],Pred,[Var|Vars],[Default|Defaults],Source) :-
    ( Var = Default ; true ),!,
    $pp_proc_opts_core([],Pred,Vars,Defaults,Source).
$pp_proc_opts_core([Opt|Opts],Pred,Vars,Defaults,Source) :-
    nonvar(Opt),
    Clause =.. [Pred,Opt,Pos,Val],
    call(Clause),
    nth1(Pos,Vars,Var),
    ( var(Var) -> Var = Val
    ; $pp_raise_runtime_error($msg(3003),[Opt],duplicate_option,Source)
    ),!,
    $pp_proc_opts_core(Opts,Pred,Vars,Defaults,Source).
$pp_proc_opts_core([Opt|_],_,_,_,Source) :-
    $pp_raise_runtime_error($msg(3002),[Opt],unknown_option,Source).

%% sorting with duplicate elements remained

$pp_sort_remain_dup(L0,L) :- sort('=<',L0,L).


%%----------------------------------------
%%  statistics

show_goals :-
    global_get($pg_observed_facts,GoalCountPairs0),!,
    sort(GoalCountPairs0,GoalCountPairs),
    $pp_find_total_count(GoalCountPairs,0,Total),
    $pp_show_goals(GoalCountPairs,Total).
show_goals :-
    $pp_raise_runtime_error($msg(3004),observation_not_found,show_goals/0).

$pp_find_total_count([],Total,Total).
$pp_find_total_count([goal(_Goal,Count)|GoalCountPairs],Total0,Total) :-
    Total1 is Total0 + Count,!,
    $pp_find_total_count(GoalCountPairs,Total1,Total).

$pp_show_goals([],Total) :- format("Total_count=~w~n",[Total]).
$pp_show_goals([goal(DummyGoal,Count)|GoalCountPairs],Total) :-
    P is Count / Total * 100,
    ( current_predicate($pd_dummy_goal_table/2),
      $pd_dummy_goal_table(DummyGoal,Goal)
        -> true
    ; Goal = DummyGoal
    ),
    format("Goal ~w (count=~w, freq=~3f%)~n",[Goal,Count,P]),
    $pp_show_goals(GoalCountPairs,Total).

get_goals(Gs) :-
    findall(Goal,$pp_get_one_goal(Goal),Gs0),
    sort(Gs0,Gs).

$pp_get_one_goal(Goal) :-
    ( global_get($pg_observed_facts,GoalCountPairs) ->
        $pp_get_one_goal(Goal,GoalCountPairs)
    ; $pp_raise_runtime_error($msg(3004),observation_not_found,show_goals/0)
    ).

$pp_get_one_goal(Goal,[goal(DummyGoal,_Count)|_]) :-
    current_predicate($pd_dummy_goal_table/2),
    $pd_dummy_goal_table(DummyGoal,Goal).
$pp_get_one_goal(Goal,[goal(Goal,_Count)|_]).
$pp_get_one_goal(Goal,[_|Pairs]) :- $pp_get_one_goal(Goal,Pairs).

get_goal_counts(GCounts) :-
    findall([Goal,Count,Freq],$pp_get_one_goal_count(Goal,Count,Freq),GCounts0),
    sort(GCounts0,GCounts).

$pp_get_one_goal_count(Goal,Count,Freq) :-
    ( global_get($pg_observed_facts,GoalCountPairs) ->
        $pp_find_total_count(GoalCountPairs,0,Total),
        $pp_get_one_goal_count(Goal,Count,Freq,GoalCountPairs,Total)
    ; $pp_raise_runtime_error($msg(3004),observation_not_found,show_goals/0)
    ).

$pp_get_one_goal_count(Goal,Count,Freq,[goal(DummyGoal,Count)|_],Total) :-
    current_predicate($pd_dummy_goal_table/2),
    $pd_dummy_goal_table(DummyGoal,Goal),
    Freq is Count / Total * 100.
$pp_get_one_goal_count(Goal,Count,Freq,[goal(Goal,Count)|_],Total) :-
    Freq is Count / Total * 100.
$pp_get_one_goal_count(Goal,Count,Freq,[_|Pairs],Total) :-
    $pp_get_one_goal_count(Goal,Count,Freq,Pairs,Total).

prism_statistics(Name,L) :-
    ( graph_statistics(Name,L)
    ; learn_statistics(Name,L)
    ; infer_statistics(Name,L)
    ; mcmc_statistics(Name,L)
    ).

graph_statistics(Name,L) :-
    ( \+ $ps_num_subgraphs(_) -> fail
    ; Name = num_subgraphs,
        ( $ps_num_subgraphs(L) -> true )
    ; Name = num_nodes,
        ( $ps_num_nodes(L) -> true )
    ; Name = num_goal_nodes,
        ( $ps_num_goal_nodes(L) -> true )
    ; Name = num_switch_nodes,
        ( $ps_num_switch_nodes(L) -> true )
    ; Name = avg_shared,
        ( $ps_avg_shared(L) -> true )
    ).

learn_statistics(Name,L) :-
    ( \+ $ps_learn_time(_) -> fail
    ; Name = log_likelihood,
        ( $ps_log_likelihood(L) -> true )
    ; Name = log_post,
        ( $ps_log_post(L) -> true )
    ; Name = log_prior,
        ( $ps_log_post(LPost), $ps_log_likelihood(LogLike) -> L is LPost - LogLike )
    ; Name = lambda,
        ( ( $ps_log_post(L) ; $ps_log_likelihood(L) ) -> true )
    ; Name = num_switches,
        ( $ps_num_switches(L) -> true )
    ; Name = num_switch_values,
        ( $ps_num_switch_values(L) -> true )
    ; Name = num_parameters,
        ( $ps_num_switches(N0), $ps_num_switch_values(N1) -> L is N1 - N0 )
    ; Name = num_iterations,
        ( $ps_num_iterations(L) -> true )
    ; Name = num_iterations_vb,
        ( $ps_num_iterations_vb(L) -> true )
    ; Name = goals,
        ( is_global($pg_observed_facts) -> get_goals(L) )
    ; Name = goal_counts,
        ( is_global($pg_observed_facts) -> get_goal_counts(L) )
    ; Name = bic,
        ( $ps_bic_score(L) -> true )
    ; Name = cs,
        ( $ps_cs_score(L) -> true )
    ; Name = free_energy,
        ( $ps_free_energy(L) -> true )
    ; Name = learn_time,
        ( $ps_learn_time(L) -> true )
    ; Name = learn_search_time,
        ( $ps_learn_search_time(L) -> true )
    ; Name = em_time,
        ( $ps_em_time(L) -> true )
    ).

infer_statistics(Name,L) :-
    ( \+ $ps_infer_time(_) -> fail
    ; Name = infer_time,
        ( $ps_infer_time(L) -> true )
    ; Name = infer_search_time,
        ( $ps_infer_search_time(L) -> true )
    ; Name = infer_calc_time,
        ( $ps_infer_calc_time(L) -> true )
    ).

mcmc_statistics(Name,L) :-
    ( Name = mcmc_sample_time,
        ( $ps_mcmc_sample_time(L) -> true )
    ; Name = mcmc_marg_time,
        ( $ps_mcmc_marg_time(L) -> true )
    ; Name = mcmc_pred_time,
        ( $ps_mcmc_pred_time(L) -> true )
    ; Name = mcmc_exact_time,
        ( $ps_mcmc_exact_time(L) -> true )
    ).

prism_statistics :-
    format("Statistics in PRISM:~n",[]),!,
    ( prism_statistics(Name,L),
      $pp_print_one_statistic(Name,L),
      fail
    ; true
    ),!.

learn_statistics :-
    format("Statistics on learning:~n",[]),!,
    ( learn_statistics(Name,L),
      $pp_print_one_statistic(Name,L),
      fail
    ; true
    ),!.

graph_statistics :-
    format("Statistics on the size of the explanation graphs:~n",[]),!,
    ( graph_statistics(Name,L),
      $pp_print_one_statistic(Name,L),
      fail
    ; true
    ),!.

infer_statistics :-
    format("Statistics on inference:~n",[]),!,
    ( infer_statistics(Name,L),
      $pp_print_one_statistic(Name,L),
      fail
    ; true
    ),!.

mcmc_statistics :-
    format("Statistics on MCMC:~n",[]),!,
    ( mcmc_statistics(Name,L),
      $pp_print_one_statistic(Name,L),
      fail
    ; true
    ),!.

$pp_print_one_statistic(Name,L) :-
    ( Name = goals       -> format("  ~w~24|: (run show_goals/0)~n",[Name])
    ; Name = goal_counts -> format("  ~w~24|: (run show_goals/0)~n",[Name])
    ; float(L)           -> format("  ~w~24|: ~9g~n",[Name,L])
    ; format("  ~w~24|: ~w~n",[Name,L])
    ).


%%----------------------------------------
%%  wrapper of reader/writer routines

% close Stream anyway after calling Goal
$pp_call_with_stream(Stream,Goal) :-
    ( catch(Goal,C,(close(Stream),throw(C)))
    ; true
    ),
    close(Stream),!.


%%----------------------------------------
%%  clause list reader/writer

load_clauses(FileName,Clauses) :-
    load_clauses(FileName,Clauses,[]).

load_clauses(FileName,Clauses,From,Size) :-
    $pp_raise_warning($msg(3300),[load_clauses/4,load_clauses/3]),
    load_clauses(FileName,Clauses,[from(From),size(Size)]).

load_clauses(FileName,Clauses,Opts) :-
    $pp_require_atom(FileName,$msg(3000),load_clauses/3),
    $pp_proc_opts(Opts,$load_clauses_option,
                  [From,Size],
                  [0   ,max ],
                  load_clauses/3),
    open(FileName,read,Stream),
    $pp_call_with_stream(Stream,
                         $pp_load_clauses_core(Stream,Clauses,From,Size)),!.

$load_clauses_option(from(N),1,N) :-
    integer(N),N >= 0.
$load_clauses_option(skip(N),1,N) :-
    integer(N),N >= 0.
$load_clauses_option(size(N),2,N) :-
    integer(N),N >= 0 ; N == max.

$pp_load_clauses_core(_,[],_,0).
$pp_load_clauses_core(S,Xs,K,N) :-
    $pp_load_clauses_read(S,X),!,
    ( K > 0    -> Xs = Xs1,     K1 is K - 1, N1 = N
    ; N == max -> Xs = [X|Xs1], K1 = K,      N1 = N
    ;             Xs = [X|Xs1], K1 = K,      N1 is N - 1
    ),!,
    $pp_load_clauses_core(S,Xs1,K1,N1).
$pp_load_clauses_core(_,[],K,N) :-
    ( K =< 0, N == max -> true
    ; $pp_raise_warning($msg(3008))
    ).

$pp_load_clauses_read(S,X) :-
    read(S,X),!,X \== end_of_file.

save_clauses(FileName,Clauses) :-
    save_clauses(FileName,Clauses,[]).

save_clauses(FileName,Clauses,From,Size) :-
    $pp_raise_warning($msg(3300),[save_clauses/4,save_clauses/3]),
    save_clauses(FileName,Clauses,[from(From),size(Size)]).

save_clauses(FileName,Clauses,Opts) :-
    $pp_require_atom(FileName,$msg(3000),save_clauses/3),
    $pp_require_list_or_nil(Clauses,$msg(2109),save_clauses/3),
    $pp_proc_opts(Opts,$load_clauses_option,
                  [From,Size],
                  [0   ,max ],
                  save_clauses/3),
    open(FileName,write,Stream),
    $pp_call_with_stream(Stream,
                         $pp_save_clauses_core(Stream,Clauses,From,Size)),!.

$pp_save_clauses_core(_,_,_,0) :- !.
$pp_save_clauses_core(S,[X|Xs1],K,N) :-
    ( K > 0  ->                          K1 is K-1, N1 = N
    ; N == max -> format(S,"~q.~n",[X]), K1 = K,    N1 = N 
    ;             format(S,"~q.~n",[X]), K1 = K,    N1 is N-1
    ),!,
    $pp_save_clauses_core(S,Xs1,K1,N1).
$pp_save_clauses_core(_,[],K,N) :-
    ( K =< 0, N == max -> true
    ; $pp_raise_warning($msg(3008))
    ),!.

%%----------------------------------------
%%  csv loader [RFC 4180]

load_csv(FileName,Rows) :-
    load_csv(FileName,Rows,[]).

load_csv(FileName,Rows,Opts) :-
    $pp_require_atom(FileName,$msg(3000),load_csv/3),
    $pp_proc_opts(Opts,$pp_load_csv_option,
                  [RFrom,RSize,CFrom,CSize,Pred,Conv,Quot,Cmnt,Miss],
                  [0,max,0,max,csvrow/1,1,34,none,_],
                  load_csv/3),
    open(FileName,read,Stream),
    $pp_call_with_stream(Stream,
                         $pp_load_csv_core(Stream,Rows,
                                           RFrom,RSize,CFrom,CSize,
                                           Pred,Conv,Quot,Cmnt,Miss)),!.

$pp_load_csv_option(row_from(N),1,N) :-
    integer(N),N >= 0.
$pp_load_csv_option(row_skip(N),1,N) :-
    integer(N),N >= 0.
$pp_load_csv_option(row_size(N),2,N) :-
    integer(N),N >= 0 ; N == max.
$pp_load_csv_option(col_from(N),3,N) :-
    integer(N),N >= 0.
$pp_load_csv_option(col_skip(N),3,N) :-
    integer(N),N >= 0.
$pp_load_csv_option(col_size(N),4,N) :-
    integer(N),N >= 0 ; N == max.

$pp_load_csv_option(pred(X),5,Pred) :-
    ( X == [] -> Pred = []/0
    ; atom(X) -> Pred = X/1
    ; X = P/N -> atom(P),(N == 1;N == n),Pred = P/N
    ).

$pp_load_csv_option(parse_number(X),6,Flag) :-
    ( X == yes -> Flag = 1 ; X == no -> Flag = 0 ).
    
$pp_load_csv_option(double_quote(X),7,Code) :-
    ( X == yes -> Code = 34 ; X == no -> Code = none ).

$pp_load_csv_option(comment(X),8,Code) :-
    atom(X),atom_length(X,1),char_code(X,Code).
$pp_load_csv_option(comment,8,35).

$pp_load_csv_option(missing(X),9,Codes) :-
    atom(X),atom_codes(X,Codes).
$pp_load_csv_option(missing,9,'').

$pp_load_csv_core(_,[],_,0,_,_,_,_,_,_,_).
$pp_load_csv_core(S,Xs,K,N,J,M,Pred,Conv,Quot,Cmnt,Miss) :-
    $pp_load_csv_read(S,Row0,Conv,Quot,Cmnt,Miss),!,
    ( K > 0 -> Xs = Xs1, K1 is K - 1, N1 = N
    ; $pp_load_csv_extract(Row0,Row,J,M),
      Pred = Name/Style,
      ( Style == 0 -> X = Row
      ; Style == 1 -> X =.. [Name,Row]
      ; Style == n -> X =.. [Name|Row]
      ),
      ( N == max -> Xs = [X|Xs1], K1 = K, N1 = N
      ;             Xs = [X|Xs1], K1 = K, N1 is N-1
      )
    ),!,
    $pp_load_csv_core(S,Xs1,K1,N1,J,M,Pred,Conv,Quot,Cmnt,Miss).
$pp_load_csv_core(_,[],K,N,_,_,_,_,_,_,_) :-
    ( K =< 0, N == max -> true
    ; $pp_raise_runtime_error($msg(3005),invalid_csv_format,load_csv/3)
    ).

$pp_load_csv_extract(Row0,Row1,J,M), M  == max =>
    $pp_load_csv_extract_step1(Row0,Row1,J).
$pp_load_csv_extract(Row0,Row2,J,M), M \== max =>
    $pp_load_csv_extract_step1(Row0,Row1,J),
    $pp_load_csv_extract_step2(Row1,Row2,M).

$pp_load_csv_extract_step1(Xs,Xs,0).
$pp_load_csv_extract_step1([_|Xs],Ys,J) :-
    J1 is J-1,!,$pp_load_csv_extract_step1(Xs,Ys,J1).
$pp_load_csv_extract_step1(_,_,_) :-
    $pp_raise_runtime_error($msg(3006),invalid_csv_format,load_csv/3).

$pp_load_csv_extract_step2(_,[],0).
$pp_load_csv_extract_step2([Z|Xs],[Z|Ys],M) :-
    M1 is M-1,!,$pp_load_csv_extract_step2(Xs,Ys,M1).
$pp_load_csv_extract_step2(_,_,_) :-
    $pp_raise_runtime_error($msg(3006),invalid_csv_format,load_csv/3).

$pp_load_csv_read(S,Row,Conv,Quot,Cmnt,Miss) :-
    $pp_load_csv_skip(S,Cmnt),!,$pp_load_csv_q0(S,Conv,Miss,Quot,Row-[],Any-Any).

$pp_load_csv_skip(S,Cm) :-
    peek_code(S,Code),
    ( Code == -1 -> fail
    ; Code == Cm -> $pp_load_csv_skip(S),!,$pp_load_csv_skip(S,Cm)
    ; true
    ).

$pp_load_csv_skip(S) :-
    get_code(S,Code),
    ( Code =:= -1 -> fail
    ; Code =:= 10 -> true
    ; Code =:= 13 -> $pp_load_csv_crlf(S)
    ; $pp_load_csv_skip(S)
    ).

$pp_load_csv_crlf(S) :-
    ( peek_code(S,10) -> get_code(S,10) ; true ).

%%  3rd arg. = parse numeric values?
%%  4th arg. = missing value

$pp_load_csv_done(_,Codes-[],_,M) :-
    nonvar(M),Codes = M,!.
$pp_load_csv_done(Value,Codes-[],1,_) :-
    forall(member(Code,Codes),(32=<Code,Code<128)),
    catch(number_codes(Value,Codes),_,fail),!.
$pp_load_csv_done(Value,Codes-[],_,_) :-
    atom_codes(Value,Codes).

$pp_load_csv_q0(S,Cv,Ms,Dq,Xs-Xs0,Ys-Ys0) :-
    get_code(S,Code),
    ( Code == -1 ->             % EOF
        $pp_load_csv_done(X,Ys-Ys0,Cv,Ms),Xs = [X|Xs0],!
    ; Code == 10 ->             % LF
        $pp_load_csv_done(X,Ys-Ys0,Cv,Ms),Xs = [X|Xs0],!
    ; Code == 13 ->             % CR
        $pp_load_csv_done(X,Ys-Ys0,Cv,Ms),Xs = [X|Xs0],!,$pp_load_csv_crlf(S)
    ; Code == 44 ->             % ,
        $pp_load_csv_done(X,Ys-Ys0,Cv,Ms),Xs = [X|Xs1],!,
        $pp_load_csv_q0(S,Cv,Ms,Dq,Xs1-Xs0,Any-Any)
    ; Code == Dq ->             % "
        !,$pp_load_csv_q2(S,Cv,Ms,Dq,Xs-Xs0,Ys-Ys0)
    ;                           % ELSE
        Ys0 = [Code|Ys1],!,$pp_load_csv_q1(S,Cv,Ms,Dq,Xs-Xs0,Ys-Ys1)
    ).

$pp_load_csv_q1(S,Cv,Ms,Dq,Xs-Xs0,Ys-Ys0) :-
    get_code(S,Code),
    ( Code == -1 ->             % EOF
        $pp_load_csv_done(X,Ys-Ys0,Cv,Ms),Xs = [X|Xs0],!
    ; Code == 10 ->             % LF
        $pp_load_csv_done(X,Ys-Ys0,Cv,Ms),Xs = [X|Xs0],!
    ; Code == 13 ->             % CR
        $pp_load_csv_done(X,Ys-Ys0,Cv,Ms),Xs = [X|Xs0],!,$pp_load_csv_crlf(S)
    ; Code == 44 ->             % ,
        $pp_load_csv_done(X,Ys-Ys0,Cv,Ms),Xs = [X|Xs1],!,
        $pp_load_csv_q0(S,Cv,Ms,Dq,Xs1-Xs0,Any-Any)
    ; Code == Dq ->             % "
        close(S),!,
        $pp_raise_runtime_error($msg(3007),invalid_csv_format,load_csv/3)
    ;                           % ELSE
        Ys0 = [Code|Ys1],!,$pp_load_csv_q1(S,Cv,Ms,Dq,Xs-Xs0,Ys-Ys1)
    ).

$pp_load_csv_q2(S,Cv,Ms,Dq,Xs-Xs0,Ys-Ys0) :-
    get_code(S,Code),
    ( Code == -1 ->             % EOF
        close(S),!,
        $pp_raise_runtime_error($msg(3007),invalid_csv_format,load_csv/3)
    ; Code == Dq ->             % "
        !,$pp_load_csv_q3(S,Cv,Ms,Dq,Xs-Xs0,Ys-Ys0)
    ;                           % ELSE
        Ys0 = [Code|Ys1],!,$pp_load_csv_q2(S,Cv,Ms,Dq,Xs-Xs0,Ys-Ys1)
    ).

$pp_load_csv_q3(S,Cv,Ms,Dq,Xs-Xs0,Ys-Ys0) :-
    get_code(S,Code),
    ( Code == -1 ->             % EOF
        $pp_load_csv_done(X,Ys-Ys0,Cv,Ms),Xs = [X|Xs0],!
    ; Code == 10 ->             % LF
        $pp_load_csv_done(X,Ys-Ys0,Cv,Ms),Xs = [X|Xs0],!
    ; Code == 13 ->             % CR
        $pp_load_csv_done(X,Ys-Ys0,Cv,Ms),Xs = [X|Xs0],!,$pp_load_csv_crlf(S)
    ; Code == 44 ->             % ,
        $pp_load_csv_done(X,Ys-Ys0,Cv,Ms),Xs = [X|Xs1],!,
        $pp_load_csv_q0(S,Cv,Ms,Dq,Xs1-Xs0,Any-Any)
    ; Code == Dq ->             % "
        Ys0 = [Code|Ys1],!,$pp_load_csv_q2(S,Cv,Ms,Dq,Xs-Xs0,Ys-Ys1)
    ;                           % ELSE
        close(S),!,
        $pp_raise_runtime_error($msg(3007),invalid_csv_format,load_csv/3)
    ).


%%----------------------------------------
%%  csv saver [RFC 4180]

save_csv(FileName,Rows) :-
    save_csv(FileName,Rows,[]).

save_csv(FileName,Rows,Opts) :-
    $pp_require_atom(FileName,$msg(3000),save_csv/3),
    $pp_proc_opts(Opts,$pp_save_csv_option,[Quote],[2],save_csv/3),
    open(FileName,write,Stream),
    $pp_call_with_stream(Stream,$pp_save_csv(Stream,Quote,Rows)).

$pp_save_csv_option(quote(X),1,Flag) :-
    ( X == none   -> Flag = 0
    ; X == single -> Flag = 1
    ; X == double -> Flag = 2
    ).

$pp_save_csv(_,_,[]).
$pp_save_csv(Stream,Q,[Row|Rows]) :-
    $pp_save_csv_aux(Stream,Q,Row),!,
    $pp_save_csv(Stream,Q,Rows).

$pp_save_csv_aux(Stream,_,[]) :-
    nl(Stream).
$pp_save_csv_aux(Stream,Q,[Col]) :- !,
    $pp_save_csv_term(Stream,Q,Col),
    nl(Stream).
$pp_save_csv_aux(Stream,Q,[Col|Cols]) :-
    $pp_save_csv_term(Stream,Q,Col),
    write(Stream,','),!,
    $pp_save_csv_aux(Stream,Q,Cols).

$pp_save_csv_term(Stream,Q,Term) :-
    $pp_require_atomic(Term,$msg(3009),save_csv/3),
    ( Q == 0 -> write(Stream,Term)
    ; Q == 1 -> writeq(Stream,Term)
    ; $pp_save_csv_dquote(Stream,Term)
    ).

$pp_save_csv_dquote(Stream,Term) :-
    $pp_save_csv_dquote(Term,Codes,DQ),
    ( DQ == 1 ->
        write(Stream,'"'),format(Stream,"~s",Codes),write(Stream,'"')
    ; % else
        write(Stream,Term)
    ).

% we do not check Term deeply if it is known to be a number
$pp_save_csv_dquote(Term,_,0) :- number(Term),!.
$pp_save_csv_dquote(Term,Codes,DQ) :-
    atom_codes(Term,Codes0),
    $pp_save_csv_dquote_aux(Codes0,Codes,DQ).

$pp_save_csv_dquote_aux([],[],0).
$pp_save_csv_dquote_aux([C|Codes0],[C|Codes],DQ) :-
    % 44:comma, 13:CR, 10:LF, 34:double-quotation
    ( (C == 44 ; C == 13 ; C == 10 ; C == 34) -> DQ = 1
    ; DQ = DQ1
    ),
    ( C == 34 -> Codes = [34|Codes1]  % escape double-quotations
    ; Codes = Codes1
    ),!,
    $pp_save_csv_dquote_aux(Codes0,Codes1,DQ1).


%%----------------------------------------
%%  pretty e-graph printer

print_graph(G) :-
    current_output(S),print_graph(S,G, [] ).

print_graph(G,Opts) :-
    current_output(S),print_graph(S,G,Opts).

print_graph(S,G,Opts) :-
    $pp_require_list(G,$msg(2104),print_graph/3),
    $pp_proc_opts(Opts,$pp_print_graph_option,
                  [Lr0,And,Or0],
                  ["" ,"&","v"],
                  pring_graph/3),!,
    ( Lr0 == "" -> Colon = ":" ; Colon = "" ),
    length(Lr0,LenLr),
    length(Or0,LenOr),
    PadLr is LenOr-LenLr,$pp_print_graph_pad(Lr0,Lr,PadLr),
    PadOr is LenLr-LenOr,$pp_print_graph_pad(Or0,Or,PadOr),!,
    $pp_print_graph_roots(S,G,Colon,Lr,And,Or).

$pp_print_graph_option(lr(T) ,1,S) :- $pp_print_graph_optarg(T,S).
$pp_print_graph_option(and(T),2,S) :- $pp_print_graph_optarg(T,S).
$pp_print_graph_option(or(T) ,3,S) :- $pp_print_graph_optarg(T,S).

$pp_print_graph_optarg(T,S) :-
    ( atom(T) -> atom_codes(T,S)
    ; length(T,_),forall(member(X,T),(integer(X),0=<X,X=<255)) -> T = S
    ).

$pp_print_graph_pad(Xs,Ys,N), N =< 0 => Xs = Ys.
$pp_print_graph_pad(Xs,Ys,N), N  > 0 => Ys = [32|Ys1], N1 is N-1, !, $pp_print_graph_pad(Xs,Ys1,N1).

$pp_print_graph_roots(_,[],_,_,_,_).
$pp_print_graph_roots(S,[node(L,[])|Nodes],Colon,Lr,And,Or) :-
    format(S,"~w~n",[L]),!,
    $pp_print_graph_roots(S,Nodes,Colon,Lr,And,Or).
$pp_print_graph_roots(S,[node(L,Paths)|Nodes],Colon,Lr,And,Or) :-
    format(S,"~w~s~n",[L,Colon]),
    $pp_print_graph_paths(S,Paths,Lr,And,Or),!,
    $pp_print_graph_roots(S,Nodes,Colon,Lr,And,Or).
$pp_print_graph_roots(S,[node(L,[],V)|Nodes],Colon,Lr,And,Or) :-
    ( V = [V1,V2] ->
      format(S,"~w [~6g,~6g]~n",[L,V1,V2])
    ; format(S,"~w [~6g]~n",[L,V])
    ),!,
    $pp_print_graph_roots(S,Nodes,Colon,Lr,And,Or).
$pp_print_graph_roots(S,[node(L,Paths,V)|Nodes],Colon,Lr,And,Or) :-
    ( V = [V1,V2] ->
      format(S,"~w [~6g,~6g]~s~n",[L,V1,V2,Colon])
    ; format(S,"~w [~6g]~s~n",[L,V,Colon])
    ),!,
    $pp_print_graph_paths_aux(S,Paths,Lr,And,Or),!,
    $pp_print_graph_roots(S,Nodes,Colon,Lr,And,Or).

$pp_print_graph_paths(_,[],_,_,_).
$pp_print_graph_paths(_,[path([],[])],_,_,_) :- !.
$pp_print_graph_paths(S,[path(TNodes,SNodes)|Paths],Conn,And,Or) :-
    write(S,'  '),
    append(TNodes,SNodes,Nodes),
    $pp_print_graph_nodes(S,Nodes,Conn,And),
    nl(S),!,
    $pp_print_graph_paths(S,Paths,Or,And,Or).

$pp_print_graph_nodes(_,[],_,_).
$pp_print_graph_nodes(S,[Node|Nodes],Conn,And) :-
    format(S," ~s ~w",[Conn,Node]),!,
    $pp_print_graph_nodes(S,Nodes,And,And).

$pp_print_graph_paths_aux(_,[],_,_,_).
$pp_print_graph_paths_aux(_,[path([],[],_)],_,_,_) :- !.
$pp_print_graph_paths_aux(S,[path(TNodes,SNodes,V)|Paths],Conn,And,Or) :-
    write(S,'  '),
    append(TNodes,SNodes,Nodes),
    $pp_print_graph_nodes_aux(S,Nodes,Conn,And),
    write(S,'  '),
    ( V = [V1,V2] ->
      format(S,"{~6g,~6g}",[V1,V2])
    ; format(S,"{~6g}",[V])
    ),
    nl(S),!,
    $pp_print_graph_paths_aux(S,Paths,Or,And,Or).

$pp_print_graph_nodes_aux(_,[],_,_).
$pp_print_graph_nodes_aux(S,[Node|Nodes],Conn,And) :-
    ( Node = gnode(Label,Value) ; Node = snode(Label,Value) ),
    ( Value = [Value1,Value2] ->
      format(S," ~s ~w [~6g,~6g]",[Conn,Label,Value1,Value2])
    ; format(S," ~s ~w [~6g]",[Conn,Label,Value])
    ),!,
    $pp_print_graph_nodes_aux(S,Nodes,And,And).


%%----------------------------------------
%%  pretty tree printer

print_tree(T) :-
    current_output(S),print_tree(S,T,[]).

print_tree(T,Opts) :-
    current_output(S),print_tree(S,T,Opts).

print_tree(S,T,Opts) :-
    $pp_require_list(T,$msg(2104),print_tree/3),
    $pp_proc_opts(Opts,$pp_opts_print_tree,[Indent],[3],print_tree/3),
    number_codes(Indent,Format0),
    append("~",Format0,Format1),
    append(Format1,"s",Format2),
    $pp_print_tree_root(S,T,0,Format2).

$pp_opts_print_tree(indent(N),1,N) :-
    integer(N), N >= 1.

$pp_print_tree_root(S,[L|Sibs],K,Format) :-
    $pp_print_tree_node(S,L,K,Format),
    K1 is K + 1, !,
    $pp_print_tree_sibs(S,Sibs,K1,Format).

$pp_print_tree_sibs(_,Xs,_,_), Xs = [] =>
    true.
$pp_print_tree_sibs(S,Xs,K,Format), Xs = [X|Xs1] =>
    ( X ?= [_|_] ->
      $pp_print_tree_root(S,X,K,Format)
    ; $pp_print_tree_node(S,X,K,Format)
    ), !,
    $pp_print_tree_sibs(S,Xs1,K,Format).

$pp_print_tree_node(S,L,K,_), K == 0 =>
    write(S,L), nl(S).
$pp_print_tree_node(S,L,K,Format), K > 0 =>
    format(S,Format,["|"]),
    K1 is K - 1, !,
    $pp_print_tree_node(S,L,K1,Format).


%%----------------------------------------
%%  e-graph manipulator

strip_switches(G0,G1) :-
    $pp_require_list(G0,$msg(2104),strip_switches/2),
    $pp_strip_switches(G0,G1).

$pp_strip_switches([],[]).
$pp_strip_switches([node(L,Ps0)|Ns0],[node(L,Ps1)|Ns1]) :-
    $pp_strip_switches_sub(Ps0,Ps1),!,
    $pp_strip_switches(Ns0,Ns1).

$pp_strip_switches_sub([],[]).
$pp_strip_switches_sub([path(Gs,_)|Ps0],[Gs|Ps1]) :- !,
    $pp_strip_switches_sub(Ps0,Ps1).

%%----------------------------------------
%%  debugging aid

write_call(Goal) :-
    write_call([],Goal).

write_call(Opts,Goal) :-
    $pp_write_call_core(Opts,Goal,Goal).

??(Goal)  :- write_call([],Goal).
??*(Goal) :- write_call([all],Goal).
??>(Goal) :- write_call([call],Goal).
??<(Goal) :- write_call([exit+fail],Goal).
??+(Goal) :- write_call([exit],Goal).
??-(Goal) :- write_call([fail],Goal).

disable_write_call :-
    set_prism_flag(write_call_events,off).

$pp_write_call_core(Opts,Source,Goal) :-
    $pp_require_write_callable(Goal,$msg(3200),write_call/2),
    $pp_write_call_proc_opts(Opts,Call,Exit,Redo,Fail,Indent,Marker),
    $pp_write_call_print(Call,'Call',Indent,Marker,Source),
    ( Goal, ( $pp_write_call_print(Exit,'Exit',Indent,Marker,Source)
            ; $pp_write_call_print(Redo,'Redo',Indent,Marker,Source), fail
            )
    ; $pp_write_call_print(Fail,'Fail',Indent,Marker,Source), fail
    ).

$pp_write_call_build(Opts,Source,Goal,Body) :-
    Body = ( $pp_write_call_proc_opts(Opts,Call,Exit,Redo,Fail,Indent,Marker),
             $pp_write_call_print(Call,'Call',Indent,Marker,Source),
             ( Goal,( $pp_write_call_print(Exit,'Exit',Indent,Marker,Source)
                    ; $pp_write_call_print(Redo,'Redo',Indent,Marker,Source), fail
                    )
             ; $pp_write_call_print(Fail,'Fail',Indent,Marker,Source), fail
             )
           ),!.

$pp_write_call_proc_opts(Opts,Call,Exit,Redo,Fail,Indent,Marker) :-
    get_prism_flag(write_call_events,FlagValue),
    $pp_proc_opts(Opts,$pp_write_call_option,
                  [Events,Indent,Marker],[FlagValue,0,_],
                  write_call/2),
    ( FlagValue == off ->
      Call = 0, Exit = 0, Redo = 0, Fail = 0
    ; $pp_write_call_decomp(Events,Call,Exit,Redo,Fail)
    ), !.

$pp_write_call_option(X,1,Y) :-
    $pp_write_call_events(X,Y), !, Y \== none.
$pp_write_call_option(indent(X),2,X) :- !, integer(X).
$pp_write_call_option(marker(X),3,X) :- !.

$pp_write_call_events(all,all) :- !.
$pp_write_call_events(none,none) :- !.
$pp_write_call_events(X,Y) :-
    $pp_expr_to_list('+',X,Xs),
    $pp_write_call_events(Xs,Y,0,0,0,0),!.

$pp_write_call_events(Xs0,Y,C,E,R,F), Xs0 == [] =>
    $pp_write_call_decomp(Y,C,E,R,F), Y \== none.
$pp_write_call_events(Xs0,Y,C,E,R,F), Xs0 = [X|Xs1] =>
    ( X == call, C == 0 -> $pp_write_call_events(Xs1,Y,1,E,R,F)
    ; X == exit, E == 0 -> $pp_write_call_events(Xs1,Y,C,1,R,F)
    ; X == redo, R == 0 -> $pp_write_call_events(Xs1,Y,C,E,1,F)
    ; X == fail, F == 0 -> $pp_write_call_events(Xs1,Y,C,E,R,1)
    ).

$pp_write_call_decomp(none,0,0,0,0).
$pp_write_call_decomp(call,1,0,0,0).
$pp_write_call_decomp(exit,0,1,0,0).
$pp_write_call_decomp(call+exit,1,1,0,0).
$pp_write_call_decomp(redo,0,0,1,0).
$pp_write_call_decomp(call+redo,1,0,1,0).
$pp_write_call_decomp(exit+redo,0,1,1,0).
$pp_write_call_decomp(call+exit+redo,1,1,1,0).
$pp_write_call_decomp(fail,0,0,0,1).
$pp_write_call_decomp(call+fail,1,0,0,1).
$pp_write_call_decomp(exit+fail,0,1,0,1).
$pp_write_call_decomp(call+exit+fail,1,1,0,1).
$pp_write_call_decomp(redo+fail,0,0,1,1).
$pp_write_call_decomp(call+redo+fail,1,0,1,1).
$pp_write_call_decomp(exit+redo+fail,0,1,1,1).
$pp_write_call_decomp(all,1,1,1,1).

$pp_write_call_print(1,Head,Indent,Marker,Goal), var(Marker) =>
    tab(Indent), format("[~w] ~q~n",[Head,Goal]).
$pp_write_call_print(1,Head,Indent,Marker,Goal), nonvar(Marker) =>
    tab(Indent), format("[~w:~w] ~q~n",[Head,Marker,Goal]).
$pp_write_call_print(0,_,_,_,_).

%%----------------------------------------

$pp_learn_message(S,E,T,M) :-
    get_prism_flag(learn_message,LM),
    $pp_learn_message_decomp(LM,S,E,T,M),!.

%%----------------------------------------

$pp_learn_message_events(all,all) :- !.
$pp_learn_message_events(none,none) :- !.
$pp_learn_message_events(X,Y) :-
    $pp_expr_to_list('+',X,Xs),
    $pp_learn_message_events(Xs,Y,0,0,0,0).

$pp_learn_message_events(Xs0,Y,S,E,T,M) :- Xs0 == [] ->
    $pp_learn_message_decomp(Y,S,E,T,M), Y \== none.
$pp_learn_message_events(Xs0,Y,S,E,T,M) :- Xs0 = [X|Xs1] ->
    ( X == search, S = 0 -> $pp_learn_message_events(Xs1,Y,1,E,T,M)
    ; X == em,     E = 0 -> $pp_learn_message_events(Xs1,Y,S,1,T,M)
    ; X == stats,  T = 0 -> $pp_learn_message_events(Xs1,Y,S,E,1,M)
    ; X == misc,   M = 0 -> $pp_learn_message_events(Xs1,Y,S,E,T,1)
    ).

$pp_learn_message_decomp(none,             0,0,0,0).
$pp_learn_message_decomp(search,           1,0,0,0).
$pp_learn_message_decomp(em,               0,1,0,0).
$pp_learn_message_decomp(search+em,        1,1,0,0).
$pp_learn_message_decomp(stats,            0,0,1,0).
$pp_learn_message_decomp(search+stats,     1,0,1,0).
$pp_learn_message_decomp(em+stats,         0,1,1,0).
$pp_learn_message_decomp(search+em+stats,  1,1,1,0).
$pp_learn_message_decomp(misc,             0,0,0,1).
$pp_learn_message_decomp(search+misc,      1,0,0,1).
$pp_learn_message_decomp(em+misc,          0,1,0,1).
$pp_learn_message_decomp(search+em+misc,   1,1,0,1).
$pp_learn_message_decomp(stats+misc,       0,0,1,1).
$pp_learn_message_decomp(search+stats+misc,1,0,1,1).
$pp_learn_message_decomp(em+stats+misc,    0,1,1,1).
$pp_learn_message_decomp(all,              1,1,1,1).

%%----------------------------------------

$pp_mcmc_message(S,E,C,T,M) :-
    get_prism_flag(mcmc_message,MM),
    $pp_mcmc_message_decomp(MM,S,E,C,T,M),!.

%%----------------------------------------

$pp_mcmc_message_events(all,all) :- !.
$pp_mcmc_message_events(none,none) :- !.
$pp_mcmc_message_events(X,Y) :-
    $pp_expr_to_list('+',X,Xs),
    $pp_mcmc_message_events(Xs,Y,0,0,0,0,0).

$pp_mcmc_message_events(Xs0,Y,S,E,C,T,M) :- Xs0 == [] ->
    $pp_mcmc_message_decomp(Y,S,E,C,T,M), Y \== none.
$pp_mcmc_message_events(Xs0,Y,S,E,C,T,M) :- Xs0 = [X|Xs1] ->
    ( X == search, S = 0 -> $pp_mcmc_message_events(Xs1,Y,1,E,C,T,M)
    ; X == em,     E = 0 -> $pp_mcmc_message_events(Xs1,Y,S,1,C,T,M)
    ; X == mcmc,   C = 0 -> $pp_mcmc_message_events(Xs1,Y,S,E,1,T,M)
    ; X == stats,  T = 0 -> $pp_mcmc_message_events(Xs1,Y,S,E,C,1,M)
    ; X == misc,   M = 0 -> $pp_mcmc_message_events(Xs1,Y,S,E,C,T,1)
    ).

$pp_mcmc_message_decomp(none,                  0,0,0,0,0).
$pp_mcmc_message_decomp(search,                1,0,0,0,0).
$pp_mcmc_message_decomp(em,                    0,1,0,0,0).
$pp_mcmc_message_decomp(search+em,             1,1,0,0,0).
$pp_mcmc_message_decomp(mcmc,                  0,0,1,0,0).
$pp_mcmc_message_decomp(search+mcmc,           1,0,1,0,0).
$pp_mcmc_message_decomp(em+mcmc,               0,1,1,0,0).
$pp_mcmc_message_decomp(search+em+mcmc,        1,1,1,0,0).
$pp_mcmc_message_decomp(stats,                 0,0,0,1,0).
$pp_mcmc_message_decomp(search+stats,          1,0,0,1,0).
$pp_mcmc_message_decomp(em+stats,              0,1,0,1,0).
$pp_mcmc_message_decomp(search+em+stats,       1,1,0,1,0).
$pp_mcmc_message_decomp(mcmc+stats,            0,0,1,1,0).
$pp_mcmc_message_decomp(search+mcmc+stats,     1,0,1,1,0).
$pp_mcmc_message_decomp(em+mcmc+stats,         0,1,1,1,0).
$pp_mcmc_message_decomp(search+em+mcmc+stats,  1,1,1,1,0).
$pp_mcmc_message_decomp(misc,                  0,0,0,0,1).
$pp_mcmc_message_decomp(search+misc,           1,0,0,0,1).
$pp_mcmc_message_decomp(em+misc,               0,1,0,0,1).
$pp_mcmc_message_decomp(search+em+misc,        1,1,0,0,1).
$pp_mcmc_message_decomp(mcmc+misc,             0,0,1,0,1).
$pp_mcmc_message_decomp(search+mcmc+misc,      1,0,1,0,1).
$pp_mcmc_message_decomp(em+mcmc+misc,          0,1,1,0,1).
$pp_mcmc_message_decomp(search+em+mcmc+misc,   1,1,1,0,1).
$pp_mcmc_message_decomp(stats+misc,            0,0,0,1,1).
$pp_mcmc_message_decomp(search+stats+misc,     1,0,0,1,1).
$pp_mcmc_message_decomp(em+stats+misc,         0,1,0,1,1).
$pp_mcmc_message_decomp(search+em+stats+misc,  1,1,0,1,1).
$pp_mcmc_message_decomp(mcmc+stats+misc,       0,0,1,1,1).
$pp_mcmc_message_decomp(search+mcmc+stats+misc,1,0,1,1,1).
$pp_mcmc_message_decomp(em+mcmc+stats+misc,    0,1,1,1,1).
$pp_mcmc_message_decomp(all,                   1,1,1,1,1).

%%----------------------------------------
%%  for parallel mode

$pp_require_mp_mode :-
    ( $pc_mp_mode -> true
    ; $pp_raise_internal_error($msg(1005),invalid_module,$damon_load/0)
    ).

%%----------------------------------------
%%  expand the outcome space

% ?- expand_values([3,2-5@2,1-3,t],X).
% X = [3,2,4,1,2,3,t] 

expand_values(Ns,ExpandedNs) :-
    $pp_require_list_or_nil(Ns,$msg(2109),expland_values/2),
    $pp_require_ground(Ns,$msg(1105),expand_values/2),
    $pp_expand_values1(Ns,ExpandedNs).

% just fails for errorneous inputs
expand_values1(Ns,ExpandedNs) :-
    is_list(Ns),
    ground(Ns),
    $pp_expand_values1(Ns,ExpandedNs).

$pp_expand_values1([],[]).
$pp_expand_values1([N|Ns],ENs) :-
    ( N = Start-End@Step,
      integer(Start),integer(End),integer(Step),Step>0 ->
        $pp_require_integer_range_incl(Start,End,$msg(2008),expand_values/2),
        $pp_expand_values2(Start,End,Step,ENs0),
        append(ENs0,ENs1,ENs)
    ; N = Start-End,integer(Start),integer(End) ->
        $pp_require_integer_range_incl(Start,End,$msg(2008),expand_values/2),
        $pp_expand_values2(Start,End,1,ENs0),
        append(ENs0,ENs1,ENs)
    ; ENs = [N|ENs1]
    ),!,
    $pp_expand_values1(Ns,ENs1).

$pp_expand_values2(Start,End,_,[]) :- Start > End.
$pp_expand_values2(Start,End,Step,[Start|Ns]) :-
    Start1 is Start + Step,!,
    $pp_expand_values2(Start1,End,Step,Ns).


%%----------------------------------------
%%  log-gamma function

lngamma(X,G) :-
    $pp_require_positive_number(X,$msg(3400),lngamma/2),
    $pc_lngamma(X,G).

%%----------------------------------------
%%  log-gamma function

digamma(X,G) :-
    $pp_require_positive_number(X,$msg(3400),digamma/2),
    $pc_digamma(X,G).


%%----------------------------------------
%%  Garbage collection (a bit experimental)

$pp_garbage_collect :-
    ( get_prism_flag(force_gc,on) -> garbage_collect
    ; true
    ).


%%----------------------------------------
%%  Garbage collection (a bit experimental)

$pp_format_if(Flag,Format) :- $pp_format_if(Flag,Format,[]).

$pp_format_if(Flag,Format,Args) :-
    ( Flag == 0 -> true
    ; format(Format,Args)
    ),!.
%%--------------------------

$pp_require_crf_params(X,MsgID,Source) :-
    ( $pp_test_crf_params(X) -> true
    ; $pp_raise_on_require([X],MsgID,Source,$pp_error_distribution)
    ).

$pp_test_crf_params(X) :-
    ( $pp_test_fixed_size_crf_params(X)
    ; $pp_test_variable_size_distribution(X)
    ).

$pp_test_fixed_size_crf_params(X) :-
    ground(X),
    ( $pp_test_numbers(X)
    ; $pp_test_crf_params_plus(X)
    ; $pp_test_ratio(X)
    ).

$pp_test_crf_params_plus(X) :-
    $pp_expr_to_list('+',X,Ps),
    length(Ps,L),
    L > 1,!,
    $pp_test_probabilities(Ps).

%%--------------------------

expand_fprobs(Dist,N,Probs) :-
    $pp_expand_fprobs(Dist,N,Probs,expand_fprobs/3).

$pp_expand_fprobs(Dist,N,Probs,Source) :-
    $pp_require_crf_params(Dist,$msg(0200),Source),
    $pp_require_positive_integer(N,$msg(0204),Source),
    $pp_spec_to_ratio(Dist,N,Ratio,Source),
    $pp_check_expanded_prob_size(Ratio,N,Source),
    $pp_normalize_ratio_crf(Dist,Ratio,Probs).

$pp_normalize_ratio_crf(Dist,Ratio,Probs) :-
    ( Dist = [_|_] -> Probs = Ratio
    ; Dist = (_+_) -> Probs = Ratio
    ; $pp_normalize_ratio(Ratio,Probs)
    ).

%%-------------------------

set_sw_w(Sw) :- set_sw_a(Sw).
set_sw_w(Sw,Spec) :- set_sw_a(Sw,Spec).

set_sw_all_w :- set_sw_all_a.
set_sw_all_w(Sw) :- set_sw_all_a(Sw).

set_sw_w_all          :- set_sw_all_w.
set_sw_w_all(Sw)      :- set_sw_all_w(Sw).
set_sw_w_all(Sw,Spec) :- set_sw_all_w(Sw,Spec).

show_sw_w :- show_sw_a.
show_sw_w(Sw) :- show_sw_a(Sw).
crf_learn(Gs):- call($pp_crflearn_core(Gs)).

$pp_crflearn_check_goals(Goals) :-
    $pp_require_observed_data(Goals,$msg(1302),$pp_crflearn_core/1),
    $pp_crflearn_check_goals1(Goals).

$pp_crflearn_check_goals1([]).
$pp_crflearn_check_goals1([G0|Gs]) :-
    ( (G0 = goal(G,Count) ; G0 = count(G,Count) ; G0 = (Count times G) ) ->
        $pp_require_positive_integer(Count,$msg(1306),$pp_crflearn_core/1)
    ; G = G0
    ),
    $pp_require_tabled_probabilistic_atom(G,$msg(1303),$pp_crflearn_core/1),!,
    $pp_crflearn_check_goals1(Gs).

$pp_crflearn_core(Goals) :-
    $pp_crflearn_check_goals(Goals),
    $pp_learn_message(MsgS,MsgE,MsgT,MsgM),
    $pc_set_em_message(MsgE),
    cputime(Start),
    $pp_clean_crflearn_info,
    $pp_trans_crf_goals(Goals,Table,GoalCountPairs,AllGoals),
    $pp_trans_crf_countpairs(GoalCountPairs,GoalCountPairs1),
    global_set($pg_observed_facts,GoalCountPairs1),
    cputime(StartExpl),
    global_set($pg_num_goals,0),
    $pp_find_explanations(AllGoals),!,
    $pp_print_num_goals(MsgS),
    cputime(EndExpl),
    statistics(table,[TableSpace,_]),
    $pp_format_if(MsgM,"Exporting switch information to the CRF-learn routine ... "),
    flush_output,
    $pp_export_sw_info,
    $pp_format_if(MsgM,"done~n"),
    $pp_crf_observed_facts(GoalCountPairs,GidCountPairs,Table,
                       0,Len,0,NGoals,-1,FailRootIndex),
    $pp_check_failure_in_crflearn(Goals,FailRootIndex,crflearn/1),
    $pc_crf_prepare(GidCountPairs,Len,NGoals,FailRootIndex),
    cputime(StartGrd),
    $pp_grd(Output),
    cputime(EndGrd),
    $pc_import_occ_crf_switches(NewSws,NSwitches,NSwVals),
    $pp_decode_update_switches(ml,NewSws),
    $pc_import_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    cputime(End),
    $pp_assert_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    $pp_assert_crf_learn_stats(Output,NSwitches,NSwVals,TableSpace,
                           Start,End,StartExpl,EndExpl,StartGrd,EndGrd,1000),
    $pp_print_learn_stats_message(MsgT),
    $pp_print_crf_learn_end_message(MsgM),!.

$pp_print_crf_learn_end_message(Msg) :-
    ( Msg == 0 -> true
    ; format("Type show_sw to show the lambdas.~n",[])
    ).

$pp_grd(Output):-
    $pc_prism_grd(Iterate,Likelihood),
    Output = [Iterate,Likelihood].

$pp_clean_crflearn_info :-
    $pp_clean_dummy_goal_table,
    $pp_clean_graph_stats,
    $pp_clean_learn_stats,
    $pp_init_tables_aux,
    $pp_init_tables_if_necessary,!.

$pp_assert_crf_learn_stats(Output,NSwitches,NSwVals,TableSpace,
                       Start,End,StartExpl,EndExpl,StartEM,EndEM,UnitsPerSec) :-
    assertz($ps_num_switches(NSwitches)),
    assertz($ps_num_switch_values(NSwVals)),
    ( integer(TableSpace) -> assertz($ps_learn_table_space(TableSpace)) ; true ),
    Time is (End - Start) / UnitsPerSec,
    assertz($ps_learn_time(Time)),
    TimeExpl is (EndExpl - StartExpl) / UnitsPerSec,
    assertz($ps_learn_search_time(TimeExpl)),
    TimeEM is (EndEM - StartEM) / UnitsPerSec,
    assertz($ps_em_time(TimeEM)),
    Output = [Iterate,Likelihood],
    assertz($ps_num_iterations(Iterate)),
    assertz($ps_log_likelihood(Likelihood)),!.

$pp_trans_crf_countpairs([],[]).
$pp_trans_crf_countpairs([goal(Goal,Count,_PGidx)|GoalCountPairs],GoalCountPairs1):-
    GoalCountPairs1 = [goal(Goal,Count)|GoalCountPairs0],!,
    $pp_trans_crf_countpairs(GoalCountPairs,GoalCountPairs0).

$pp_trans_crf_goals(Goals,ParentTable,GoalCountPairs,AllGoals) :-
    $pp_build_crf_count_pairs(Goals,Pairs,ParentPairs),
    new_hashtable(PCountTable),
    $pp_trans_crf_count_pairs(Pairs,GoalCountPairs0,PCountTable,AllGoals0),
    new_hashtable(ParentTable),
    $pp_trans_crf_parent_count_pairs(ParentPairs,PCountTable,ParentTable,PGoalCountPairs,AllGoals1),
    append(GoalCountPairs0,PGoalCountPairs,GoalCountPairs),
    append(AllGoals0,AllGoals1,AllGoals).

$pp_build_crf_count_pairs(Goals,Pairs,ParentPairs) :-
    new_hashtable(Table),
    new_hashtable(Table2),
    $pp_count_crf_goals(Goals,Table,Table2,0),
    hashtable_to_list(Table,Pairs0),
    hashtable_to_list(Table2,ParentPairs0),
    sort(Pairs0,Pairs),
    sort(ParentPairs0,ParentPairs).

$pp_count_crf_goals([],_,_,_).
$pp_count_crf_goals([G0|Goals],Table,Table2,N) :-
    ( G0 = goal(Goal,Count)  -> true
    ; G0 = count(Goal,Count) -> true
    ; G0 = (Count times Goal) -> true
    ; Goal = G0, Count = 1
    ),
    $pp_require_ground(Goal,$msg(1601),$pp_crflearn_core),
    ( $pp_hashtable_get(Table,Goal,(Count0,Pid)) ->
        Count1 is Count0 + Count,
        $pp_hashtable_put(Table,Goal,(Count1,Pid)),
        N1 = N
    ; $pp_build_parent_goal(Goal,ParentGoal0),
        copy_term(ParentGoal0,ParentGoal),
        ( $pp_hashtable_get(Table2,ParentGoal,Pid) ->
            $pp_hashtable_put(Table,Goal,(Count,Pid)),N1 = N
        ; N1 is N + 1,
            $pp_hashtable_put(Table2,ParentGoal,N),
            $pp_hashtable_put(Table,Goal,(Count,N))
        )
    ),!,
    $pp_count_crf_goals(Goals,Table,Table2,N1).

$pp_build_parent_goal(Goal,ParentGoal) :-
    ( Goal =.. [F,X,_] -> ParentGoal =.. [F,X]
    ; $pp_raise_runtime_error($msg(1602),Goal,$pp_crf_learn_core)
    ),
    $pp_require_tabled_probabilistic_atom(ParentGoal,$msg(1604),$pp_crf_learn_core).

$pp_trans_crf_count_pairs([],[],_,[]).
$pp_trans_crf_count_pairs([Goal=(Count,PGidx)|Pairs],GoalCountPairs,PCountTable,AllGoals) :-
    $pp_build_dummy_goal(Goal,DummyGoal),
    GoalCountPairs = [goal(DummyGoal,Count,PGidx)|GoalCountPairs1],
    AllGoals = [DummyGoal|AllGoals1],
    ( $pp_hashtable_get(PCountTable,PGidx,Count0) ->
        Count1 is Count0 + Count,
        $pp_hashtable_put(PCountTable,PGidx,Count1)
    ; $pp_hashtable_put(PCountTable,PGidx,Count)
    ),!,
    $pp_trans_crf_count_pairs(Pairs,GoalCountPairs1,PCountTable,AllGoals1).

$pp_trans_crf_parent_count_pairs([],_,_,[],[]).
$pp_trans_crf_parent_count_pairs([PGoal=PGidx|ParentPairs],PCountTable,Table,PGoalCountPairs,AllGoals) :-
    $pp_build_dummy_goal(PGoal,DummyGoal),
    $pp_hashtable_put(Table,PGidx,DummyGoal),
    $pp_hashtable_get(PCountTable,PGidx,Count),
    PGoalCountPairs = [goal(DummyGoal,Count,-1)|PGoalCountPairs1],
    AllGoals = [DummyGoal|AllGoals1],!,
    $pp_trans_crf_parent_count_pairs(ParentPairs,PCountTable,Table,PGoalCountPairs1,AllGoals1).

$pp_crf_observed_facts([],[],_Table,Len,Len,NGoals,NGoals,FailRootIndex,FailRootIndex).
$pp_crf_observed_facts([goal(Goal,Count,PGidx)|GoalCountPairs],GidCountPairs,Table,
                   Len0,Len,NGoals0,NGoals,FailRootIndex0,FailRootIndex) :-
    % fails if the goal is ground but has no proof
    ( $pc_prism_goal_id_get(Goal,Gid) ->
        ( Goal == failure ->
            NGoals1 = NGoals0,
            FailRootIndex1 = Len0
        ; NGoals1 is NGoals0 + Count,
          FailRootIndex1 = FailRootIndex0
        ),
        ( $pp_hashtable_get(Table,PGidx,PGoal) ->
            $pc_prism_goal_id_get(PGoal,PGid)
        ; PGid = PGidx
        ),
        GidCountPairs = [goal(Gid,Count,PGid)|GidCountPairs1],
        Len1 is Len0 + 1
    ; $pp_raise_unexpected_failure($pp_crf_observed_facts/8)
    ),!,
    $pp_crf_observed_facts(GoalCountPairs,GidCountPairs1,Table,
                       Len1,Len,NGoals1,NGoals,FailRootIndex1,FailRootIndex).

$pp_check_failure_in_crflearn(Gs,FailRootIndex,Source) :-
    ( FailRootIndex >= 0 ->
        $pp_raise_runtime_error($msg(1603),[Gs],failure_in_crf_learn,Source)
    ; true
    ).
%%%% CRF-Viterbi wrappers

crf_viterbi(G) :-
    $pp_crfviterbi_wrapper(crf_viterbi(G)).
crf_viterbi(G,P) :-
    $pp_crfviterbi_wrapper(crf_viterbi(G,P)).
crf_viterbif(G) :-
    $pp_crfviterbi_wrapper(crf_viterbif(G)).
crf_viterbif(G,P,V) :-
    $pp_crfviterbi_wrapper(crf_viterbif(G,P,V)).
crf_viterbit(G) :-
    $pp_crfviterbi_wrapper(crf_viterbit(G)).
crf_viterbit(G,P,T) :-
    $pp_crfviterbi_wrapper(crf_viterbit(G,P,T)).
n_crf_viterbi(N,G) :-
    $pp_crfviterbi_wrapper(n_crf_viterbi(N,G)).
n_crf_viterbi(N,G,P) :-
    $pp_crfviterbi_wrapper(n_crf_viterbi(N,G,P)).
n_crf_viterbif(N,G) :-
    $pp_crfviterbi_wrapper(n_crf_viterbif(N,G)).
n_crf_viterbif(N,G,V) :-
    $pp_crfviterbi_wrapper(n_crf_viterbif(N,G,V)).
n_crf_viterbit(N,G) :-
    $pp_crfviterbi_wrapper(n_crf_viterbit(N,G)).
n_crf_viterbit(N,G,T) :-
    $pp_crfviterbi_wrapper(n_crf_viterbit(N,G,T)).
crf_viterbig(G) :-
    $pp_crfviterbi_wrapper(crf_viterbig(G)).
crf_viterbig(G,P) :-
    $pp_crfviterbi_wrapper(crf_viterbig(G,P)).
crf_viterbig(G,P,V) :-
    $pp_crfviterbi_wrapper(crf_viterbig(G,P,V)).
n_crf_viterbig(N,G) :-
    $pp_crfviterbi_wrapper(n_crf_viterbig(N,G)).
n_crf_viterbig(N,G,P) :-
    $pp_crfviterbi_wrapper(n_crf_viterbig(N,G,P)).
n_crf_viterbig(N,G,P,V) :-
    $pp_crfviterbi_wrapper(n_crf_viterbig(N,G,P,V)).

$pp_crfviterbi_wrapper(Pred0) :-
    Suffix = '_p',
    Pred0 =.. [Name0|Args],
    atom_concat(Name0,Suffix,Name1),
    Pred1 =.. [Name1|Args],!,
    call(Pred1).  % do not add cut here (n_viterbig is non-deterministic)

%%%% Viterbi routine with C interface
%%
%% viterbi_p(G) :- print the Viterbi prob
%% viterbi_p(G,P) :- output the Viterbi prob
%% viterbif_p(G) :- print the Viterbi path and the Viterbi prob
%% viterbif_p(G,P,VPath) :- output the Viterbi path and the Viterbi prob
%%
%% VPath is a list of node(G,Paths), where Paths is a list of
%% path(Gs,Sws), where Gs are subgoals of G and Sws are switches.
%%
%% Usually in VPath, node(msw(Sw,V),[]) is omitted, but optionally
%% it can be included in VPath.

% Main routine:

% viterbi family:

crf_viterbi_p(Goal) :-
    crf_viterbif_p(Goal,Pmax,_),
    $pp_print_crfviterbi_prob(Pmax).

crf_viterbi_p(Goal,Pmax) :-
    crf_viterbif_p(Goal,Pmax,_).

% viterbif family:

crf_viterbif_p(Goal) :-
    crf_viterbif_p(Goal,Pmax,VNodeL),
    format("~n",[]),
    print_graph(VNodeL,[lr('<=')]),
    $pp_print_crfviterbi_prob(Pmax).

crf_viterbif_p(Goal,Pmax,VNodeL) :-
    $pp_require_tabled_probabilistic_atom(Goal,$msg(0006),viterbif_p/3),
    ( Goal = msw(I,_) ->
        $pp_require_ground(I,$msg(0101),viterbif_p/3),
        $pp_require_switch_outcomes(I,$msg(0102),viterbif_p/3)
    ; true
    ),
    $pp_crfviterbif_p(Goal,Pmax,VNodeL).

$pp_crfviterbif_p(Goal,Pmax,VNodeL) :-
    $pp_clean_infer_stats,
    cputime(T0),
    $pp_crfviterbi_core(Goal,Pmax,VNodeL),
    cputime(T1),
    $pp_garbage_collect,
    InfTime is T1 - T0,
    $pp_assert_viterbi_stats1(InfTime),!.

% viterbit family:

crf_viterbit_p(Goal) :-
    crf_viterbit_p(Goal,Pmax,VTreeL),
    format("~n",[]),
    print_tree(VTreeL),
    $pp_print_crfviterbi_prob(Pmax).

crf_viterbit_p(Goal,Pmax,VTreeL) :-
    $pp_require_tabled_probabilistic_atom(Goal,$msg(0006),viterbit_p/3),
    $pp_crfviterbif_p(Goal,Pmax,VNodeL),
    viterbi_tree(VNodeL,VTreeL).

% viterbig family:

crf_viterbig_p(Goal) :-
    ( ground(Goal) -> crf_viterbi_p(Goal)
    ; crf_viterbig_p(Goal,_,_)
    ).

crf_viterbig_p(Goal,Pmax) :-
    ( ground(Goal) -> crf_viterbi_p(Goal,Pmax)
    ; crf_viterbig_p(Goal,Pmax,_)
    ).

crf_viterbig_p(Goal,Pmax,VNodeL) :-
    $pp_require_tabled_probabilistic_atom(Goal,$msg(0006),viterbig_p/3),
    ( Goal = msw(I,_) ->
        $pp_require_ground(I,$msg(0101),viterbif_p/3),
        $pp_require_switch_outcomes(I,$msg(0102),viterbig_p/3)
    ; true
    ),
    $pp_crfviterbig_p(Goal,Pmax,VNodeL).

$pp_crfviterbig_p(Goal,Pmax,VNodeL) :-
    $pp_clean_infer_stats,
    cputime(T0),
    $pp_crfviterbi_core(Goal,Pmax,VNodeL),
    ( ground(Goal) -> true
    ; VNodeL = [node(_,[path([Goal1],[])])|_] -> Goal = Goal1
    ; VNodeL = [node(_,[path([],[SwIns])])|_] -> Goal = SwIns
    ),
    cputime(T1),
    InfTime is T1 - T0,
    $pp_garbage_collect,
    $pp_assert_viterbi_stats1(InfTime),!.

%% Common routine:

$pp_print_crfviterbi_prob(Pmax) :-
    format("~nCRF-Viterbi weight = ~15f~n",[Pmax]).

$pp_crfviterbi_core(Goal,Pmax,VNodeL) :-
    Goal = msw(I,V),!,
    $pp_require_ground(I,$msg(0101),$pp_viterbi_core/3),
    $pp_require_switch_outcomes(I,$msg(0102),$pp_viterbi_core/3),
    $pp_init_tables_aux,
    $pp_clean_graph_stats,
    $pp_init_tables_if_necessary,!,
    ( ground(V) -> V = VCp ; copy_term(V,VCp) ),
    $pp_create_dummy_goal(DummyGoal),
    DummyBody = ($prism_expl_msw(I,VCp,Sid),
                 $pc_prism_goal_id_register(DummyGoal,Hid),
                 $prism_eg_path(Hid,[],[Sid])),
    Prog = [pred(DummyGoal,0,_,_,tabled(_,_,_,_),[(DummyGoal:-DummyBody)])],
    $pp_consult_preds_cond([],Prog),!,
    cputime(T1),
    $pp_find_explanations(DummyGoal),
    cputime(T2),
    $pp_compute_crfviterbi_p(DummyGoal,Pmax,[node(DummyGoal,Paths)|VNodeL0]),!,
    cputime(T3),
    VNodeL = [node(msw(I,V),Paths)|VNodeL0],
    $pc_import_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    $pp_assert_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    SearchTime  is T2 - T1,
    NumCompTime is T3 - T2,
    $pp_assert_viterbi_stats2(SearchTime,NumCompTime),!.

$pp_crfviterbi_core(Goal,Pmax,VNodeL) :-
    ground(Goal),!,
    $pp_init_tables_aux,
    $pp_clean_graph_stats,
    $pp_init_tables_if_necessary,!,
    cputime(T1),
    $pp_find_explanations(Goal),
    cputime(T2),
    $pp_compute_crfviterbi_p(Goal,Pmax,VNodeL),!,
    cputime(T3),
    $pc_import_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    $pp_assert_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    SearchTime  is T2 - T1,
    NumCompTime is T3 - T2,
    $pp_assert_viterbi_stats2(SearchTime,NumCompTime),!.

$pp_crfviterbi_core(Goal,Pmax,VNodeL) :-
    $pp_init_tables_aux,
    $pp_clean_graph_stats,
    $pp_init_tables_if_necessary,!,
    copy_term(Goal,GoalCp),
    ( $pp_trans_one_goal(GoalCp,CompGoal) -> BodyGoal = CompGoal
    ; BodyGoal = (savecp(CP),Depth=0,
                  $pp_expl_interp_goal(GoalCp,Depth,CP,[],_,[],_,[],_,[],_))
    ),
    $pp_create_dummy_goal(DummyGoal),
    DummyBody = (BodyGoal,
                 $pc_prism_goal_id_register(GoalCp,GId),
                 $pc_prism_goal_id_register(DummyGoal,HId),
                 $prism_eg_path(HId,[GId],[])),
    Prog = [pred(DummyGoal,0,_Mode,_Delay,tabled(_,_,_,_),
                 [(DummyGoal:-DummyBody)])],
    $pp_consult_preds_cond([],Prog),!,
    cputime(T1),
    $pp_find_explanations(DummyGoal),
    cputime(T2),
    $pp_compute_crfviterbi_p(DummyGoal,Pmax,[node(DummyGoal,Paths)|VNodeL0]),!,
    cputime(T3),
    VNodeL = [node(Goal,Paths)|VNodeL0],
    $pc_import_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    $pp_assert_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    SearchTime  is T2 - T1,
    NumCompTime is T3 - T2,
    $pp_assert_viterbi_stats2(SearchTime,NumCompTime),!.

% Sws = [sw(Id,Instances,Probs,PseudoCs,Fixed,FixedH),...]
$pp_compute_crfviterbi_p(Goal,Pmax,VNodeL) :-
    $pp_export_sw_info,
    $pc_prism_goal_id_get(Goal,Gid),
    $pc_compute_crfviterbi(Gid,EGs,EGPaths,ESwPaths,Pmax),
    $pp_decode_viterbi_path(EGs,EGPaths,ESwPaths,VNodeL),!.

%%%%
%%%%  Top-N Viterbi
%%%%
%%%% n_viterbi_p(N,G) :- print the top-N Viterbi probs
%%%% n_viterbi_p(N,G,Ps) :- output the top-N Viterbi probs
%%%% n_viterbif_p(N,G) :- print the top-N Viterbi paths and the corresponding
%%%%                     Viterbi probs
%%%% n_viterbif_p(N,G,VPathL) :- output the list of top-N Viterbi paths and
%%%%                            the corresponding Viterbi probs
%%%%

% n_viterbi family

n_crf_viterbi_p(N,Goal) :-
    n_crf_viterbif_p(N,Goal,VPathL),
    ( member(v_expl(J,Pmax,_),VPathL),
      $pp_print_n_crfviterbi(J,Pmax),
      fail
    ; true
    ).

n_crf_viterbi_p(N,Goal,Ps) :-
    n_crf_viterbif_p(N,Goal,VPathL),!,
    findall(Pmax,member(v_expl(_,Pmax,_),VPathL),Ps).

% n_viterbif family

n_crf_viterbif_p(N,Goal) :-
    n_crf_viterbif_p(N,Goal,VPathL),!,
    $pp_print_n_crfviterbif(VPathL).

n_crf_viterbif_p(N,Goal,VPathL) :-
    $pp_require_positive_integer(N,$msg(1400),n_viterbif_p/3),
    $pp_require_tabled_probabilistic_atom(Goal,$msg(0006),n_viterbif_p/3),
    $pp_n_crfviterbif_p(N,Goal,VPathL).

$pp_n_crfviterbif_p(N,Goal,VPathL) :-
    $pp_clean_infer_stats,
    cputime(T0),
    $pp_n_crfviterbi_p_core(N,Goal,VPathL),
    cputime(T1),
    InfTime is T1 - T0,
    $pp_garbage_collect,
    $pp_assert_viterbi_stats1(InfTime),!.

% n_viterbit family

n_crf_viterbit_p(N,Goal) :-
    n_crf_viterbif_p(N,Goal,VPathL),!,
    $pp_print_n_crfviterbit(VPathL).

n_crf_viterbit_p(N,Goal,VPathL) :-
    n_crf_viterbif_p(N,Goal,VPathL0),!,
    $pp_build_n_viterbit(VPathL0,VPathL).

%%%% 
%%%% $pp_n_viterbig_p(N,Goal) :- the same as $pp_n_viterbig_p(N,Goal,_,_)
%%%% $pp_n_viterbig_p(N,Goal,Pmax) :- the same as $pp_n_viterbig_p(N,Goal,Pmax,_)
%%%% $pp_n_viterbig_p(N,Goal,Pmax,VNodeL) :-
%%%%      if Goal is not ground, unify Goal with the first element in the K-th
%%%%      Viterbi path VNodeL (K=0,1,2,...,(N-1) on backtracking). Pmax is the
%%%%      probability of VNodeL.
%%%%

n_crf_viterbig_p(N,Goal) :-
    ( ground(Goal) -> n_crf_viterbi_p(N,Goal)
    ; n_crf_viterbig_p(N,Goal,_,_)
    ).

n_crf_viterbig_p(N,Goal,Pmax) :-
    ( ground(Goal) -> n_crf_viterbi_p(N,Goal,Ps),!,member(Pmax,Ps)
    ; n_crf_viterbig_p(N,Goal,Pmax,_)
    ).

n_crf_viterbig_p(N,Goal,Pmax,VNodeL) :-
    $pp_require_positive_integer(N,$msg(1400),n_viterbi_p/3),
    $pp_require_tabled_probabilistic_atom(Goal,$msg(0006),n_viterbi_p/3),
    $pp_n_crfviterbig_p(N,Goal,Pmax,VNodeL).

$pp_n_crfviterbig_p(N,Goal,Pmax,VNodeL) :-
    $pp_clean_infer_stats,
    cputime(T0),
    $pp_n_crfviterbi_p_core(N,Goal,VPathL),!,
    cputime(T1),
    InfTime is T1 - T0,
    $pp_garbage_collect,
    $pp_assert_viterbi_stats1(InfTime),!,
    ( ground(Goal) -> member(v_expl(J,Pmax,VNodeL),VPathL)
    ; Goal = msw(_,_) ->
        member(v_expl(J,Pmax,VNodeL),VPathL),
        VNodeL = [node(_,[path([],[SwIns])])|_],
        Goal = SwIns
    ; % else
        member(v_expl(J,Pmax,VNodeL),VPathL),
        VNodeL = [node(_,[path([Goal1],[])])|_],
        Goal = Goal1
    ).

%% Common routines:

$pp_print_n_crfviterbi(J,Pmax) :-
      format("#~w: CRF-Viterbi weight = ~15f~n",[J,Pmax]).

$pp_print_n_crfviterbif([]).
$pp_print_n_crfviterbif([v_expl(J,Pmax,VNodeL)|VPathL]) :-
    format("~n#~w~n",[J]),
    print_graph(VNodeL,[lr('<=')]),
    format("~nCRF-Viterbi weight = ~15f~n",[Pmax]),!,
    $pp_print_n_crfviterbif(VPathL).

$pp_print_n_crfviterbit([]).
$pp_print_n_crfviterbit([v_expl(J,Pmax,VNodeL)|VPathL]) :-
    format("~n#~w~n",[J]),
    viterbi_tree(VNodeL,VTreeL),
    print_tree(VTreeL),
    $pp_print_crfviterbi_prob(Pmax),!,
    $pp_print_n_crfviterbit(VPathL).

$pp_n_crfviterbi_p_core(N,Goal,VPathL) :-
    Goal = msw(I,V),!,
    $pp_require_ground(I,$msg(0101),$pp_n_viterbi_p_core/3),
    $pp_require_switch_outcomes(I,$msg(0102),$pp_n_viterbi_p_core/3),
    $pp_init_tables_aux,
    $pp_clean_graph_stats,
    $pp_init_tables_if_necessary,!,
    ( ground(V) -> V = VCp ; copy_term(V,VCp) ),
    $pp_create_dummy_goal(DummyGoal),
    DummyBody = ($prism_expl_msw(I,VCp,Sid),
                 $pc_prism_goal_id_register(DummyGoal,Hid),
                 $prism_eg_path(Hid,[],[Sid])),
    Prog = [pred(DummyGoal,0,_Mode,_Delay,tabled(_,_,_,_),
                 [(DummyGoal:-DummyBody)])],
    $pp_consult_preds_cond([],Prog),!,
    cputime(T1),
    $pp_find_explanations(DummyGoal),
    cputime(T2),
    $pp_compute_n_crfviterbi_p(N,DummyGoal,VPathL0),!,
    cputime(T3),
    $pp_replace_dummy_goal(Goal,DummyGoal,VPathL0,VPathL),
    $pc_import_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    $pp_assert_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    SearchTime  is T2 - T1,
    NumCompTime is T3 - T2,
    $pp_assert_viterbi_stats2(SearchTime,NumCompTime),!.

$pp_n_crfviterbi_p_core(N,Goal,VPathL) :-
    ground(Goal),!,
    $pp_init_tables_aux,
    $pp_clean_graph_stats,
    $pp_init_tables_if_necessary,!,
    cputime(T1),
    $pp_find_explanations(Goal),
    cputime(T2),
    $pp_compute_n_crfviterbi_p(N,Goal,VPathL),!,
    cputime(T3),
    $pc_import_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    $pp_assert_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    SearchTime  is T2 - T1,
    NumCompTime is T3 - T2,
    $pp_assert_viterbi_stats2(SearchTime,NumCompTime),!.

$pp_n_crfviterbi_p_core(N,Goal,VPathL) :-
    $pp_init_tables_aux,
    $pp_clean_graph_stats,
    $pp_init_tables_if_necessary,!,
    copy_term(Goal,GoalCp),
    ( $pp_trans_one_goal(GoalCp,CompGoal) -> BodyGoal = CompGoal
    ; BodyGoal = (savecp(CP),Depth=0,
                  $pp_expl_interp_goal(GoalCp,Depth,CP,[],_,[],_,[],_,[],_))
    ),
    $pp_create_dummy_goal(DummyGoal),
    DummyBody = (BodyGoal,
                 $pc_prism_goal_id_register(GoalCp,GId),
                 $pc_prism_goal_id_register(DummyGoal,HId),
                 $prism_eg_path(HId,[GId],[])),
    Prog = [pred(DummyGoal,0,_Mode,_Delay,tabled(_,_,_,_),
                 [(DummyGoal:-DummyBody)])],
    $pp_consult_preds_cond([],Prog),!,
    cputime(T1),
    $pp_find_explanations(DummyGoal),
    cputime(T2),
    $pp_compute_n_crfviterbi_p(N,DummyGoal,VPathL0),!,
    cputime(T3),
    $pp_replace_dummy_goal(Goal,DummyGoal,VPathL0,VPathL),
    $pc_import_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    $pp_assert_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    SearchTime  is T2 - T1,
    NumCompTime is T3 - T2,
    $pp_assert_viterbi_stats2(SearchTime,NumCompTime),!.

$pp_compute_n_crfviterbi_p(N,Goal,VPathL) :-
    $pp_export_sw_info,!,
    $pc_prism_goal_id_get(Goal,Gid),
    $pc_compute_n_crfviterbi(N,Gid,VPathL0),
    $pp_build_n_viterbi_path(VPathL0,VPathL),!.
%%%%
%%%% Hindsight routine with C interface
%%%%

%%
%% hindsight(G,SubG,HProbs) :-
%%   output hindsight probs of subgoals that matches with SubG given G
%%
%% hindsight(G,SubG) :- print hindsight probs of SubG given G
%%

crf_hindsight(G) :- crf_hindsight(G,_).

crf_hindsight(G,SubG) :-
    crf_hindsight(G,SubG,HProbs),
    ( HProbs == [] -> $pp_raise_warning($msg(1404))
    ; format("hindsight weights:~n",[]),
      $pp_print_hindsight_probs(HProbs)
    ).

crf_hindsight(G,SubG,HProbs) :-
    $pp_require_tabled_probabilistic_atom(G,$msg(0006),hindsight/3),
    ( nonvar(SubG) -> $pp_require_callable(SubG,$msg(1403),hindsight/3)
    ; true
    ),
    $pp_clean_infer_stats,
    cputime(T0),
    $pp_crfhindsight_core(G,SubG,HProbs0),
    $pp_sort_hindsight_probs(HProbs0,HProbs),
    cputime(T1),
    InfTime is T1 - T0,
    $pp_garbage_collect,
    $pp_assert_hindsight_stats1(InfTime),!.

$pp_crfhindsight_core(G,SubG,HProbs) :-
    ground(G),!,
    $pp_init_tables_aux,
    $pp_clean_graph_stats,
    $pp_init_tables_if_necessary,!,
    cputime(T0),
    $pp_find_explanations(G),!,
    cputime(T1),
    $pp_compute_crfhindsight(G,SubG,HProbs),
    cputime(T2),
    $pc_import_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    $pp_assert_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    SearchTime  is T1 - T0,
    NumCompTime is T2 - T1,
    $pp_assert_hindsight_stats2(SearchTime,NumCompTime),!.

$pp_crfhindsight_core(G,SubG,HProbs) :-
    $pp_init_tables_aux,
    $pp_clean_graph_stats,
    $pp_init_tables_if_necessary,!,
    copy_term(G,GoalCp),
    ( $pp_trans_one_goal(GoalCp,CompGoal) -> BodyGoal = CompGoal
    ; BodyGoal = (savecp(CP),Depth=0,
                  $pp_expl_interp_goal(GoalCp,Depth,CP,[],_,[],_,[],_,[],_))
    ),
    $pp_create_dummy_goal(DummyGoal),
    Clause = (DummyGoal:-BodyGoal,
                         $pc_prism_goal_id_register(GoalCp,GId),
                         $pc_prism_goal_id_register(DummyGoal,HId),
                         $prism_eg_path(HId,[GId],[])),
    Prog = [pred(DummyGoal,0,_Mode,_Delay,tabled(_,_,_,_),[Clause]),
            pred('$damon_load',0,_,_,_,[('$damon_load':-true)])],
    $pp_consult_preds_cond([],Prog),!,
    cputime(T0),
    $pp_find_explanations(DummyGoal),!,
    cputime(T1),
    $pp_compute_crfhindsight(DummyGoal,SubG,HProbs),
    cputime(T2),
    $pc_import_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    $pp_assert_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    SearchTime  is T1 - T0,
    NumCompTime is T2 - T1,
    $pp_assert_hindsight_stats2(SearchTime,NumCompTime),!.

% Sws = [sw(Id,Instances,Probs,PseudoCs,Fixed,FixedH),...]
$pp_compute_crfhindsight(Goal,SubG,HProbs) :-
    $pp_export_sw_info,
    $pc_prism_goal_id_get(Goal,Gid),
    $pc_compute_crfhindsight(Gid,SubG,0,HProbs0), % "0" indicates "unconditional"
    $pp_decode_hindsight(HProbs0,HProbs),!.
%
crf_prob(Goal) :-
    fprob(Goal,P),
    ( $pp_in_log_scale -> Text = 'Log-weight' ; Text = 'Weight' ),
    format("~w of ~w is: ~15f~n",[Text,Goal,P]).

crf_prob(Goal,Prob) :-
    $pp_require_tabled_probabilistic_atom(Goal,$msg(0006),prob/2),
    $pp_fprob(Goal,Prob).

$pp_fprob(msw(Sw,V),Prob) :-
    $pp_require_ground(Sw,$msg(0101),prob/2),
    $pp_require_switch_outcomes(Sw,$msg(0102),prob/2),
    $pp_clean_infer_stats,
    ( var(V) ->
        cputime(T0),
        ( $pp_in_log_scale -> Prob = 0.0 ; Prob = 1.0 ),
        cputime(T1),
        InfTime is T1 - T0,
        $pp_assert_prob_stats1(InfTime)
    ; % else
        cputime(T0),
        $pp_get_value_prob(Sw,V,Prob0),
        ( $pp_in_log_scale -> Prob is log(Prob0) ; Prob = Prob0 ),
        cputime(T1),
        InfTime is T1 - T0,
        $pp_assert_prob_stats1(InfTime)
    ),
    $pp_assert_prob_stats2(0.0,0.0),!.

$pp_fprob(Goal,Prob) :-
    $pp_clean_infer_stats,
    cputime(T0),
    $pp_fprob_core(Goal,Prob),
    cputime(T1),
    InfTime is T1 - T0,
    $pp_assert_prob_stats1(InfTime),!.

log_crf_prob(Goal) :-
    log_fprob(Goal,P),format("Log-weight of ~w is: ~15f~n",[Goal,P]).
log_crf_prob(Goal,P) :-
    $pp_fprob(Goal,P0),( $pp_in_log_scale -> P = P0 ; P is log(P0) ).

$pp_fprob_core(Goal,Prob) :-
    ground(Goal),
    $pp_is_tabled_probabilistic_atom(Goal),!,
    $pp_init_tables_aux,
    $pp_clean_graph_stats,
    $pp_init_tables_if_necessary,!,
    cputime(T1),
    $pp_find_explanations(Goal),
    cputime(T2),
    $pp_compute_inside_feature(Goal,Prob),!,
    cputime(T3),
    $pc_import_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    $pp_assert_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    SearchTime  is T2 - T1,
    NumCompTime is T3 - T2,
    $pp_assert_prob_stats2(SearchTime,NumCompTime),!.

$pp_fprob_core(Goal,Prob) :-
    $pp_init_tables_aux,
    $pp_clean_graph_stats,
    $pp_init_tables_if_necessary,!,
    copy_term(Goal,GoalCp),
    ( $pp_trans_one_goal(GoalCp,CompGoal) -> BodyGoal = CompGoal
    ; BodyGoal = (savecp(CP),Depth=0,
                  $pp_expl_interp_goal(GoalCp,Depth,CP,[],_,[],_,[],_,[],_))
    ),
    $pp_create_dummy_goal(DummyGoal),
    Clause = (DummyGoal:-BodyGoal,
                         $pc_prism_goal_id_register(GoalCp,GId),
                         $pc_prism_goal_id_register(DummyGoal,HId),
                         $prism_eg_path(HId,[GId],[])),
    Prog = [pred(DummyGoal,0,_Mode,_Delay,tabled(_,_,_,_),[Clause])],
    $pp_consult_preds_cond([],Prog),!,
    cputime(T1),
    $pp_find_explanations(DummyGoal),
    cputime(T2),
    $pp_compute_inside_feature(DummyGoal,Prob),
    cputime(T3),
    $pc_import_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    $pp_assert_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    SearchTime  is T2 - T1,
    NumCompTime is T3 - T2,
    $pp_assert_prob_stats2(SearchTime,NumCompTime),!.

% Sws = [sw(Id,Instances,Probs,Deltas,FixedP,FixedH),...]
$pp_compute_inside_feature(Goal,Prob) :-
    $pp_export_sw_info,
    $pc_prism_goal_id_get(Goal,Gid),
    $pc_compute_feature(Gid,Prob),!.

crf_probf(Goal) :-
    $pp_fprobf(Goal,Expls,1,0), \+ \+ print_graph(Expls,[lr('<=>')]).
crf_probfi(Goal) :-
    $pp_fprobf(Goal,Expls,1,1), \+ \+ print_graph(Expls,[lr('<=>')]).
crf_probfo(Goal) :-
    $pp_fprobf(Goal,Expls,1,2), \+ \+ print_graph(Expls,[lr('<=>')]).
crf_probfio(Goal) :-
    $pp_fprobf(Goal,Expls,1,4), \+ \+ print_graph(Expls,[lr('<=>')]).

crf_probf(Goal,Expls) :-
    $pp_fprobf(Goal,Expls,1,0).
crf_probfi(Goal,Expls) :-
    $pp_fprobf(Goal,Expls,1,1).
crf_probfo(Goal,Expls) :-
    $pp_fprobf(Goal,Expls,1,2).
crf_probfio(Goal,Expls) :-
    $pp_fprobf(Goal,Expls,1,4).

crf_probef(Goal) :-
    $pp_fprobf(Goal,Expls,0,0), \+ \+ print_graph(Expls,[lr('<=>')]).
crf_probefi(Goal) :-
    $pp_fprobf(Goal,Expls,0,1), \+ \+ print_graph(Expls,[lr('<=>')]).
crf_probefo(Goal) :-
    $pp_fprobf(Goal,Expls,0,2), \+ \+ print_graph(Expls,[lr('<=>')]).
crf_probefio(Goal) :-
    $pp_fprobf(Goal,Expls,0,4), \+ \+ print_graph(Expls,[lr('<=>')]).

crf_probef(Goal,Expls) :-
    $pp_fprobf(Goal,Expls,0,0).
crf_probefi(Goal,Expls) :-
    $pp_fprobf(Goal,Expls,0,1).
crf_probefo(Goal,Expls) :-
    $pp_fprobf(Goal,Expls,0,2).
crf_probefio(Goal,Expls) :-
    $pp_fprobf(Goal,Expls,0,4).

crf_probef(Goal,Expls,GoalHashTab,SwHashTab) :-
    $pp_fprobf(Goal,Expls,0,0),
    $pp_get_subgoal_hashtable(GoalHashTab),
    $pp_get_switch_hashtable(SwHashTab).
crf_probefi(Goal,Expls,GoalHashTab,SwHashTab) :-
    $pp_fprobf(Goal,Expls,0,1),
    $pp_get_subgoal_hashtable(GoalHashTab),
    $pp_get_switch_hashtable(SwHashTab).
crf_probefo(Goal,Expls,GoalHashTab,SwHashTab) :-
    $pp_fprobf(Goal,Expls,0,2),
    $pp_get_subgoal_hashtable(GoalHashTab),
    $pp_get_switch_hashtable(SwHashTab).
crf_probefio(Goal,Expls,GoalHashTab,SwHashTab) :-
    $pp_fprobf(Goal,Expls,0,4),
    $pp_get_subgoal_hashtable(GoalHashTab),
    $pp_get_switch_hashtable(SwHashTab).

%% PrMode is one of 0 (none), 1 (inside + feature)

$pp_fprobf(Goal,Expls,Decode,PrMode) :-
    $pp_require_tabled_probabilistic_atom(Goal,$msg(0006),$pp_fprobf/4),
    $pp_compute_expls_feature(Goal,Expls,Decode,PrMode),
    $pp_garbage_collect.

$pp_compute_expls_feature(Goal,Expls,Decode,PrMode) :-
    Goal = msw(I,V),!,
    $pp_require_ground(I,$msg(0101),$pp_fprobf/4),
    $pp_require_switch_outcomes(I,$msg(0102),$pp_fprobf/4),
    $pp_clean_infer_stats,
    ( ground(V) -> V = VCp ; copy_term(V,VCp) ),
    $pp_create_dummy_goal(DummyGoal),
    DummyBody = ($prism_expl_msw(I,VCp,Sid),
                 $pc_prism_goal_id_register(DummyGoal,Hid),
                 $prism_eg_path(Hid,[],[Sid])),
    Prog = [pred(DummyGoal,0,_,_,tabled(_,_,_,_),[(DummyGoal:-DummyBody)])],
    $pp_consult_preds_cond([],Prog),
    cputime(T0),
    $pp_compute_expls_feature(DummyGoal,Goal,Expls,Decode,PrMode,T0),!.

$pp_compute_expls_feature(Goal,Expls,Decode,PrMode) :-
    $pp_is_tabled_probabilistic_atom(Goal),
    ground(Goal),!,
    $pp_clean_infer_stats,
    cputime(T0),
    $pp_compute_expls_feature(Goal,_,Expls,Decode,PrMode,T0),!.

$pp_compute_expls_feature(Goal,Expls,Decode,PrMode) :-
    $pp_clean_infer_stats,
    copy_term(Goal,GoalCp),
    ( $pp_trans_one_goal(GoalCp,CompGoal) ->
      BodyGoal = CompGoal
    ; BodyGoal = (savecp(CP),Depth=0,
                  $pp_expl_interp_goal(GoalCp,Depth,CP,[],_,[],_,[],_,[],_))
    ),
    $pp_create_dummy_goal(DummyGoal),
    DummyBody = (BodyGoal,
                 $pc_prism_goal_id_register(GoalCp,GId),
                 $pc_prism_goal_id_register(DummyGoal,HId),
                 $prism_eg_path(HId,[GId],[])),
    Prog = [pred(DummyGoal,0,_,_,tabled(_,_,_,_),[(DummyGoal:-DummyBody)])],
    $pp_consult_preds_cond([],Prog),
    cputime(T0),
    $pp_compute_expls_feature(DummyGoal,Goal,Expls,Decode,PrMode,T0),!.

$pp_compute_expls_feature(Goal,GLabel,Expls,Decode,PrMode,T0) :-
    $pp_init_tables_aux,
    $pp_clean_graph_stats,
    $pp_init_tables_if_necessary,!,
    cputime(T1),
    $pp_find_explanations(Goal),
    cputime(T2),
    $pc_prism_goal_id_get(Goal,Gid),
    $pc_alloc_sort_egraph(Gid),
    cputime(T3),
    ( $pp_export_sw_info,
      $pc_compute_fprobf(PrMode)
    ),
    cputime(T4),
    $pc_import_sorted_graph_size(Size),
    $pp_build_expls_feature(Size,Decode,PrMode,GLabel,Expls),
    $pc_import_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    $pp_assert_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    cputime(T5),
    SearchTime  is T2 - T1,
    NumCompTime is T4 - T3,
    InfTime     is T5 - T0,
    $pp_assert_prob_stats2(SearchTime,NumCompTime),
    $pp_assert_prob_stats1(InfTime),!.

$pp_build_expls_feature(I0,_,_,_,Expls), I0 =< 0 =>
    Expls = [].
$pp_build_expls_feature(I0,Decode,PrMode,GLabel,Expls), I0 > 0 =>
    I is I0 - 1,
    $pc_import_sorted_graph_gid(I,Gid),
    $pc_import_sorted_graph_paths(I,Paths0),
    ( Decode == 0    -> Label = Gid
    ; nonvar(GLabel) -> Label = GLabel
    ; $pc_prism_goal_term(Gid,Label)
    ),
    ( PrMode == 0 -> Node = node(Label,Paths)  % fprobf
    ; PrMode == 4 ->                           % fprobfio
        $pp_get_gnode_probs(PrMode,Gid,Value),
        Node = node(Label,Paths,Value),
        Value = [_,Vo]
    ; $pp_get_gnode_probs(PrMode,Gid,Value), % fprobfi,fprobfo
      Node  = node(Label,Paths,Value),
      Value = Vo % ??
    ),
    $pp_decode_paths_feature(Paths0,Paths,Decode,PrMode,Vo),
    Expls = [Node|Expls1],!,
    $pp_build_expls_feature(I,Decode,PrMode,_,Expls1).

$pp_decode_paths_feature([],[],_Decode,_PrMode,_Vo).
$pp_decode_paths_feature([Pair|Pairs],[Path|Paths],Decode,PrMode,Vo) :-
    Pair = [Gids,Sids],
    $pp_decode_gnodes_feature(Gids,GNodes,Decode,PrMode,Vg),
    $pp_decode_snodes_feature(Sids,SNodes,Decode,PrMode,Vs),
    get_prism_flag(log_scale,LogScale),
    ( PrMode == 0 ->
        Path = path(GNodes,SNodes)
    ; PrMode == 1 -> ( LogScale == on -> Vi is Vg + Vs ; Vi is Vg * Vs),
        Path = path(GNodes,SNodes,Vi)
    ; PrMode == 2 ->
        Path = path(GNodes,SNodes,Vo)
    ; PrMode == 4 -> ( LogScale == on -> Vi is Vg + Vs ; Vi is Vg * Vs),
        Path = path(GNodes,SNodes,[Vi,Vo])
    ),!,
    $pp_decode_paths_feature(Pairs,Paths,Decode,PrMode,Vo).

$pp_decode_gnodes_feature(Gids,GNodes,Decode,PrMode,V) :-
    get_prism_flag(log_scale,LogScale),
    ( LogScale == on -> V0 = 0.0 ; V0 = 1.0 ),
    $pp_decode_gnodes_feature(Gids,GNodes,Decode,PrMode,LogScale,V0,V).

$pp_decode_gnodes_feature([],[],_Decode,_PrMode,_LogScale,V,V) :- !.
$pp_decode_gnodes_feature([Gid|Gids],[GNode|GNodes],Decode,PrMode,LogScale,V0,V) :-
    ( Decode == 0 -> Gid = Label
    ; $pc_prism_goal_term(Gid,Label)
    ),
    ( PrMode == 0 -> GNode = Label
    ; $pp_get_gnode_probs(PrMode,Gid,Value),
      GNode = gnode(Label,Value),
      ( LogScale == on ->
        V1 is Value + V0
      ; V1 is Value * V0
      )
    ),!,
    $pp_decode_gnodes_feature(Gids,GNodes,Decode,PrMode,LogScale,V1,V).

$pp_decode_snodes_feature(Sids,SNodes,Decode,PrMode,V) :-
    get_prism_flag(log_scale,LogScale),
    ( LogScale == on -> V0 = 0.0 ; V0 = 1.0 ),
    $pp_decode_snodes_feature(Sids,SNodes,Decode,PrMode,LogScale,V0,V).

$pp_decode_snodes_feature([],[],_Decode,_PrMode,_LogScale,V,V) :- !.
$pp_decode_snodes_feature([Sid|Sids],[SNode|SNodes],Decode,PrMode,LogScale,V0,V) :-
    ( Decode == 0 -> Sid = Label
    ; $pc_prism_sw_ins_term(Sid,Label)
    ),
    ( PrMode == 0 -> SNode = Label
    ; PrMode == 1 ->
        $pp_get_snode_feature(PrMode,Sid,[Pi,F]),
        SNode = snode(Label,[Pi,F]),
        ( LogScale == on ->
          V1 is Pi * F + V0
        ; V1 is exp(Pi * F) * V0
        )
    ; PrMode == 4 ->
        $pp_get_snode_feature(PrMode,Sid,[PiF,E]),
        SNode = snode(Label,[PiF1,E]),
        ( LogScale = on ->
          V1 is PiF + V0,PiF1 = PiF
        ; V1 is exp(PiF) * V0,PiF1 is exp(PiF)
        )
    ; $pp_get_snode_feature(PrMode,Sid,Value),
        SNode = snode(Label,Value),
        ( LogScale == on ->
          V1 is Value + V0
        ; V1 is Value * V0
        )
    ),!,
    $pp_decode_snodes_feature(Sids,SNodes,Decode,PrMode,LogScale,V1,V).

$pp_get_snode_feature(1,Sid,[Pi,F]) :-
    $pc_get_snode_feature(Sid,F,Pi),!.
$pp_get_snode_feature(2,Sid,E) :-
    $pc_get_snode_expectation(Sid,E),!.
$pp_get_snode_feature(4,Sid,[PiF,E]) :-
    $pc_get_snode_feature(Sid,F,Pi),
    PiF is Pi * F,
    $pc_get_snode_expectation(Sid,E),!.
%%%%
%%%%  bigarray.pl -- A large one-dimensional array for B-Prolog
%%%%

%%----------------------------------------

$pp_bigarray_unit(65535).        % max_arity

%%----------------------------------------

new_bigarray(Array,N), var(Array), integer(N), N > 0 =>
    $pp_bigarray_unit(M),
    Array = $bigarray(N,Body),
    $pp_new_bigarray(Body,N,M).

new_bigarray(Array,N) =>
    $pp_new_bigarray_throw(Array,N).

$pp_new_bigarray_throw(Array,N) :-
    ( var(Array) -> true
    ; throw(error(type_error(variable,Array),new_bigarray/2))
    ),
    ( nonvar(N) -> true
    ; throw(error(instantiation_error,new_bigarray/2))
    ),
    ( integer(N) -> true
    ; throw(error(type_error(integer,N),new_bigarray/2))
    ),
    ( N > 0 -> true
    ; throw(error(domain_error(greater_than_zero,N),new_bigarray/2))
    ), !,
    fail.                       % should not reach here

$pp_new_bigarray(Body,N,M), N =< M =>
    functor(Body,array,N).

$pp_new_bigarray(Body,N,M), N > M =>
    L is (N - 1) // M + 1,
    functor(Body,outer,L),
    $pp_new_bigarray(Body,1,N,M).

$pp_new_bigarray(Body,K,N,M), N =< M =>
    arg(K,Body,SubBody),
    functor(SubBody,array,N).

$pp_new_bigarray(Body,K,N,M), N > M =>
    arg(K,Body,SubBody),
    functor(SubBody,array,M),
    K1 is K + 1,
    N1 is N - M, !,
    $pp_new_bigarray(Body,K1,N1,M).

%%----------------------------------------

is_bigarray(Array), Array = $bigarray(_,_) => true.

bigarray_length(Array,L), Array = $bigarray(N,_) => L = N.
bigarray_length(Array,_) =>
    $pp_bigarray_length_throw(Array).

$pp_bigarray_length_throw(Array) :-
    ( nonvar(Array) -> true
    ; throw(error(instantiation_error,bigarray_length/2))
    ),
    ( Array ?= $bigarray(_,_) -> true
    ; throw(error(domain_error(bigarray,Array),bigarray_length/2))
    ), !,
    fail.                   % should not reach here

%%----------------------------------------

bigarray_get(Array,I,Value),
      Array = $bigarray(N,Body),
      integer(I),
      I >= 1,
      I =< N =>
    $pp_bigarray_get(Body,I,Value).

bigarray_get(Array,I,_Value) =>
    $pp_bigarray_access_throw(Array,I,bigarray_get/3).

bigarray_put(Array,I,Value),
      Array = $bigarray(N,Body),
      integer(I),
      I >= 1,
      I =< N =>
    $pp_bigarray_put(Body,I,Value).

bigarray_put(Array,I,_Value) =>
    $pp_bigarray_access_throw(Array,I,bigarray_put/3).

$pp_bigarray_access_throw(Array,I,Source) :-
    ( nonvar(Array) -> true
    ; throw(error(instantiation_error,Source))
    ),
    ( Array = $bigarray(N,_) -> true
    ; throw(error(domain_error(bigarray,Array),Source))
    ),
    ( nonvar(I) -> true
    ; throw(error(instantiation_error,Source))
    ),
    ( integer(I) -> true
    ; throw(error(type_error(integer,I),Source))
    ),
    ( I >= 1, I =< N -> true
    ; throw(error(domain_error(bigarray_index,I),Source))
    ), !,
    fail.                       % should not reach here

$pp_bigarray_get(Body,I,Elem), functor(Body,array,_) =>
    arg(I,Body,Elem).
$pp_bigarray_get(Body,I,Elem), functor(Body,outer,_) =>
    $pp_bigarray_unit(M),
    Q is (I - 1) //  M + 1,
    R is (I - 1) mod M + 1,
    arg(Q,Body,SubBody),
    arg(R,SubBody,Elem).

$pp_bigarray_put(Body,I,Elem), functor(Body,array,_) =>
    setarg(I,Body,Elem).
$pp_bigarray_put(Body,I,Elem), functor(Body,outer,_) =>
    $pp_bigarray_unit(M),
    Q is (I - 1) //  M + 1,
    R is (I - 1) mod M + 1,
    arg(Q,Body,SubBody),
    setarg(R,SubBody,Elem).

%%----------------------------------------

list_to_bigarray(List,Array) :-
    $pp_bigarray_unit(M),
    length(List,N),
    Array = $bigarray(N,Body),
    $pp_new_bigarray(Body,N,M),
    $pp_list_to_bigarray(List,1,Body).

$pp_list_to_bigarray(Xs,_,_), Xs = [] => true.
$pp_list_to_bigarray(Xs,K,Body), Xs = [X|Xs1] =>
    $pp_bigarray_put(Body,K,X),
    K1 is K + 1, !,
    $pp_list_to_bigarray(Xs1,K1,Body).

bigarray_to_list(Array,List), Array = $bigarray(N,Body) =>
    $pp_bigarray_to_list(Body,1,N,List).

$pp_bigarray_to_list(_,K,N,Xs), K > N =>
    Xs = [].
$pp_bigarray_to_list(Body,K,N,Xs), K =< N =>
    $pp_bigarray_get(Body,K,X),
    Xs = [X|Xs1],
    K1 is K + 1, !,
    $pp_bigarray_to_list(Body,K1,N,Xs1).

%%----------------------------------------
%% -*- Prolog -*-

%%======================================================================
%%
%%  [Notes on translation information]
%%
%%  This translator uses a term containing the global information shared
%%  by the translation processes.  It takes the form:
%%
%%      $trans_info(DoTable,TPredTab,NoDebug,PPredTab)
%%
%%  DoTable denotes whether probabilistic predicates should be tabled
%%  by default (i.e. unless declared in the source program); it takes
%%  1 if the predicates should be tabled; 0 otherwise.  In case of an
%%  unbound variable, DoTable should be considered to be 1.
%%
%%  TPredTab is a hashtable that contains tabled/non-tabled predicates
%%  which are compatible with the default (i.e. DoTable).  The key of
%%  each entry has the form P/N; the value is ignored.  In consultation
%%  mode where all probabilistic predicates are not tabled, TPredTab is
%%  just a free variable.
%%
%%  NoDebug indicates whether "write_call" should be disabled; any non-
%%  variable disables the feature.
%%
%%  PPredTab is a hashtable that contains probabilistic predicates found
%%  in the source program.  Each entry has the form P/N={0 or 1}, where
%%  the value is 1 if the predicate is tabled and 0 otherwise.
%%
%%======================================================================

%%----------------------------------------------------------------------
%%  Entry Point
%%----------------------------------------------------------------------

$pp_compile(PsmFile,DmpFile,OutFile) :-
    $pp_bpif_read_program(Prog0,PsmFile),
    new_hashtable(TPredTab),
    new_hashtable(PPredTab),
    Info = $trans_info(_DoTable,TPredTab,_NoDebug,PPredTab),
    $pp_trans_phase1(Prog0,Prog1,Info),
    $pp_trans_phase2(Prog1,Prog2,Info),
    $pp_trans_phase3(Prog2,Prog3,Info),
    $pp_trans_phase4(Prog3,Prog4,Info),
    $pp_trans_phase5(Prog4,Prog5,Info),
    Prog = Prog5,
    % $pp_dump_program(Prog),   % for debugging
    ( $pp_valid_program(Prog)
    ; $pp_raise_internal_error($msg(9802),invalid_compilation,$pp_compile/3)
    ),
    ( var(DmpFile) -> true ; $pp_save_program(Prog,DmpFile) ),
    $pp_bpif_compile_program(Prog,OutFile),!.


%%----------------------------------------------------------------------
%%  Phase #1: Scan the queries.
%%----------------------------------------------------------------------

$pp_trans_phase1(Prog0,Prog,Info) :-
    $pp_extract_decls(Prog0,Info),
    Prog = Prog0.

$pp_extract_decls([],_) => true.
$pp_extract_decls([Pred|Preds],Info), 
      Pred = pred($damon_load,0,_,_,_,[($damon_load:-Demon0)|_]) =>
    $pp_extract_decls_from_demons(Demon0,Info),!,
    $pp_extract_decls(Preds,Info).
$pp_extract_decls([_Pred|Preds],Info) =>
    $pp_extract_decls(Preds,Info).

$pp_extract_decls_from_demons((D1,D2),Info) =>
    $pp_extract_decls_from_demons(D1,Info),!,
    $pp_extract_decls_from_demons(D2,Info).
$pp_extract_decls_from_demons($query((p_table Preds)),Info) =>
    Info = $trans_info(DoTable,TPredTab,_,_),
    ( var(TPredTab) -> true    % consult mode
    ; DoTable == 1 ->
        $pp_add_preds_to_hashtable(Preds,TPredTab)
    ; var(DoTable) ->
        $pp_add_preds_to_hashtable(Preds,TPredTab),
        DoTable = 1
    ; DoTable == 0 ->
        $pp_raise_trans_error($msg(1101),mixed_table_declarations,$pp_trans_phase1/3)
    ; $pp_raise_unmatched_branches($pp_extract_decls_from_demons/2,
                                   query)
    ).
$pp_extract_decls_from_demons($query((p_not_table Preds)),Info) =>
    Info = $trans_info(DoTable,TPredTab,_,_),
    ( var(TPredTab) -> true    % consult mode
    ; DoTable == 0 ->
        $pp_add_preds_to_hashtable(Preds,TPredTab)
    ; var(DoTable) ->
        $pp_add_preds_to_hashtable(Preds,TPredTab),
        DoTable = 0
    ; DoTable == 1 ->
        $pp_raise_trans_error($msg(1101),mixed_table_declarations,$pp_trans_phase1/3)
    ; $pp_raise_unmatched_branches($pp_extract_decls_from_demons/2,
                                   p_not_table)
    ).
$pp_extract_decls_from_demons($query(disable_write_call),Info) =>
    Info = $trans_info(_,_,NoDebug,_),
    ( NoDebug == 1 -> true
    ; var(NoDebug) -> NoDebug = 1
    ; $pp_raise_unmatched_branches($pp_extract_decls_from_demons/2,
                                   disable_write_call)
    ).
$pp_extract_decls_from_demons(_,_Info) => true.

$pp_add_preds_to_hashtable((Pred,Preds),TPredTab) :- !,
    $pp_add_one_pred_to_hashtable(Pred,TPredTab),!,
    $pp_add_preds_to_hashtable(Preds,TPredTab).
$pp_add_preds_to_hashtable(Pred,TPredTab) :-
    $pp_add_one_pred_to_hashtable(Pred,TPredTab),!.

$pp_add_one_pred_to_hashtable(Pred,TPredTab) :-
    $pp_require_predicate_indicator(Pred,$msg(1102),$pp_trans_phase1/3),
    Pred = F/N,
    ( hashtable_get(TPredTab,F/N,_) -> true
    ; hashtable_register(TPredTab,F/N,1)
    ).

%%----------------------------------------------------------------------
%%  Phase #2: Process values/2-3.
%%----------------------------------------------------------------------

% We do not refer to the information objects here.
$pp_trans_phase2(Prog0,Prog,_Info) :-
    $pp_trans_values(Prog0,Prog1),
    $pp_replace_values(Prog1,Prog).

% translate the "values" declarations
$pp_trans_values(Preds0,Preds) :-
    $pp_trans_values(Preds0,Preds1,ValCls,Demon,DemonAux),
    Preds2 = [pred($pu_values,2,_Mode,_Delay,_Tabled,ValCls)|Preds1],
    DemonCl1 = ($damon_load:-Demon,DemonAux),
    DemonCl2 = ($damon_load:-true),
    Preds = [pred($damon_load,0,_,_,_,[DemonCl1,DemonCl2])|Preds2].

$pp_trans_values([],[],[],true,true).
$pp_trans_values([pred(F,2,_,_,_,Cls0)|Preds0],
                 Preds,ValCls,Demon,DemonAux) :-
    (F = values ; F = values_x),!,
    $pp_trans_values_clauses(Cls0,Cls1),
    append(Cls1,ValCls1,ValCls),!,
    $pp_trans_values(Preds0,Preds,ValCls1,Demon,DemonAux).
$pp_trans_values([pred(F,3,_,_,_,Cls0)|Preds0],
                 Preds,ValCls,Demon,DemonAux) :-
    (F = values ; F = values_x),!,
    $pp_trans_values_demon_clauses(Cls0,Cls1,DemonAux),
    append(Cls1,ValCls1,ValCls),!,
    $pp_trans_values(Preds0,Preds,ValCls1,Demon,_).
$pp_trans_values([pred(F,4,_,_,_,Cls0)|Preds0],
                 Preds,ValCls,Demon,DemonAux) :-
    (F = values ; F = values_x),!,
    $pp_trans_values_demon_clauses_4(Cls0,Cls1,DemonAux),
    append(Cls1,ValCls1,ValCls),!,
    $pp_trans_values(Preds0,Preds,ValCls1,Demon,_).
$pp_trans_values([pred($damon_load,0,_,_,_,[($damon_load:-Demon)|_])|Preds0],
                 Preds,ValCls,Demon,DemonAux) :- !,
    $pp_trans_values(Preds0,Preds,ValCls,_,DemonAux).
$pp_trans_values([P|Preds0],[P|Preds],ValCls,Demon,DemonAux) :- !,
    $pp_trans_values(Preds0,Preds,ValCls,Demon,DemonAux).

$pp_trans_values_clauses([],[]).
$pp_trans_values_clauses([Cl0|Cls0],[Cl|Cls]) :-
    $pp_trans_values_one_clause(Cl0,Cl),!,
    $pp_trans_values_clauses(Cls0,Cls).

/*for D-PRISM*/
$pp_trans_values_demon_clauses_4([],[],true).
$pp_trans_values_demon_clauses_4([Cl0|Cls0],[Cl|Cls],Demon) :-
      ( Cl0 = (values(Sw,Vals0,Demons,Demons0):-Body)   -> true
      ; Cl0 = (values_x(Sw,Vals0,Demons,Demons0):-Body) -> true
      ; Cl0 = values(Sw,Vals0,Demons,Demons0)           -> Body = true
      ; Cl0 = values_x(Sw,Vals0,Demons,Demons0)         -> Body = true
      ),
      $pp_build_expand_values(Vals0,Vals,Expand),
      Cl = ($pu_values(Sw,Vals):-Body,Expand),
      ( ground(Sw),ground(Demons)
          -> $pp_trans_values_demons(Sw,Demons,Demon1), Demon = (Demon1,Demon2)
      ; $pp_raise_warning($msg(1104),[Sw,Demons]), Demon = Demon2
      ),
      ( ground(Demons0)
          -> $pp_trans_values_demons(Sw,Demons0,Demon0), Demon2 = (Demon0,Demon3)
      ; $pp_raise_warning($msg(1104),[Sw,Demons0]), Demon = Demon3
      ),!,
      $pp_trans_values_demon_clauses(Cls0,Cls,Demon3).

$pp_trans_values_one_clause(Cl0,Cl) :-
    ( Cl0 = (values(Sw,Vals0):-Body)   -> true
    ; Cl0 = (values_x(Sw,Vals0):-Body) -> true
    ; Cl0 = values(Sw,Vals0)           -> Body = true
    ; Cl0 = values_x(Sw,Vals0)         -> Body = true
    ),
    $pp_build_expand_values(Vals0,Vals,Expand),
    Cl = ($pu_values(Sw,Vals):-Body,Expand).

$pp_trans_values_demon_clauses([],[],true).
$pp_trans_values_demon_clauses([Cl0|Cls0],[Cl|Cls],Demon) :-
    ( Cl0 = (values(Sw,Vals0,Demons):-Body)   -> true
    ; Cl0 = (values_x(Sw,Vals0,Demons):-Body) -> true
    ; Cl0 = values(Sw,Vals0,Demons)           -> Body = true
    ; Cl0 = values_x(Sw,Vals0,Demons)         -> Body = true
    ),
    $pp_build_expand_values(Vals0,Vals,Expand),
    Cl = ($pu_values(Sw,Vals):-Body,Expand),
    ( ground(Sw),ground(Demons)
        -> $pp_trans_values_demons(Sw,Demons,Demon1), Demon = (Demon1,Demon2)
    ; $pp_raise_warning($msg(1104),[Sw,Demons]), Demon = Demon2
    ),!,
    $pp_trans_values_demon_clauses(Cls0,Cls,Demon2).

$pp_trans_values_demons(_Sw,true,true) :- !.
$pp_trans_values_demons(Sw,(Demon0,Demons),(Demon2,Demon1)) :- !,
    $pp_trans_values_demons(Sw,Demon0,Demon2),!,
    $pp_trans_values_demons(Sw,Demons,Demon1).
$pp_trans_values_demons(Sw,Demon0,Demon) :-
    ( Demon0 = set@Params    -> Demon = $query(set_sw(Sw,Params))
    ; Demon0 = fix@Params    -> Demon = $query(fix_sw(Sw,Params))
    ; Demon0 = a@HParams     -> Demon = $query(set_sw_a(Sw,HParams))
    ; Demon0 = d@HParams     -> Demon = $query(set_sw_d(Sw,HParams))
    ; Demon0 = h@HParams     -> Demon = $query(set_sw_d(Sw,HParams))
    ; Demon0 = set_a@HParams -> Demon = $query(set_sw_a(Sw,HParams))
    ; Demon0 = set_d@HParams -> Demon = $query(set_sw_d(Sw,HParams))
    ; Demon0 = set_h@HParams -> Demon = $query(set_sw_d(Sw,HParams))
    ; Demon0 = fix_a@HParams -> Demon = $query(fix_sw_a(Sw,HParams))
    ; Demon0 = fix_d@HParams -> Demon = $query(fix_sw_d(Sw,HParams))
    ; Demon0 = fix_h@HParams -> Demon = $query(fix_sw_d(Sw,HParams))
    ; Demon0 = Params        -> Demon = $query(set_sw(Sw,Params))
    ; Demon0 = set_l@Lambdas -> Demon = $query(set_sw(Sw,Lambdas))
    ; Demon0 = set_w@Weights -> Demon = $query(set_sw_a(Sw,Weights))
    ).

$pp_build_expand_values(Vals0,Vals,Expand) :-
    ( $pp_unexpandable_values(Vals0) -> Expand = true, Vals = Vals0
    ; Expand = expand_values1(Vals0,Vals)   % use the no-exception version
    ).

% Checks if Vals only contains ground values that cannot be expanded by
% expand_values{,1}/2:
$pp_unexpandable_values(Vals) :-
    is_list(Vals),
    ground(Vals),
    $pp_unexpandable_values1(Vals).

$pp_unexpandable_values1([]).
$pp_unexpandable_values1([V|Vals]) :-
    V \= _Start-_End@_Step,
    V \= _Start-_End, !,
    $pp_unexpandable_values1(Vals).

% replace all appearances of values/2 in the clause bodies with get_values/2
$pp_replace_values([],[]).
$pp_replace_values([Pred0|Preds0],[Pred|Preds]) :-
    Pred0 = pred(F,N,Mode,Delay,Tabled,Cls0),
    Pred = pred(F,N,Mode,Delay,Tabled,Cls),
    $pp_replace_values_clauses(Cls0,Cls),!,
    $pp_replace_values(Preds0,Preds).
    
$pp_replace_values_clauses([],[]).
$pp_replace_values_clauses([Cl0|Cls0],[Cl|Cls]) :-
    $pp_replace_values_one_clause(Cl0,Cl),!,
    $pp_replace_values_clauses(Cls0,Cls).

$pp_replace_values_one_clause(Cl0,Cl) :-
    ( Cl0 = (Head:-Body0) ->
        $pp_replace_values_body(Body0,Body), Cl = (Head:-Body)
    ; Cl = Cl0
    ).

$pp_replace_values_body((G1,G2),(NG1,NG2)) :- !,
    $pp_replace_values_body(G1,NG1),
    $pp_replace_values_body(G2,NG2).
$pp_replace_values_body((G1;G2),(NG1;NG2)) :- !,
    $pp_replace_values_body(G1,NG1),
    $pp_replace_values_body(G2,NG2).
$pp_replace_values_body(not(G),not(NG)) :- !,
    $pp_replace_values_body(G,NG).
$pp_replace_values_body((\+ G),(\+ NG)) :- !,
    $pp_replace_values_body(G,NG).
$pp_replace_values_body((C->G),(NC->NG)) :- !,
    $pp_replace_values_body(C,NC),
    $pp_replace_values_body(G,NG).
$pp_replace_values_body(write_call(G),write_call(NG)) :- !,
    $pp_replace_values_body(G,NG).
$pp_replace_values_body(write_call(Opts,G),write_call(Opts,NG)) :- !,
    $pp_replace_values_body(G,NG).
$pp_replace_values_body((?? G),(?? NG)) :- !,
    $pp_replace_values_body(G,NG).
$pp_replace_values_body((??* G),(??* NG)) :- !,
    $pp_replace_values_body(G,NG).
$pp_replace_values_body((??> G),(??> NG)) :- !,
    $pp_replace_values_body(G,NG).
$pp_replace_values_body((??< G),(??< NG)) :- !,
    $pp_replace_values_body(G,NG).
$pp_replace_values_body((??+ G),(??+ NG)) :- !,
    $pp_replace_values_body(G,NG).
$pp_replace_values_body((??- G),(??- NG)) :- !,
    $pp_replace_values_body(G,NG).
$pp_replace_values_body(values(Sw,Vals),get_values(Sw,Vals)) :-  !.
$pp_replace_values_body(G,G).


%%----------------------------------------------------------------------
%%  Phase #3: Find probabilistic predicates.
%%----------------------------------------------------------------------

$pp_trans_phase3(Prog0,Prog,Info) :-
    $pp_analyze(Prog0,Info),
    Prog = Prog0.

$pp_analyze(Prog,Info) :-
    Info = $trans_info(_,_,_,PPredTab),
    $pp_collect_preds(Prog,PPredTab),
    $pp_infer_prob_preds_fixpoint(Prog,Info),
    $pp_complete_prob_preds(Info),
    $pp_assert_prob_preds(Prog,Info).

% collect the predicates appearing in the program
$pp_collect_preds([],_).
$pp_collect_preds([pred($damon_load,0,_,_,_,_)|Preds],PPredTab) :- !,
    hashtable_register(PPredTab,$damon_load/0,_),!,
    $pp_collect_preds(Preds,PPredTab).
$pp_collect_preds([pred(values,2,_,_,_,_)|Preds],PPredTab) :- !,
    hashtable_register(PPredTab,values/2,_),!,
    $pp_collect_preds(Preds,PPredTab).
$pp_collect_preds([pred(F,N,_Mode,_Delay,_Tabled,_Cls)|Preds],PPredTab) :-
    hashtable_register(PPredTab,F/N,_),!,
    $pp_collect_preds(Preds,PPredTab).

$pp_infer_prob_preds_fixpoint(Prog,Info) :-
    Info = $trans_info(_,_,_,PPredTab),
    global_set($pg_prob_tab_updated,0,0),
    $pp_infer_prob_preds(Prog,PPredTab),
        % if some probabilistic predicate have been newly found, try again:
    ( global_get($pg_prob_tab_updated,0,1)
        -> $pp_infer_prob_preds_fixpoint(Prog,Info)
    ; true
    ).

$pp_infer_prob_preds([],_PPredTab) => true.
$pp_infer_prob_preds([pred(values,2,_,_,_,_)|Preds],PPredTab) =>
    $pp_infer_prob_preds(Preds,PPredTab).
$pp_infer_prob_preds([pred(F,N,_Mode,_Delay,_Tab,Cls)|Preds],PPredTab) =>
    hashtable_get(PPredTab,F/N,IsProb),
    ( var(IsProb) -> $pp_infer_prob_cls(Cls,IsProb,PPredTab),
      ( nonvar(IsProb) -> global_set($pg_prob_tab_updated,0,1)
      ; true
      )
    ; true
    ),!,
    $pp_infer_prob_preds(Preds,PPredTab).

$pp_infer_prob_cls([],_IsProb,_PPredTab) => true.
$pp_infer_prob_cls([Cl|Cls],IsProb,PPredTab) =>
    $pp_infer_prob_cl(Cl,IsProb,PPredTab),
    ( var(IsProb) -> $pp_infer_prob_cls(Cls,IsProb,PPredTab)
    ; true
    ).

$pp_infer_prob_cl((_H:-B),IsProb,PPredTab) =>
    $pp_infer_prob_body(B,IsProb,PPredTab).
$pp_infer_prob_cl(_H,_IsProb,_PPredTab) => true.

$pp_infer_prob_body((G1,G2),IsProb,PPredTab) =>
    $pp_infer_prob_body(G1,IsProb,PPredTab),
    ( var(IsProb) -> $pp_infer_prob_body(G2,IsProb,PPredTab)
    ; true
    ).
$pp_infer_prob_body((C->G1;G2),IsProb,PPredTab) =>
    $pp_infer_prob_body(C,IsProb,PPredTab),
    ( var(IsProb) ->
        $pp_infer_prob_body(G1,IsProb,PPredTab),
        ( var(IsProb) -> $pp_infer_prob_body(G2,IsProb,PPredTab)
        ; true
        )
      ; true
    ).
$pp_infer_prob_body((G1;G2),IsProb,PPredTab) =>
    $pp_infer_prob_body(G1,IsProb,PPredTab),
    ( var(IsProb) -> $pp_infer_prob_body(G2,IsProb,PPredTab)
    ; true
    ).
$pp_infer_prob_body(not(G1),IsProb,PPredTab) =>
    $pp_infer_prob_body(G1,IsProb,PPredTab).
$pp_infer_prob_body((\+ G1),IsProb,PPredTab) =>
    $pp_infer_prob_body(G1,IsProb,PPredTab).
$pp_infer_prob_body((C->G1),IsProb,PPredTab) =>    
    $pp_infer_prob_body(C,IsProb,PPredTab),
    ( var(IsProb) -> $pp_infer_prob_body(G1,IsProb,PPredTab)
    ; true
    ).
$pp_infer_prob_body(write_call(G1),IsProb,PPredTab) =>
    $pp_infer_prob_body(G1,IsProb,PPredTab).
$pp_infer_prob_body(write_call(_,G1),IsProb,PPredTab) =>
    $pp_infer_prob_body(G1,IsProb,PPredTab).
$pp_infer_prob_body((?? G1),IsProb,PPredTab) =>
    $pp_infer_prob_body(G1,IsProb,PPredTab).
$pp_infer_prob_body((??* G1),IsProb,PPredTab) =>
    $pp_infer_prob_body(G1,IsProb,PPredTab).
$pp_infer_prob_body((??> G1),IsProb,PPredTab) =>
    $pp_infer_prob_body(G1,IsProb,PPredTab).
$pp_infer_prob_body((??< G1),IsProb,PPredTab) =>
    $pp_infer_prob_body(G1,IsProb,PPredTab).
$pp_infer_prob_body((??+ G1),IsProb,PPredTab) =>
    $pp_infer_prob_body(G1,IsProb,PPredTab).
$pp_infer_prob_body((??- G1),IsProb,PPredTab) =>
    $pp_infer_prob_body(G1,IsProb,PPredTab).
$pp_infer_prob_body(msw(_,_,_),IsProb,_PPredTab) => IsProb = 1.
$pp_infer_prob_body(msw(_,_),IsProb,_PPredTab) => IsProb = 1.
$pp_infer_prob_body(soft_msw(_,_,_),IsProb,_PPredTab) => IsProb = 1.
$pp_infer_prob_body(soft_msw(_,_),IsProb,_PPredTab) => IsProb = 1.
$pp_infer_prob_body(b_msw(_,_,_),IsProb,_PPredTab) => IsProb = 1.
$pp_infer_prob_body(b_msw(_,_),IsProb,_PPredTab) => IsProb = 1.
$pp_infer_prob_body(G,IsProb,PPredTab) :-
    functor(G,F,N),
    hashtable_get(PPredTab,F/N,IsProb1),!,
    ( nonvar(IsProb1) -> IsProb = 1
    ; true
    ).
$pp_infer_prob_body(_G,_IsProb,_PPredTab).  /* G: undefined predicates */
    
$pp_complete_prob_preds(Info) :-
    Info = $trans_info(_,_,_,PPredTab),
    hashtable_keys_to_list(PPredTab,Preds),
    $pp_complete_prob_preds(Preds,PPredTab).

$pp_complete_prob_preds([],_).
$pp_complete_prob_preds([F/N|Preds],PPredTab) :-
    hashtable_get(PPredTab,F/N,IsProb),!,
    ( var(IsProb) -> IsProb = 0
    ; true
    ),!,
    $pp_complete_prob_preds(Preds,PPredTab).

$pp_assert_prob_preds([],_). 
$pp_assert_prob_preds([pred(F,N,_,_,_,_)|Preds],Info) :-
    Info = $trans_info(DoTable,TPredTab,_,PPredTab),
    hashtable_get(PPredTab,F/N,IsProb),!,
    ( IsProb = 1 ->
        $pp_abolish_compiled_pred(F,N),
        ( $pd_is_prob_pred(F,N) -> true
        ; assert($pd_is_prob_pred(F,N))
        ),
        ( $pp_is_tabled_prob_pred(F/N,DoTable,TPredTab)
            -> ( $pd_is_tabled_pred(F,N) -> true
               ; assert($pd_is_tabled_pred(F,N))
               )
        ; true
        )
    ; true
    ),!,
    $pp_assert_prob_preds(Preds,Info).

$pp_abolish_compiled_pred(F,N) :-
    $pp_trans_prob_pred_name(F,NewF),
    global_del(NewF,N),!.


%%----------------------------------------------------------------------
%%  Phase #4: Translate the probabilistic predicates.
%%----------------------------------------------------------------------

% [Note] Mode indicators in B-Prolog:
%   c (or +) : closed term
%   f (or -) : free variable
%   nv       : non-variable term
%   d (or ?) : dont-know term

$pp_trans_phase4(Prog0,Prog,Info) :-
    $pp_trans_prob_preds(Prog0,Prog,Info).

$pp_trans_prob_preds([],Prog,_Info) => Prog = [].
$pp_trans_prob_preds([Pred|Preds],Prog,Info),
      Pred = pred(F,N,Mode,Delay,Tabled,Cls) =>
    Info = $trans_info(_,_,NoDebug,_),
    ( $pd_is_prob_pred(F,N) ->
        Prog = [pred(F,N,Mode,Delay,_,Cls1),NewPred|Prog1],
        ( $pd_is_tabled_pred(F,N) ->
            NewTabled = tabled(_,_,_,_),
            ( nonvar(Mode) -> NewMode = [f|Mode] ; true),
            NewArity is N + 1
        ; % \+ $is_tabled_pred(F,N)
          ( nonvar(Mode) -> NewMode = [d,d,d,d|Mode]
          ; true
          ),
          NewArity is N + 4
        ),
        NewPred = pred(NewF,NewArity,NewMode,_,NewTabled,NewCls),
        $pp_trans_prob_pred_name(F,NewF),
        copy_term(Cls,ClsCp),   % Pred and NewPred do not share variables
        $pp_trans_prob_cls(ClsCp,NewCls,NewF,NewTabled,Info)
    ; % \+ $pd_is_prob_pred(F,N)
      Prog = [pred(F,N,Mode,Delay,Tabled,Cls1)|Prog1]
    ),
    ( var(NoDebug) -> Cls1 = Cls
    ; $pp_strip_write_call_cls(Cls,Cls1)  % just strip the write_call predicates
    ),!,
    $pp_trans_prob_preds(Preds,Prog1,Info).

$pp_trans_prob_cls([],Cls,_F,_Tabled,_Info) => Cls = [].
$pp_trans_prob_cls([(Head0:-Body0)|Cls0],Cls,F,Tabled,Info) =>
    Cls = [(Head:-Body)|Cls1],
    Head0 =.. [_|Args],
    ((nonvar(Tabled),Tabled = tabled(_,_,_,_)) ->
        Head =.. [F,Gid0|Args],
        $pp_trans_prob_body(Body0,Body1,Gids,[],Sids,[],Info),
        (get_prism_flag(crf_enable,on) ->
          ( Gids == [], Sids == [] -> RegistPath = true
          ; RegistPath = $prism_eg_path(Gid0,Gids,Sids)
          )
        ;RegistPath = $prism_eg_path(Gid0,Gids,Sids)
        ),
        Body = (Body1,
                $pc_prism_goal_id_register(Head0,Gid0),
                RegistPath)
    ; % Non-tabled
      Head =.. [F,Gids,GidsR,Sids,SidsR|Args],
      $pp_trans_prob_body(Body0,Body1,Gids,GidsR,Sids,SidsR,Info),
      Body = Body1
    ),!,
    $pp_trans_prob_cls(Cls0,Cls1,F,Tabled,Info).
$pp_trans_prob_cls([Head|Cls0],Cls,F,Tabled,Info) =>
    $pp_trans_prob_cls([(Head:-true)|Cls0],Cls,F,Tabled,Info).

$pp_trans_prob_body((G1,G2),NewGoal,Gids,GidsR,Sids,SidsR,Info) =>
    NewGoal = (NG1,NG2),
    $pp_trans_prob_body(G1,NG1,Gids,Gids1,Sids,Sids1,Info),
    $pp_trans_prob_body(G2,NG2,Gids1,GidsR,Sids1,SidsR,Info).
$pp_trans_prob_body((C->A;B),NewGoal,Gids,GidsR,Sids,SidsR,Info) =>
    NewGoal = (InitVars,
               (NC->
                (NA,Gids=GidsCp1,Sids=SidsCp1,GidsR=GidsRCp1,SidsR=SidsRCp1)
               ;(NB,Gids=GidsCp2,Sids=SidsCp2,GidsR=GidsRCp2,SidsR=SidsRCp2))),
    $pp_trans_prob_body(C,NC,GidsCp1,GidsCp3,SidsCp1,SidsCp3,Info),
    $pp_trans_prob_body(A,NA,GidsCp3,GidsRCp1,SidsCp3,SidsRCp1,Info),
    $pp_trans_prob_body(B,NB,GidsCp2,GidsRCp2,SidsCp2,SidsRCp2,Info),
    vars_set((NA;NB),Vars),
    $pp_gen_initialize_var([Vars,Gids,Sids,GidsR,SidsR,
                            GidsCp1,SidsCp1,GidsRCp1,SidsRCp1,
                            GidsCp2,SidsCp2,GidsRCp2,SidsRCp2,
                            GidsCp3,SidsCp3],InitVars).
$pp_trans_prob_body((A;B),NewGoal,Gids,GidsR,Sids,SidsR,Info) =>
    NewGoal = (InitVars,
               ((NA,Gids=GidsCp1,Sids=SidsCp1,GidsR=GidsRCp1,SidsR=SidsRCp1)
               ;(NB,Gids=GidsCp2,Sids=SidsCp2,GidsR=GidsRCp2,SidsR=SidsRCp2))),
    $pp_trans_prob_body(A,NA,GidsCp1,GidsRCp1,SidsCp1,SidsRCp1,Info),
    $pp_trans_prob_body(B,NB,GidsCp2,GidsRCp2,SidsCp2,SidsRCp2,Info),
    vars_set((NA;NB),Vars),
    $pp_gen_initialize_var([Vars,Gids,Sids,GidsR,SidsR,
                            GidsCp1,SidsCp1,GidsRCp1,SidsRCp1,
                            GidsCp2,SidsCp2,GidsRCp2,SidsRCp2],InitVars).
$pp_trans_prob_body(not(G),NewGoal,Gids,GidsR,Sids,SidsR,Info) =>
    NewGoal = not(NG),
    Gids = GidsR,
    Sids = SidsR,
    $pp_trans_prob_body(G,NG,Gids,_,Sids,_,Info).
$pp_trans_prob_body(\+(G),NewGoal,Gids,GidsR,Sids,SidsR,Info) =>
    NewGoal = \+(NG),
    Gids = GidsR,
    Sids = SidsR,
    $pp_trans_prob_body(G,NG,Gids,_,Sids,_,Info).
$pp_trans_prob_body((C->A),NewGoal,Gids,GidsR,Sids,SidsR,Info) =>
    NewGoal = (NC->NA),
    $pp_trans_prob_body(C,NC,Gids,Gids1,Sids,Sids1,Info),
    $pp_trans_prob_body(A,NA,Gids1,GidsR,Sids1,SidsR,Info).
$pp_trans_prob_body(Goal,NewGoal,Gids,GidsR,Sids,SidsR,Info),
      Goal = write_call(Goal1) =>
    $pp_trans_prob_body(write_call([],Goal1),
                        NewGoal,Gids,GidsR,Sids,SidsR,Info).
$pp_trans_prob_body(Goal,NewGoal,Gids,GidsR,Sids,SidsR,Info),
      Goal = write_call(Opts,Goal1) =>
    Info = $trans_info(_,_,NoDebug,_),
    ( $pp_is_write_callable(Goal1) -> true
    ; $pp_raise_trans_error($msg(1103),not_write_callable,$pp_trans_phase4/3)
    ),
    ( var(NoDebug) -> $pp_write_call_build(Opts,Goal1,NewGoal1,NewGoal)
    ; NewGoal1 = NewGoal
    ),!,
    $pp_trans_prob_body(Goal1,NewGoal1,Gids,GidsR,Sids,SidsR,Info).
$pp_trans_prob_body(Goal,NewGoal,Gids,GidsR,Sids,SidsR,Info),
      Goal = (?? Goal1) =>
    $pp_trans_prob_body(write_call([],Goal1),
                        NewGoal,Gids,GidsR,Sids,SidsR,Info).
$pp_trans_prob_body(Goal,NewGoal,Gids,GidsR,Sids,SidsR,Info),
      Goal = (??* Goal1) =>
    $pp_trans_prob_body(write_call([all],Goal1),
                        NewGoal,Gids,GidsR,Sids,SidsR,Info).
$pp_trans_prob_body(Goal,NewGoal,Gids,GidsR,Sids,SidsR,Info),
      Goal = (??> Goal1) =>
    $pp_trans_prob_body(write_call([call],Goal1),
                        NewGoal,Gids,GidsR,Sids,SidsR,Info).
$pp_trans_prob_body(Goal,NewGoal,Gids,GidsR,Sids,SidsR,Info),
      Goal = (??< Goal1) =>
    $pp_trans_prob_body(write_call([exit+fail],Goal1),
                        NewGoal,Gids,GidsR,Sids,SidsR,Info).
$pp_trans_prob_body(Goal,NewGoal,Gids,GidsR,Sids,SidsR,Info),
      Goal = (??+ Goal1) =>
    $pp_trans_prob_body(write_call([exit],Goal1),
                        NewGoal,Gids,GidsR,Sids,SidsR,Info).
$pp_trans_prob_body(Goal,NewGoal,Gids,GidsR,Sids,SidsR,Info),
      Goal = (??- Goal1) =>
    $pp_trans_prob_body(write_call([fail],Goal1),
                        NewGoal,Gids,GidsR,Sids,SidsR,Info).
$pp_trans_prob_body(Goal,NewGoal,Gids,GidsR,Sids,SidsR,_Info),
      Goal = msw(I,V) =>
    Gids = GidsR,
    Sids = [Sid|SidsR],
    NewGoal = $prism_expl_msw(I,V,Sid).
$pp_trans_prob_body(Goal,NewGoal,Gids,GidsR,Sids,SidsR,_Info),
      Goal = soft_msw(I,V) =>
    Gids = GidsR,
    Sids = [Sid|SidsR],
    NewGoal = $prism_expl_msw(I,V,Sid).
$pp_trans_prob_body(Goal,NewGoal,Gids,GidsR,Sids,SidsR,_Info),
      Goal = b_msw(I,V) =>
    Gids = GidsR,
    Sids = [Sid|SidsR],
    NewGoal = $prism_expl_msw(I,V,Sid).
$pp_trans_prob_body(Goal,NewGoal,Gids,GidsR,Sids,SidsR,Info) :-
    Info = $trans_info(DoTable,TPredTab,_,_),
    functor(Goal,F,N),
    $pd_is_prob_pred(F,N),!,
    Goal =.. [_|Args],
    $pp_trans_prob_pred_name(F,NewF),
    ( $pp_is_tabled_prob_pred(F/N,DoTable,TPredTab) ->
        NewGoal =.. [NewF,Gid|Args], 
        Gids = [Gid|GidsR],
        Sids = SidsR
    ; NewGoal =.. [NewF,Gids,GidsR,Sids,SidsR|Args]
    ).
$pp_trans_prob_body(Goal,NewGoal,Gids,GidsR,Sids,SidsR,_Info) :-
    Sids = SidsR,
    Gids = GidsR,
    Goal = NewGoal.

$pp_strip_write_call_cls([],Cls)=> Cls = [].
$pp_strip_write_call_cls([(Head:-Body0)|Cls0],Cls) =>
    Cls = [(Head:-Body)|Cls1],
    $pp_strip_write_call_body(Body0,Body),!,
    $pp_strip_write_call_cls(Cls0,Cls1).
$pp_strip_write_call_cls([Head|Cls0],Cls) =>
    Cls = [Head|Cls1],!,
    $pp_strip_write_call_cls(Cls0,Cls1).

$pp_strip_write_call_body((A0,B0),Goal) =>
    Goal = (A1,B1),
    $pp_strip_write_call_body(A0,A1),
    $pp_strip_write_call_body(B0,B1).
$pp_strip_write_call_body((A0->B0;C0),Goal) =>
    Goal = (A1->B1;C1),
    $pp_strip_write_call_body(A0,A1),
    $pp_strip_write_call_body(B0,B1),
    $pp_strip_write_call_body(C0,C1).
$pp_strip_write_call_body((A0;B0),Goal) =>
    Goal = (A1;B1),
    $pp_strip_write_call_body(A0,A1),
    $pp_strip_write_call_body(B0,B1).
$pp_strip_write_call_body(not(A0),Goal) =>
    Goal = not(A1),
    $pp_strip_write_call_body(A0,A1).
$pp_strip_write_call_body(\+(A0),Goal) =>
    Goal = \+(A1),
    $pp_strip_write_call_body(A0,A1).
$pp_strip_write_call_body((A0->B0),Goal) =>
    Goal = (A1->B1),
    $pp_strip_write_call_body(A0,A1),
    $pp_strip_write_call_body(B0,B1).
$pp_strip_write_call_body(write_call(A0),Goal) => Goal = A1,
    $pp_strip_write_call_body(A0,A1).
$pp_strip_write_call_body(write_call(_,A0),Goal) => Goal = A1,
    $pp_strip_write_call_body(A0,A1).
$pp_strip_write_call_body((??  A0),Goal) => Goal = A1,
    $pp_strip_write_call_body(A0,A1).
$pp_strip_write_call_body((??* A0),Goal) => Goal = A1,
    $pp_strip_write_call_body(A0,A1).
$pp_strip_write_call_body((??> A0),Goal) => Goal = A1,
    $pp_strip_write_call_body(A0,A1).
$pp_strip_write_call_body((??< A0),Goal) => Goal = A1,
    $pp_strip_write_call_body(A0,A1).
$pp_strip_write_call_body((??+ A0),Goal) => Goal = A1,
    $pp_strip_write_call_body(A0,A1).
$pp_strip_write_call_body((??- A0),Goal) => Goal = A1,
    $pp_strip_write_call_body(A0,A1).
$pp_strip_write_call_body(Goal0,Goal) => Goal = Goal0.

$pp_gen_initialize_var(VarsL,InitVars):-
    flatten(VarsL,Vars0),
    sort(Vars0,Vars),
    $pp_gen_initialize_var_aux(Vars,InitVarsL),
    list_to_and(InitVarsL,InitVars).

$pp_gen_initialize_var_aux([],[]).
$pp_gen_initialize_var_aux([Var|Vars],InitVars):-
    ( var(Var) -> InitVars = ['_$initialize_var'(Var)|InitVars1]
    ; InitVars = InitVars1
    ),!,
    $pp_gen_initialize_var_aux(Vars,InitVars1).

%%----------------------------------------------------------------------
%%  Phase #5: Add assert calls to the first demon call.
%%----------------------------------------------------------------------

$pp_trans_phase5(Prog0,Prog,Info) :-
    $pp_add_assert_calls(Prog0,Prog,Info).

$pp_add_assert_calls([],[],_).
$pp_add_assert_calls([Pred|Preds],[Pred1|Preds1],Info) :-
    Pred = pred($damon_load,0,_,_,_,[($damon_load:-Demon)|DemonCls]),
    $pp_build_assert_calls(Info,AssertCalls),
    Demon1 = ($query(retractall($pd_is_prob_pred(_,_))),
              $query(retractall($pd_is_tabled_pred(_,_))),
              $query(call(AssertCalls)),
              Demon),
    Pred1 = pred($damon_load,0,_,_,_,[($damon_load:-Demon1)|DemonCls]),!,
    $pp_add_assert_calls(Preds,Preds1,Info).
$pp_add_assert_calls([Pred|Preds],[Pred|Preds1],Info) :- !,
    $pp_add_assert_calls(Preds,Preds1,Info).

$pp_build_assert_calls(Info,AssertCalls) :-
    Info = $trans_info(_,_,_,PPredTab),
    hashtable_to_list(PPredTab,Pairs),
    $pp_build_assert_calls1(Pairs,Info,AssertGs),
    list_to_and(AssertGs,AssertCalls).

$pp_build_assert_calls1([],_,[]).
$pp_build_assert_calls1([Pair|Pairs],Info,AssertGs) :-
    Info = $trans_info(DoTable,TPredTab,_,_),
    ( Pair = (F/N=V) ->
      ( V == 1 ->
          AssertGs = [assert($pd_is_prob_pred(F,N))|AssertGs2],
          ( $pp_is_tabled_prob_pred(F/N,DoTable,TPredTab) ->
              AssertGs2 = [assert($pd_is_tabled_pred(F,N))|AssertGs1]
          ; AssertGs2 = AssertGs1
          )
      ; V == 0 -> AssertGs = AssertGs1
      ; $pp_raise_unmatched_branches($pp_build_assert_calls1/3,value)
      )
    ; $pp_raise_unmatched_branches($pp_build_assert_calls1/3,pair)
    ),!,
    $pp_build_assert_calls1(Pairs,Info,AssertGs1).


%%----------------------------------------
%% Auxiliary predicates for translation

'_$initialize_var'(_).
'_$if_then_else'(C,A,B) :- (C->A;B).

%%----------------------------------------
%% Miscellaneous routines

$pp_trans_prob_pred_name(F,NewF) :-
    name(F,FString),
    append("$pu_expl_",FString,NewFString),
    name(NewF,NewFString).


$pp_is_tabled_prob_pred(F/N,DoTable,TPredTab) :-
    ( var(TPredTab) -> fail   % consult mode
    ; true
    ),!,
    ( DoTable == 1 -> hashtable_get(TPredTab,F/N,_)
    ; DoTable == 0 ->
        ( hashtable_get(TPredTab,F/N,_) -> fail
        ; true
        )
    ; var(DoTable) -> true
    ),!.


$pp_add_conj_to_list((A,B),List) =>
    $pp_add_conj_to_list(A,List),!,
    $pp_add_conj_to_list(B,List).
$pp_add_conj_to_list(A,List) =>
    $member1(A,List).
%% -*- Prolog -*-

%%======================================================================
%%
%% This module provides a pretty-printer for programs.  In the following
%% preidcates, <Prog> should be a valid program in the B-Prolog internal
%% form; otherwise they would behave in an unexpected way.
%% 
%% $pp_dump_program(Prog) :-
%%     Writes <Prog> into the current output stream.
%% 
%% $pp_dump_program(S,Prog) :-
%%     Writes <Prog> into the stream <S>.
%% 
%% $pp_save_program(Prog,File) :-
%%     Writes <Prog> into <File>.
%% 
%%======================================================================

%%--------------------------------
%%  Entry Point

$pp_dump_program(Prog) :-
    current_output(S), $pp_dump_program(S,Prog).

$pp_save_program(Prog,File) :-
    open(File,write,S), $pp_dump_program(S,Prog), close(S).

$pp_dump_program(S,Prog) :-
    $pp_dump_split(Prog,Damon,Preds),
    $pp_dump_damon(S,Damon),
    $pp_dump_decls(S,Preds),
    $pp_dump_preds(S,Preds).


%%--------------------------------
%%  Separator

$pp_dump_nl(S,L) :-
    var(L), !,
    nl(S),
    L = 1.
$pp_dump_nl(_,L) :-
    nonvar(L), !.


%%--------------------------------
%%  Split $damon_load/0

$pp_dump_split(Prog,Damon,Preds) :-
    Q = pred($damon_load,0,_,_,_,[($damon_load :- Damon)|_]),
    select(Q,Prog,Preds), !.


%%--------------------------------
%%  Start-up Queries

$pp_dump_damon(S,Damon) :-
    $pp_dump_damon(S,Damon,_).

$pp_dump_damon(S,Damon,L) :-
    Damon = (A,B), !,
    $pp_dump_damon(S,A,L),
    $pp_dump_damon(S,B,L).
$pp_dump_damon(_,Damon,_) :-
    Damon = true, !.
$pp_dump_damon(S,Damon,L) :-
    Damon = $query(Query), !,
    $pp_dump_nl(S,L),
    \+ \+ $pp_dump_query(S,Query).

$pp_dump_query(S,Query) :-
    prettyvars(Query),
    format(S,":- ~k.~n",[Query]).


%%--------------------------------
%%  Declarations

$pp_dump_decls(S,Preds) :-
    $pp_dump_m_decls(S,Preds,_),
    $pp_dump_t_decls(S,Preds,_).


%%--------------------------------
%%  Mode Declarations

$pp_dump_m_decls(_,Preds,_) :- Preds == [], !.
$pp_dump_m_decls(S,Preds,L) :- Preds = [Pred|Preds1], !,
    Pred = pred(F,N,M,_,_,_),
    $pp_dump_m_decl(S,F,N,M,L),
    $pp_dump_m_decls(S,Preds1,L).

$pp_dump_m_decl(_,_,_,M,_) :- var(M), !.
$pp_dump_m_decl(S,F,N,M,L) :- M = [_|_], !,
    $pp_dump_nl(S,L),
    format(S,":- mode ~q(",[F]),
    $pp_dump_m_spec(S,N,M),
    format(S,").~n",[]).

$pp_dump_m_spec(S,N,Mode) :- N == 1, !,
    Mode = [M],
    $pp_mode_symbol(M,Sym), !,   % M can be an unbound variable
    write(S,Sym).
$pp_dump_m_spec(S,N,Mode) :- N >= 2, !,
    Mode = [M|Mode1],
    $pp_mode_symbol(M,Sym), !,   % M can be an unbound variable
    write(S,Sym),
    write(S,','),
    N1 is N - 1,
    $pp_dump_m_spec(S,N1,Mode1).

$pp_mode_symbol(d ,? ).
$pp_mode_symbol(? ,? ).
$pp_mode_symbol(c ,+ ).
$pp_mode_symbol(+ ,+ ).
$pp_mode_symbol(f ,- ).
$pp_mode_symbol(- ,- ).
$pp_mode_symbol(nv,nv).


%%--------------------------------
%%  Table Decalrations

$pp_dump_t_decls(_,Preds,_) :- Preds == [], !.
$pp_dump_t_decls(S,Preds,L) :- Preds = [Pred|Preds1], !,
    Pred = pred(F,N,_,_,T,_),
    $pp_dump_t_decl(S,F,N,T,L),
    $pp_dump_t_decls(S,Preds1,L).

$pp_dump_t_decl(_,_,_,T,_) :- var(T), !.
$pp_dump_t_decl(S,F,N,T,L) :- nonvar(T), !,
    $pp_dump_nl(S,L),
    format(S,":- table ~q/~d.~n",[F,N]).


%%--------------------------------
%%  Clauses

$pp_dump_preds(_,Preds) :- Preds == [], !.
$pp_dump_preds(S,Preds) :- Preds = [Pred|Preds1], !,
    Pred = pred(_,_,_,_,_,Cls),
    $pp_dump_clauses(S,Cls,_),
    $pp_dump_preds(S,Preds1).

$pp_dump_clauses(_,Cls,_) :- Cls == [], !.
$pp_dump_clauses(S,Cls,L) :- Cls = [Cl|Cls1], !,
    $pp_dump_nl(S,L),
    portray_clause(S,Cl),
    $pp_dump_clauses(S,Cls1,L).
%% -*- Prolog -*-

%%======================================================================
%%
%% This module provides a quick validator for programs represented in the
%% B-Prolog internal form.
%%
%% $pp_valid_program(Prog) :-
%%     Succeeds if and only if <Prog> is a valid program.
%%
%%======================================================================

%%--------------------------------
%%  Entry Point

$pp_valid_program(Prog) :-
    new_hashtable(Done),
    $pp_valid_program_aux(Prog,Done).

$pp_valid_program_aux(Prog,_), Prog == [] =>
    true.
$pp_valid_program_aux(Prog,Done), Prog = [Pred|Prog1] =>
    ( $pp_valid_prog_elem(Pred,Done) ->
      true
    ; $pp_emit_message($msg(1100),[Pred]), fail
    ),
    arg(1,Pred,F),
    arg(2,Pred,N),
    hashtable_register(Done,F/N,1),
    $pp_valid_program_aux(Prog1,Done).


%%--------------------------------
%%  Predicate

$pp_illegal_pred(':-',2).

$pp_valid_prog_elem(Pred,Done) :-
    Pred = pred(F,N,_,_,_,_),
    atom(F), integer(N), N >= 0,
    \+ ( $pp_illegal_pred(F,N) ; hashtable_get(Done,F/N,_) ),
    $pp_valid_prog_pred(Pred).

$pp_valid_prog_pred(Pred),
      Pred = pred(F,N,M,D,T,Cls),
      F == $damon_load,
      N == 0 =>
    var(M),
    var(D),
    var(T),
    Cls = [Cl0,Cl1],
    Cl0 = ($damon_load :- Body),
    Cl1 = ($damon_load :- true),
    $pp_valid_damon(Body).
$pp_valid_prog_pred(Pred),
      Pred = pred(F,N,M,D,T,Cls) =>
    $pp_valid_mspec(N,M),
    $pp_valid_delay(D),
    $pp_valid_table(T),
    $pp_valid_clauses(F,N,D,Cls).


%%--------------------------------
%%  $damon_load/0

$pp_valid_damon(G) :- G = (A,B), !,
    $pp_valid_damon(A),
    $pp_valid_damon(B).
$pp_valid_damon(G) :- G == true, !,
    true.
$pp_valid_damon(G) :- G = $query(_), !,
    true.
$pp_valid_damon(G) :- callable(G), !,
    true.

%%--------------------------------
%%  Mode Spec

$pp_valid_mspec(_,M), var(M)    => true.
$pp_valid_mspec(N,M), nonvar(M) =>
    $pp_valid_mspec_loop(N,M).

$pp_valid_mspec_loop(N,ModeL), N == 0 => ModeL == [].
$pp_valid_mspec_loop(N,ModeL), N >= 1 =>
    ModeL = [Mode|ModeL1],
    $pp_valid_mode(Mode),
    N1 is N - 1,
    $pp_valid_mspec_loop(N1,ModeL1).

$pp_valid_mode(M), M == c  => true.
$pp_valid_mode(M), M == f  => true.
$pp_valid_mode(M), M == nv => true.
$pp_valid_mode(M), M == d  => true.

%%--------------------------------
%%  Delay

$pp_valid_delay(D), var(D) => true.
$pp_valid_delay(D), D == 1 => true.


%%--------------------------------
%%  Table

$pp_valid_table(T), var(T) => true.
$pp_valid_table(T),
      T = tabled(U1,U2,U3,U4),
      var(U1),
      var(U2),
      var(U3),
      var(U4) => true.


%%--------------------------------
%%  Clauses

$pp_valid_clauses(_,_,_,Cls), Cls == [] => true.
$pp_valid_clauses(F,N,D,Cls), Cls = [Cl|Cls1] =>
    $pp_valid_clause(F,N,D,Cl),
    $pp_valid_clauses(F,N,D,Cls1).

$pp_valid_clause(F,N,_,Cl), Cl = (H :- _) =>
    nonvar(H),
    functor(H,F,N).
$pp_valid_clause(F,N,D,Cl), Cl = delay(Cl1) =>
    D == 1,
    $pp_valid_clause(F,N,_,Cl1).
$pp_valid_clause(F,N,_,Cl) =>
    nonvar(Cl),
    functor(Cl,F,N).
%% -*- Prolog -*-

/*
========================================================================
  
This module provides a simple interface to the B-Prolog compiler.
In the following description, <Prog> denotes a program represented in
the B-Prolog internal form (i.e. a list of pred/6).

$pp_bpif_read_program(-Prog,+File) :-
    Loads <Prog> from <File>.

$pp_bpif_compile_program(+Prog,+File) :-
    Compiles <Prog> and saves the resultant byte-code into <File>.

========================================================================
*/

%%--------------------------------
%%  Entry Point

$pp_bpif_read_program(Prog,File) :-
    getclauses1(File,Prog,0).

$pp_bpif_compile_program(Prog0,File) :-
    $pp_preproc_program(Prog0,Prog1),
    phase_1_process(Prog1,Prog2),
    compileProgToFile(_,File,Prog2).


%%--------------------------------
%%  Preprocessing

$pp_preproc_program(Prog0,Prog1) :-
    new_hashtable(AuxTable),
    $pp_preproc_program(Prog0,Prog1,AuxTable,0).

$pp_preproc_program(Prog0,Prog1,AuxTable,K),
      Prog0 = [pred(F,N,M,D,T,Cls0)|Prog0R] =>
    Prog1 = [pred(F,N,M,D,T,Cls1)|Prog1R],
    $pp_preproc_clauses(Cls0,Cls1,AuxTable,K,NewK),
    $pp_preproc_program(Prog0R,Prog1R,AuxTable,NewK).
$pp_preproc_program(Prog0,Prog1,AuxTable,_),
      Prog0 = [] =>
    hashtable_values_to_list(AuxTable,Prog1).

$pp_preproc_clauses(Cls0,Cls1,AuxTable,K,NewK), Cls0 = [Cl0|Cls0R] =>
    Cls1 = [Cl1|Cls1R],
    preprocess_cl(Cl0,Cl1,AuxTable,K,TmpK,1),
    $pp_preproc_clauses(Cls0R,Cls1R,AuxTable,TmpK,NewK).
$pp_preproc_clauses(Cls0,Cls1,_,K,NewK), Cls0 = [] =>
    Cls1 = [],
    K = NewK.


%%--------------------------------
%%  Loading programs at runtime

% erase all solutions in the current table unconditionally
$pp_consult_preds(Preds,Prog) :-
    $pp_consult_preds_aux(Preds,Prog),
    c_INITIALIZE_TABLE,!.

% erase solutions depending on the clean_table flag
$pp_consult_preds_cond(Preds,Prog) :-
    $pp_consult_preds_aux(Preds,Prog),
    ( get_prism_flag(clean_table,on) -> c_INITIALIZE_TABLE
    ; true
    ),!.

$pp_consult_preds_aux([],Prog) :- var(Prog),!.
$pp_consult_preds_aux([],Prog) :-
    closetail(Prog),
    phase_1_process(Prog,Prog2),
    $pd_tmp_out(TmpOutBase,TmpOutFull),
    $output_file(TmpOutBase,OutFile),
    compileProgToFile(tmp,OutFile,Prog2),
    b_LOAD_cfc(OutFile,_,0),
    ( file_exists(TmpOutFull) -> delete_file(TmpOutFull)
    ; true
    ),!.
$pp_consult_preds_aux([Pred|Preds],CompiledProg):-
    consult_pred(Pred,CompiledProg),!,
    $pp_consult_preds_aux(Preds,CompiledProg).
/* tracer and debugger of B-Prolog,
   Neng-Fa Zhou
*/
/*********************** eval_call(Call) no trace ******************/
eval_call(Goal,_CP), var(Goal) =>
    handle_exception(illegal_predicate, Goal).
/*
eval_call((A : B),CP) =>
    eval_call(A,CP),
    '_$cutto'(CP),
    eval_call(B,CP).
eval_call((A ? B),CP) =>
    eval_call(A,CP),
    eval_call(B,CP).
*/
eval_call(true,_CP) => true.
eval_call((A,B),CP) =>
    eval_call(A,CP),
    eval_call(B,CP).
eval_call((A -> B ; C),CP) =>
    eval_if_then_else(C,CP,A,B).
eval_call((A;B),CP) =>
    eval_or(A,B,CP).
eval_call((A -> B),CP) =>
    eval_if_then(A,B,CP).
eval_call(not(A),_CP) =>
     '_$savecp'(CP1),
    eval_not(A,CP1).
eval_call(\+(A),_CP) =>
     '_$savecp'(CP1),
    eval_not(A,CP1).
eval_call('!',CP) =>
    '_$cutto'(CP).
eval_call(call(X),_CP) =>
    '_$savecp'(CP1),
    eval_call(X,CP1).
eval_call(Xs,_CP), [_|_]<=Xs =>
    consult_list(Xs).
eval_call(Goal,_CP), b_IS_CONSULTED_c(Goal) =>
    '_$savecp'(CP1),
    clause(Goal,Body),
    eval_call(Body,CP1).
eval_call(Goal,_CP) =>
    call(Goal).

%% Prism-specific part
eval_call('_$initialize_var'(_Vars),_CP) => true.
eval_call('_$if_then_else'(C,A,B),CP) => eval_call((C->A;B),CP).

eval_if_then_else(_C,CP,A,B) ?=>
    '_$savecp'(CP1),
    eval_call(A,CP1),!,
    eval_call(B,CP).
eval_if_then_else(C,CP,_A,_B) =>
    eval_call(C,CP).

eval_or(A,_B,CP) ?=>
    eval_call(A,CP).
eval_or(_A,B,CP) =>
    eval_call(B,CP).

eval_if_then(A,B,CP) =>
    '_$savecp'(CP1),
     eval_call(A,CP1),!,
    eval_call(B,CP).

eval_not(A,CP) ?=>
    eval_call(A,CP),!,
    fail.
eval_not(_A,_CP) => true.

/*********************** eval_call(Call) ******************/
$trace_call(Call), b_IS_DEBUG_MODE =>
    '_$savecp'(CP),
    eval_debug_call(Call,0,CP).
$trace_call(Call) =>
    '_$savecp'(CP),
    eval_call(Call,CP).

eval_debug_call(Goal,_Depth,_CP), var(Goal) =>
    handle_exception(illegal_predicate, Goal).
/*
eval_debug_call((A : B),Depth,CP) =>
    eval_debug_call(A,Depth,CP),
    '_$cutto'(CP),
    eval_debug_call(B,Depth,CP).
eval_debug_call((A ? B),Depth,CP) =>
    eval_debug_call(A,Depth,CP),
    eval_debug_call(B,Depth,CP).
*/
eval_debug_call((A,B),Depth,CP) =>
    eval_debug_call(A,Depth,CP),
    eval_debug_call(B,Depth,CP).
eval_debug_call((A -> B ; C),Depth,CP) =>
    eval_debug_if_then_else(C,Depth,CP,A,B).
eval_debug_call((A;B),Depth,CP) =>
    eval_debug_or(A,B,Depth,CP).
eval_debug_call((A -> B),Depth,CP) =>
    eval_debug_if_then(A,B,Depth,CP).
eval_debug_call(not(A),Depth,_CP) =>
    '_$savecp'(CP1),
    eval_debug_not(A,Depth,CP1).
eval_debug_call(\+(A),Depth,_CP) =>
    '_$savecp'(CP1),
    eval_debug_not(A,Depth,CP1).
eval_debug_call('!',_Depth,CP) =>
    '_$cutto'(CP).
eval_debug_call('_$cutto'(X),_Depth,_CP) =>
    '_$cutto'(X).
eval_debug_call($trace_call(X),_Depth,_CP) =>
    $trace_call(X).
eval_debug_call(call(X),Depth,_CP) =>
    '_$savecp'(CP1),
    eval_debug_call(X,Depth,CP1).
eval_debug_call($query(X),Depth,CP) =>
    eval_debug_call(X,Depth,CP).
eval_debug_call(true,_Depth,_CP) => true.
eval_debug_call($internal_match(X,Y),_Depth,_CP) =>
    nonvar(Y),X=Y.
eval_debug_call(trace,_Depth,_CP) => trace.
eval_debug_call(op(Prec,Fix,Op),_Depth,_CP) =>
    op(Prec,Fix,Op).
eval_debug_call(dynamic(Calls),_Depth,_CP) =>
    dynamic(Calls).
eval_debug_call(nospy,_Depth,_CP) =>
    nospy.
eval_debug_call(nospy(X),_Depth,_CP) =>
    nospy(X).
eval_debug_call(notrace,_Depth,_CP) =>
    notrace.
eval_debug_call(spy(S),_Depth,_CP) =>
    spy(S).
eval_debug_call(nospy(S),_Depth,_CP) =>
    nospy(S).
eval_debug_call(Xs,_Depth,_CP), [_|_]<=Xs =>
    consult_list(Xs).
eval_debug_call(Goal,Depth,_CP) =>
    c_SAVE_AR(AR),
    c_next_global_call_number(CallNo),
    $eval_and_monitor_call(Goal,Depth,CallNo,AR).

%% Prism-specific part
eval_debug_call(Goal,_Depth,_CP), var(Goal) =>
    handle_exception(illegal_predicate, Goal).
eval_debug_call('_$initialize_var'(_Vars),_Depth,_CP) => true.
eval_debug_call('_$if_then_else'(C,A,B),Depth,CP) =>
    eval_debug_call((C->A;B),Depth,CP).
eval_debug_call(msw(Sw,V),Depth,CP) =>
    $pp_require_ground(Sw,$msg(0101),msw/2),
    c_SAVE_AR(AR),
    c_next_global_call_number(CallNo),
    $prism_sample_msw(Sw,V,Depth,CP,CallNo,AR).

eval_debug_if_then_else(_C,Depth,CP,A,B) ?=>
    '_$savecp'(NewCP),
    eval_debug_call(A,Depth,NewCP),!,
    eval_debug_call(B,Depth,CP).
eval_debug_if_then_else(C,Depth,CP,_A,_B) =>
    eval_debug_call(C,Depth,CP).

eval_debug_or(A,_B,Depth,CP) ?=>
    eval_debug_call(A,Depth,CP).
eval_debug_or(_A,B,Depth,CP) =>
    eval_debug_call(B,Depth,CP).

eval_debug_if_then(A,B,Depth,CP) =>
    '_$savecp'(NewCP),
    eval_debug_call(A,Depth,NewCP),!,
    eval_debug_call(B,Depth,CP).

eval_debug_not(A,Depth,CP) ?=>
    eval_debug_call(A,Depth,CP),!,
    fail.
eval_debug_not(_A,_Depth,_CP) => true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
$eval_and_monitor_call(Call,Depth,CallNo,AR) ?=>
    c_get_dg_flag(Flag),
    $print_call(Flag,'   Call: ',Call,Depth,CallNo,AR),
    Depth1 is 1+Depth,
    $eval_single_call(Call,Depth1),
    $switch_skip_off(AR),
    $eval_call_exit(Call,Depth,CallNo,AR).
$eval_and_monitor_call(Call,Depth,CallNo,AR) =>
    c_get_dg_flag(Flag),
    $print_call(Flag,'   Fail: ',Call,Depth,CallNo,AR),
    fail.

$eval_call_exit(Call,Depth,CallNo,AR) ?=>
    c_get_dg_flag(Flag),
    $print_call(Flag,'   Exit: ',Call,Depth,CallNo,AR).
$eval_call_exit(Call,Depth,CallNo,AR) =>
    c_get_dg_flag(Flag),
    $print_call(Flag,'   Redo: ',Call,Depth,CallNo,AR),
    fail.

$eval_single_call(Call,Depth), b_IS_CONSULTED_c(Call) =>
    '_$savecp'(CP),
    clause(Call,Body), 
    eval_debug_call(Body,Depth,CP).
$eval_single_call(Call,_Depth) =>
    call(Call).

/*
 ---------------------------------------------  
 |repeat | skip | leap | creep | spy | debug | 
 ---------------------------------------------  
#define DG_FLAG_DEBUG 0x1
#define DG_FLAG_SPY 0x2
#define DG_FLAG_C 0x4
#define DG_FLAG_L 0x8
#define DG_FLAG_S 0x10
#define DG_FLAG_R 0x20
*/

%% Prism-specific part
$print_call(_F,_T,$pu_values(_,_),             _D,_CNo,_AR) => true.
$print_call(_F,_T,$pd_is_prob_pred(_,_),       _D,_CNo,_AR) => true.
$print_call(_F,_T,$pd_is_tabled_pred(_,_),     _D,_CNo,_AR) => true.
$print_call(_F,_T,$pd_parameters(_,_,_),       _D,_CNo,_AR) => true.
$print_call(_F,_T,$pd_hyperparameters(_,_,_,_),_D,_CNo,_AR) => true.
$print_call(_F,_T,$pd_expectations(_,_,_),     _D,_CNo,_AR) => true.
$print_call(_F,_T,$pd_hyperexpectations(_,_,_),_D,_CNo,_AR) => true.
$print_call(_F,_T,$pd_fixed_parameters(_),     _D,_CNo,_AR) => true.
$print_call(_F,_T,$pd_fixed_hyperparameters(_),_D,_CNo,_AR) => true.
$print_call(_Flag,_Type,write_call(_),  _Depth,_CallNo,_AR) => true.
$print_call(_Flag,_Type,write_call(_,_),_Depth,_CallNo,_AR) => true.
$print_call(_Flag,_Type,(??  _),        _Depth,_CallNo,_AR) => true.
$print_call(_Flag,_Type,(??* _),        _Depth,_CallNo,_AR) => true.
$print_call(_Flag,_Type,(??> _),        _Depth,_CallNo,_AR) => true.
$print_call(_Flag,_Type,(??< _),        _Depth,_CallNo,_AR) => true.
$print_call(_Flag,_Type,(??+ _),        _Depth,_CallNo,_AR) => true.
$print_call(_Flag,_Type,(??- _),        _Depth,_CallNo,_AR) => true.
$print_call(Flag,Type,$prism_expl_msw(I,V,_SwId),Depth,CallNo,AR) =>
    $print_call(Flag,Type,msw(I,V),Depth,CallNo,AR).

$print_call(Flag,Type,Call,Depth,CallNo,_AR),
      Flag /\ 2'100000 =:= 2'100000 => %repeat
    '$readl_userio'(I,O),
    tab(2*Depth),write(Type),write('('),write(CallNo),write(') '),
    print(Call),nl,
    '$readl_resetio'(I,O).
$print_call(Flag,Type,Call,Depth,CallNo,AR),
      Flag /\ 2'1000 =:= 2'1000 ?=>    %leap
    c_is_spy_point(Call),!,
    $real_print_call(Type,Call,Depth,CallNo),
    $next_monitor_instruction(Type,Call,Depth,CallNo,AR).
$print_call(Flag,Type,Call,Depth,CallNo,AR),
      Flag /\ 2'100 =:= 2'100 =>      %creap
    $real_print_call(Type,Call,Depth,CallNo),
    $next_monitor_instruction(Type,Call,Depth,CallNo,AR).
$print_call(Flag,Type,Call,Depth,CallNo,AR),
      Flag /\ 2'10000 =:= 2'10000 ?=> %skip
    c_is_skip_call_no(AR),!,
    $real_print_call(Type,Call,Depth,CallNo),
    $next_monitor_instruction(Type,Call,Depth,CallNo,AR).
$print_call(_Flag,_Type,_Call,_Depth,_AR,_CallNo) => true.

$real_print_call(Type,Call,Depth,CallNo):-
    '$readl_userio'(I,O),
    tab(2*Depth),write(Type),write('('),write(CallNo),write(') '),
    print(Call),writename(' ?'),
    '$readl_resetio'(I,O).

$next_monitor_instruction(Type,Call,Depth,CallNo,AR):-
    $get_monitor_instruction(Inst),
    $process_monitor_instruction(Type,Call,Depth,CallNo,AR,Inst).

/*
#define DG_FLAG_DEBUG 0x1
#define DG_FLAG_SPY 0x2
#define DG_FLAG_C 0x4
#define DG_FLAG_L 0x8
#define DG_FLAG_S 0x10
#define DG_FLAG_R 0x20
*/
$process_monitor_instruction(_Type,_Call,_Depth,_CallNo,_AR,0'a) =>
    abort.                     % abort
$process_monitor_instruction(_Type,_Call,_Depth,_CallNo,_AR,0'r) =>
    c_set_dg_flag(2'100000).   % repeat
$process_monitor_instruction(_Type,_Call,_Depth,_CallNo,_AR,0'c) =>
    c_set_dg_flag(2'100).      % creep
$process_monitor_instruction(_Type,_Call,_Depth,_CallNo,_AR,10) =>
    c_set_dg_flag(2'100).      % return
$process_monitor_instruction(_Type,_Call,_Depth,_CallNo,_AR,0'n) =>
    c_get_dg_flag(Flag),
    NewFlag is Flag/\2'11, 
    c_init_dg_flag(NewFlag).   % no trace
$process_monitor_instruction(_Type,_Call,_Depth,_CallNo,_AR,0'l) =>
    c_set_dg_flag(2'1000).     % leap
$process_monitor_instruction(Type,Call,Depth,CallNo,AR,0's) =>
    ((Type=='   Fail: ';Type=='   Exit: ')->
      write(user_output,'Option not applicable'),nl(user_output),
      $real_print_call(Type,Call,Depth,CallNo),
      $next_monitor_instruction(Type,Call,Depth,CallNo,AR);
     c_set_dg_flag(2'10000),
     c_set_skip_ar(AR)).       % skip
$process_monitor_instruction(Type,Call,Depth,CallNo,AR,_) => % other ?
    $print_help(Type),
    $real_print_call(Type,Call,Depth,CallNo),
    $next_monitor_instruction(Type,Call,Depth,CallNo,AR).

$print_help(_Type):-
    write(user,' a     abort'),nl(user),
    write(user,' ?     help'),nl(user),
    write(user,' h     help'),nl(user),
    write(user,'<cr>   creep'),nl(user),
    write(user,' c     creep'),nl(user),
    write(user,' h     help'),nl(user),
    write(user,' l     leap'),nl(user),
    write(user,' n     nodebug'),nl(user),
    write(user,' r     repeat creep'),nl(user),
    write(user,' s     skip'),nl(user),nl(user).
    
$get_monitor_instruction(Command):-
    '$readl_userio'(I,O),
    get0(Command),
    $get_until_return(Command),
    '$readl_resetio'(I,O).

$get_until_return(10) => true.
$get_until_return(_Command) =>
    get0(X),
    $get_until_return(X).

$switch_skip_off(AR):-
    c_is_skip_call_no(AR),!,
    c_set_skip_ar(0),
    c_set_dg_flag(2'100). % creep
$switch_skip_off(_) => true.
    
    
/**************trace/1 spy/1******************/
trace =>
    c_init_dg_flag(1).

spy(S), var(S) =>
    c_get_spy_points(S).
spy([X|Xs]) =>
    spy(X),
    spy(Xs).
spy([]) => true.
spy(Pred), F/N<=Pred, atom(F),integer(N) =>
    (c_CURRENT_PREDICATE(F,N)->
    '$readl_userio'(I,O),
    write('Spy point '), write(Pred), write(' has been set.'),nl,
    '$readl_resetio'(I,O),
    c_add_spy_point(F,N);
    handle_exception(predicate_not_exist, Pred)).
spy(F), atom(F) =>
    $search_preds(F,25,[],X),    
    (X\==[]->spy(X); handle_exception(predicate_not_exist, F)).
spy(F):-
    handle_exception(illegal_argument, spy(F)).

$search_preds(_X,N,P0,P), N<0 =>
    P=P0.
$search_preds(X,N,P0,P):-
    c_CURRENT_PREDICATE(X,N),!,
    N1 is N-1,
    $search_preds(X,N1,[X/N|P0],P).
$search_preds(X,N,P0,P) =>
    N1 is N-1,
    $search_preds(X,N1,P0,P).
    
notrace =>
    c_init_dg_flag(0),
    nospy.

nospy([X|Xs]) =>
    nospy(X),
    nospy(Xs).
nospy([]) => true.
nospy(F/N), atom(F), integer(N) =>
    c_remove_spy_point(F,N).
nospy(F), atom(F) =>
    $search_preds(F,25,[],X),
    nospy(X).
nospy(F) =>
    handle_exception(illegal_predicate, nospy(F)).

nospy:-
    c_remove_spy_points.

trace(Call) =>
    $trace_call(Call).
$prprobg(Goal):-
    (get_prism_flag(log_scale,on)->Text='Log-probability';Text='Probability'),
    prprobg(Goal,L),
    foreach(S in L,[G,P],
    ([P,G]=S,format("~w of ~w is: ~15f~n",[Text,G,P]))).

$prprobg(Goal,Probs):-
    vars_set(Goal,Vars),
    probf(Goal,N),
    N=[node(_,Z)|_],
    foreach(S in Z,ac(Ps,[]),[P,G|Vars],
    (S=path([G],[]),Goal=G->prprob(G,P),Ps^1=[[P,G]|Ps^0];true)),
	sort(>,Ps,Probs).
$probg(Goal,Probs):-
    vars_set(Goal,Vars),
    probf(Goal,N),
    N=[node(_,Z)|_],
    foreach(S in Z,ac(Ps,[]),[P,G|Vars],
    (S=path([G],[]),Goal=G->prob(G,P),Ps^1=[[P,G]|Ps^0];true)),
	sort(>,Ps,Probs).


lin_prob(Goal) :-
  lin_prob(Goal,P),
  (get_prism_flag(log_scale,on)->Text='Log-probability';Text='Probability'),
  format("~w of ~w is: ~15f~n",[Text,Goal,P]).

find_scc(Goal,Components,CompT) :-
  % Testing goal
  probefi(Goal,ExpGraph),
  % Transforming graph
  $pp_trans_graph(ExpGraph,HGraph,_,_),
  % Finding SCC
  $pp_find_scc(HGraph,Components,CompT).

lin_prob(Goal,Prob) :-
  % Testing goal
  probefi(Goal,ExpGraph),
  % Transforming graph
  $pp_trans_graph(ExpGraph,HGraph,_,_),
  % Finding SCC
  $pp_find_scc(HGraph,Components,CompTable),
  % Solving graph
  $pp_solve_graph(HGraph,Components,CompTable,ProbTable),
  bigarray_get(ProbTable,1,Prob),!.



lin_probfi(Goal):-
  lin_probfi(Goal,Expls),print_graph(Expls,[lr('<=>')]).
lin_probefi(Goal):-
  lin_probefi(Goal,Expls),print_graph(Expls,[lr('<=>')]).

lin_probfi(Goal,Expls) :-
  $pp_cyc_probfi(Goal,_,1,Expls).
lin_probefi(Goal,Expls) :-
  $pp_cyc_probfi(Goal,_,0,Expls).

$pp_cyc_replace_prob(Id,Mapping,P):-
  !,(X=(Id,P),member(X,Mapping)),!.

$pp_cyc_probfi(Goal,OrgExpls,Decode,NewExpls2) :-
  % Testing goal
  (Decode=0->probefi(Goal,OrgExpls);probfi(Goal,OrgExpls)),
  % Transforming graph
  $pp_trans_graph(OrgExpls,HGraph,_,_),
  % Finding SCC
  $pp_find_scc(HGraph,Components,CompTable),
  % Solving graph
  $pp_solve_graph(HGraph,Components,CompTable,ProbTable),
  bigarray_length(ProbTable,Size),
  % Creating mapping from ProbTableIndex to NodeID
  %new_bigarray(Mapping,Size),
  %foreach((E,I) in (OrgExpls,1..Size),[Id,T1,T2],
  %  (E=node(Id,T1,T2),bigarray_put(Mapping,I,Id))),
  % Creating mapping ProbTableIndex and NodeID
  Src @= [Index : Index in 1..Size],
  maplist(I,Ex,Pair,(
      bigarray_get(ProbTable,I,Temp),
      Ex=node(Id,_,_),
      %bigarray_get(Mapping,I,Index2),
      Pair=(Id,Temp)
    ),Src,OrgExpls,IMapping),!,
  $pc_import_sorted_graph_size(ESize),
  % Replacing probabilities
  maplist(E,NewExpl,(E=node(Id,Paths,P),
  maplist(Path,NewPath,(Path=path(GNodes,SNodes,PP),
  maplist(GNode,NewGNode,(GNode=gnode(GID,GP),$pp_cyc_replace_prob(GID,IMapping,NewGP),NewGNode=gnode(GID,NewGP)),GNodes,NewGNodes)
  ,NewPath=path(NewGNodes,SNodes,PP)),Paths,NewPaths)
  ,$pp_cyc_replace_prob(Id,IMapping,NewP),NewExpl = node(Id, NewPaths ,NewP) ),OrgExpls,NewExpls),
  % TODO:Re-calculate path-probabilities
  get_prism_flag(log_scale,LogScale),
  %( LogScale == on -> Vi is Vg + Vs ; Vi is Vg * Vs),
  maplist(E,NewExpl,(E=node(Id,Paths,P),
  maplist(Path,NewPath,(Path=path(GNodes,SNodes,PP),
  maplist(GNode,GP,(GNode=gnode(GID,GP)),GNodes,GNodeProbs),
  maplist(SNode,SP,(SNode=snode(SID,SP)),SNodes,SNodeProbs),
  ( LogScale == on -> (
    reducelist(Y0,X,Y1,(Y1 is Y0+X),GNodeProbs,0.0,TempP),
    reducelist(Y0,X,Y1,(Y1 is Y0+X),SNodeProbs,TempP,PathP)
  );(
    reducelist(Y0,X,Y1,(Y1 is Y0*X),GNodeProbs,1.0,TempP),
    reducelist(Y0,X,Y1,(Y1 is Y0*X),SNodeProbs,TempP,PathP)
  )),
  NewPath=path(GNodes,SNodes,PathP)),Paths,NewPaths)
  ,NewExpl = node(Id, NewPaths ,P) ),NewExpls,NewExpls2),
  %
  $pp_garbage_collect.


$cyc_learn(Goals) :-
$pp_cyc_learn_core(ml,Goals).

$pp_cyc_learn_core(Mode,Goals) :-
    $pp_learn_check_goals(Goals),
    $pp_learn_message(MsgS,MsgE,MsgT,MsgM),
    $pc_set_em_message(MsgE),
    cputime(Start),
    $pp_clean_learn_info,
    $pp_learn_reset_hparams(Mode),
    $pp_trans_goals(Goals,GoalCountPairs,AllGoals),!,
    global_set($pg_observed_facts,GoalCountPairs),
    cputime(StartExpl),
    global_set($pg_num_goals,0),
    $pp_find_explanations(AllGoals),!,
    $pp_print_num_goals(MsgS),
    cputime(EndExpl),
    statistics(table,[TableSpace,_]),
    $pp_format_if(MsgM,"Exporting switch information to the EM routine ... "),
    flush_output,
    $pp_export_sw_info,
    $pp_format_if(MsgM,"done~n"),
    format("~w\n",[GoalCountPairs,GidCountPairs,0,Len,0,NGoals,-1,FailRootIndex]),
    $pp_observed_facts(GoalCountPairs,GidCountPairs,
                       0,Len,0,NGoals,-1,FailRootIndex),
    format("a"),
    $pc_prism_prepare(GidCountPairs,Len,NGoals,FailRootIndex),
    format("b"),
    cputime(StartEM),
    %%%$pp_em(Mode,Output),
    $pp_cyc_em(Mode,Output),
    %%%
    cputime(EndEM),
    $pc_import_occ_switches(NewSws,NSwitches,NSwVals),
    $pp_decode_update_switches(Mode,NewSws),
    $pc_import_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    cputime(End),
    $pp_assert_graph_stats(NSubgraphs,NGoalNodes,NSwNodes,AvgShared),
    $pp_assert_learn_stats(Mode,Output,NSwitches,NSwVals,TableSpace,
                           Start,End,StartExpl,EndExpl,StartEM,EndEM,1000),
    $pp_print_learn_stats_message(MsgT),
    $pp_print_learn_end_message(MsgM,Mode),!.

$pp_cyc_em(ml,Output) :-
    format("c"),
    $pc_cyc_em(Iterate,LogPost,LogLike,BIC,CS,ModeSmooth),
    format("d"),
    Output = [Iterate,LogPost,LogLike,BIC,CS,ModeSmooth].


nonlin_prob_check(Goal) :-
  nonlin_prob_m(1,Goal,P),
  format("Probability is ~w\n",P).
nonlin_prob_check(Goal,P) :-
  nonlin_prob_m(1,Goal,P).

nonlin_prob(Goal) :-
  nonlin_prob_m(0,Goal,P),
  format("Probability is ~w\n",P).
nonlin_prob(Goal,P) :-
  nonlin_prob_m(0,Goal,P).

nonlin_prob_m(Mode,Goal) :-
  nonlin_prob_m(Mode,Goal,P),
  format("Probability is ~w\n",P).
nonlin_prob_m(Mode,Goal,P) :-
  probefi(Goal,_),
  $pc_nonlinear_eq(Mode,P).



% Transform the prism explanation graph into a hypergraph
$pp_trans_graph(ExpGraph,HGraph,NodesId,ExpTableId) :- !,
  length(ExpGraph,L),
  new_bigarray(NodesAcc,L),
  new_hashtable(NodesId,L),
  new_bigarray(ExpTableId,L),
  $pp_create_node_array(ExpGraph,1,NodesId,ExpTableId),
  $pp_transform_graph(ExpGraph,HGraph,NodesAcc,NodesId).

$pp_create_node_array([],_,_,_).
$pp_create_node_array([node(H,_,_)|ExpGraph],I,NodesId,ExpTableId) :-
  hashtable_register(NodesId,H,I),
  bigarray_put(ExpTableId,I,H),
  I1 is I + 1,
  $pp_create_node_array(ExpGraph,I1,NodesId,ExpTableId).

$pp_transform_graph([],HGraph,HGraph,_).
% Traverse node H <=> B_1 + ... + B_n
% where P(H) = P(B_1) + ... + P(B_n)
$pp_transform_graph([node(H,Paths,_)|ExpGraph],HGraph,HGraphAcc,NodesId) :-
% Traverse edges B_i = C_1 ^ ... ^ C_m ^ msw_1 ^ ... ^ msw_n
% where P(B_i) = P(C_1)*...*P(C_m)*P(msw_1)*...*P(msw_n)
  $pp_transform_edges(NodesId,Paths,Edges),
  hashtable_get(NodesId,H,I),
  bigarray_put(HGraphAcc,I,Edges),
  $pp_transform_graph(ExpGraph,HGraph,HGraphAcc,NodesId).

% Transform the paths into multiedges
$pp_transform_edges(NodesId,Paths,Edges) :-
  $pp_transform_edges(NodesId,Paths,Edges,[]).
$pp_transform_edges(_,[],Edges,Edges).
$pp_transform_edges(NodesId,[path(Nodes,Switches,_)|Paths],Edges,EdgesAcc) :-
  % Get children of edge
  Children @= [C : N in Nodes,[C,P,G],(N = gnode(G,P),hashtable_get(NodesId,G,C))],
  % Accumulate probability from switches
  % AccProb = P(msw_1)*...*P(msw_n)
  ($pp_in_log_scale ->
    foreach(snode(_,Prob) in Switches,ac(AccProb,0),(AccProb^1 is Prob + AccProb^0))
    ;foreach(snode(_,Prob) in Switches,ac(AccProb,1),AccProb^1 is Prob * AccProb^0)
  ),
  $pp_transform_edges(NodesId,Paths,Edges,[edge(AccProb,Children)|EdgesAcc]).

% Utility functions for manipulating the stack
$pp_scc_pop_until_less([Id|P],CTarget,PreTable,[Id|P]) :-
  bigarray_get(PreTable,Id,C),
  C =< CTarget.
$pp_scc_pop_until_less([_|P],CTarget,PreTable,P1) :-
  $pp_scc_pop_until_less(P,CTarget,PreTable,P1).

$pp_scc_pop_until_found(S,NTarget,S1,Popped) :-
  $pp_scc_pop_until_found(S,NTarget,S1,Popped,[]).
$pp_scc_pop_until_found([N|S],N,S,[N|Popped],Popped).
$pp_scc_pop_until_found([N|S],NTarget,S1,Popped,PoppedAcc) :-
  $pp_scc_pop_until_found(S,NTarget,S1,Popped,[N|PoppedAcc]).

% Wrapper for finding strongly connected components
$pp_find_scc(G,Components,CompTable) :-
  % Initialize arrays
  bigarray_length(G,Size),
  new_bigarray(PreTable,Size),
  new_bigarray(CompTable,Size),
  % Call Gabow algorithm on each node
  foreach(I in 1..Size,
    [ac(S,[]),ac(P,[]),ac(C,1),ac(D,1),ac(Comp,[])],
    $pp_find_scc(G,I,S^0,P^0,C^0,D^0,Comp^0,S^1,P^1,C^1,D^1,Comp^1,PreTable,CompTable)
  ),
  % Have components in topological ordering
  reverse(Comp,Components).

% Find strongly connected components using Gabow's algorithm 
% G: Graph (array of outgoing edges for each node id)
% N: Current node id
% S: Stack of nodes that have not yet been assigned to a scc
% P: Stack of nodes which have not yet been determined to belong to different scc
% C: Counter for preorder number
% D: Counter for component number
% Comp: List of components found so far
% PreTable: Table containing the preorder number for each node
% CompTable: Table containing the component number for each node and the
%   index number within that component
$pp_find_scc(G,N,S0,P0,C0,D0,Comp0,S3,P3,C3,D3,Comp3,PreTable,CompTable) :-
  ((bigarray_get(PreTable,N,C),nonvar(C)) ->
    % Node already has a preorder number
    ((bigarray_get(CompTable,N,Comp),nonvar(Comp)) ->
      % Node has been assigned to a component, do nothing
      P3 = P0
    ; % Node has not been assigned to a component
      % pop from P to collapse nodes until top element 
      % has same or lower preorder number
      $pp_scc_pop_until_less(P0,C,PreTable,P3)
    ),
    S3 = S0, C3 is C0, D3 is D0, Comp3 = Comp0
  ; % Node has no preorder number
    % Assigning new preorder number, pushing N on both stacks
    bigarray_put(PreTable,N,C0),
    S1 = [N|S0],
    P1 = [N|P0],
    C1 is C0 + 1,
    % Traverse all children
    bigarray_get(G,N,Edges),
    foreach(edge(_,Children) in Edges, Child in Children,
      [ac(C2,C1),ac(D2,D0),ac(S2,S1),ac(P2,P1),ac(Comp2,Comp0)],
        $pp_find_scc(G,Child,S2^0,P2^0,C2^0,D2^0,Comp2^0,S2^1,P2^1,C2^1,D2^1,Comp2^1,PreTable,CompTable)
    ),
    ( P2 = [N|P3] ->
      % N is top element of P, make new component
      $pp_scc_pop_until_found(S2,N,S3,NewComp),
      foreach(NodeId in NewComp, [ac(SubId,1)],
        ( bigarray_put(CompTable,NodeId,(D2,SubId^0)), SubId^1 is SubId^0 + 1 )
      ),
      Comp3 = [NewComp|Comp2],
      D3 is D2 + 1
    ; % N is part of another component, simply continue
      P3 = P2, S3 = S2, D3 is D2, Comp3 = Comp2
    ), C3 = C2
  ).

% log scale mode use scaling for matrix calculous
$pp_scaling_param(Scale):-Scale is 10000000.
% solve system given by graph G, dividided into components
% by creating linear equation systems and solving bottom up
$pp_solve_graph(G,Components,CompTable,ProbTable) :-
  bigarray_length(G,L),
  new_bigarray(ProbTable,L),
  % Solve each component
  foreach(Comp in Components,
    $pp_solve_component(G,Comp,CompTable,ProbTable)).

% Solve component by creating a linear system Ax = b
$pp_solve_component(G,CompNodes,CompTable,ProbTable) :-
  length(CompNodes,Length),
  SizeA is Length * Length,
  % Create matrix A and vector b
  new_bigarray(A,SizeA),
  new_bigarray(B,Length),
  % Fill diagonal of A with 1 and rest with 0
  foreach(I in 1..Length,
    ( foreach(J in 1..Length, [IJ],
      ( IJ is (I - 1) * Length + J,
        ( I == J -> bigarray_put(A,IJ,-1)
        ; bigarray_put(A,IJ,0)
        )
      )),
      bigarray_put(B,I,0)
    )
  ),
  % Iterate over nodes to fill A and b
  foreach(Node in CompNodes,
    $pp_update_linear_system(G,Node,CompTable,ProbTable,A,B,Length)
  ),
  % Solve linear system to x
  ( Length == 1 -> % single value, solve directly
    new_bigarray(X,1),
    bigarray_get(A,1,AVal),
    bigarray_get(B,1,BVal),
    ( AVal =:= 0.0 -> % singular system, not solvable
      $pp_raise_evaluation_error($msg(0200),['system not solvable'],non_solvable,$pp_solve_component/4)
    ; ($pp_in_log_scale -> ($pp_scaling_param(Scale),Prob is -1* BVal / AVal);Prob is -1*BVal/AVal),
      bigarray_put(X,1,Prob)
    )
  ; % Transform to list and call c interface for solving
    bigarray_to_list(A,AList),
    bigarray_to_list(B,BList),
    ( $pc_solve_linear_system(Length, AList, BList, XList,0) ->
      list_to_bigarray(XList, X)
    ; $pp_raise_evaluation_error($msg(0200),['system not solvable'],non_solvable,$pp_solve_component/4)
    )
  ),
  % Write probabilites
  foreach(Node in CompNodes, [CompSubId,NodeProb,CompId,NP,Scale],
    ( bigarray_get(CompTable,Node,(CompId,CompSubId)),
      bigarray_get(X,CompSubId,NodeProb),
      ($pp_in_log_scale->($pp_scaling_param(Scale),NP is log(NodeProb/Scale));NP is NodeProb),
      bigarray_put(ProbTable,Node,NP) )
  ).

% Update linear system for a certain node by traversing all edges
$pp_update_linear_system(G,Node,CompTable,ProbTable,A,B,Length) :-
  bigarray_get(G,Node,Edges),
  bigarray_get(CompTable,Node,(CompId,CompSubId)),
  foreach(edge(EdgeProb,Children) in Edges,
    [ProdProb,Dependants,DependantId,SumProb,TmpProb,Index,Scale],
    ( foreach(Child in Children, [ac(ProdProb,EdgeProb),ac(Dependants,[])],
        [ChildCompId,ChildProb,ChildCompSubId],
        ( bigarray_get(CompTable,Child,(ChildCompId,ChildCompSubId)),
          ( ChildCompId == CompId ->
            ProdProb^1 is ProdProb^0,
            Dependants^1 = [ChildCompSubId|Dependants^0]
          ; ( ChildCompId < CompId ->
              % Child is in lower component, then prob already known
              bigarray_get(ProbTable,Child,ChildProb),
              ($pp_in_log_scale->
                ProdProb^1 is ProdProb^0 + ChildProb
                ;ProdProb^1 is ProdProb^0 * ChildProb),
              Dependants^1 = Dependants^0
            ; % Edge to higher component, should not happen
              $pp_raise_internal_error($msg(9802), invalid_component_dependence,$pp_update_lin_system/7)
            )
          )
        )
      ), % Check for number of dependants
      ( Dependants = [] -> % Put into B
        bigarray_get(B,CompSubId,TmpProb),
        ($pp_in_log_scale->
                ($pp_scaling_param(Scale),SumProb is TmpProb + exp(ProdProb)*Scale)
          ;SumProb is TmpProb + ProdProb),
        bigarray_put(B,CompSubId,SumProb)
      ; ( Dependants = [DependantId] -> % Put into A
          Index is (CompSubId - 1) * Length + DependantId,
          bigarray_get(A,Index,TmpProb),
          ($pp_in_log_scale->
                  ($pp_scaling_param(Scale),SumProb is (TmpProb + exp(ProdProb)))
            ;(SumProb is TmpProb + ProdProb)),
          bigarray_put(A,Index,SumProb)
        ; % Non-linear relation, can not solve it
          $pp_raise_evaluation_error($msg(1402),['non-linear dependence'],non_linear_dependence,$pp_update_lin_system/7)
        )
      )
    )
  ).


