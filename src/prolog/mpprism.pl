:- $pp_require_mp_mode.
:- $pc_mp_master -> print_copyright ; true.

%%------------------------------------------------------------------------
%%  [[ Tags for $pc_mp_sync/2 ]]
%%------------------------------------------------------------------------
%%  01 : $pp_batch_call
%%  02 : $pp_mp_call_s_core
%%  03 : $pp_compile_load
%%  04 : $pp_foc
%%------------------------------------------------------------------------

%%----------------------------------------
%%  batch routines

main :- $pp_batch.

%$pp_batch_call(Goal) :-
%    $pc_mp_master -> $pp_mpm_batch_call(Goal) ; $pp_mps_batch_call.

$pp_batch_call(Goal) :-
    ( $pc_mp_master -> $pp_mpm_batch_call(Goal)
    ; $pp_mps_batch_call
    ).

$pp_mpm_batch_call(Goal) :-
    ( call(Goal) -> Sync = 1 ; Sync = -1 ),
    $pc_mpm_bcast_command($stop),!,
    ( $pc_mp_sync(1,Sync) -> Res = yes ; Res = no ),
    format("~n~w~n",[Res]).

$pp_mps_batch_call :-
    ( $pp_slave_loop -> Sync = 1 ; Sync = -1 ),!,
    ( $pc_mp_sync(1,Sync) ; true ).

$pp_slave_loop :-
    $pc_mps_bcast_command(Cmd),
    ( Cmd \== $stop -> call(Cmd),!,$pp_slave_loop
    ; true
    ).

%%----------------------------------------
%%  system predicates

abort :- $pc_mp_abort.

$pp_mps_err_msg(Msg) :-
    $pc_mps_revert_stdout, $pp_err_msg(Msg).
$pp_mps_err_msg(Fmt,Args) :-
    $pc_mps_revert_stdout, $pp_err_msg(Fmt,Args).

$pp_load(File) :-
    $pp_mp_call_s_core(\+ \+ $myload(File)),
    $pp_init_tables.

$pp_compile_load(File) :-
    $pp_add_out_extension(File,OutFile),
    ( $pc_mp_master -> $pp_compile(File,_DmpFile,OutFile) ; true ),!,
    $pc_mp_sync(3,1),
    $pp_load(OutFile).
$pp_compile_load(_File) :-
    $pc_mp_sync(3,-1).

$pp_foc(File1,File2) :-
    ( $pc_mp_master ->
        fo(File1,File2), format("Compilation done by FOC~n~n",[])
    ; true
    ),!,
    $pc_mp_sync(4,1).
$pp_foc(_,_) :-
    $pc_mp_sync(4,-1).

%%----------------------------------------
%%  user predicates

mp_call(Goal) :-
    $pc_mpm_bcast_command(Goal),call(Goal).
mp_call_s(Goal) :-
    $pc_mpm_bcast_command($pp_mp_call_s_core(Goal)),$pp_mp_call_s_core(Goal).
    
$pp_mp_call_s_core(Goal) :-
    $pc_mp_rank(R),
    $pc_mp_size(N),
    $pp_mp_call_s_core(Goal,R,N,0).

$pp_mp_call_s_core(_,_,N,K) :-
    K >= N,!.
$pp_mp_call_s_core(Goal,MyID,N,K) :-
    ( K =:= MyID ->
        ( call(Goal) -> Sync = K ; Sync = -1 )
    ; % else
        Sync = K
    ),
    $pc_mp_sync(2,Sync),
    K1 is K + 1,!,
    $pp_mp_call_s_core(Goal,MyID,N,K1).

%%----------------------------------------
%%  debug predicates

$pp_mp_debug(Format,Args) :-
    current_output(Stream),
    $pp_mp_debug(Stream,Format,Args).

$pp_mp_debug(Stream,Format,Args) :-
    $pc_mp_rank(R),
    append("[RANK:~w] ",Format,NewFormat),
    NewArgs = [R|Args],
    format(Stream,NewFormat,NewArgs),!.

%%----------------------------------------

:- $pp_require_mp_mode.

%%----------------------------------------

$pp_learn_core(Mode) :-
    ( $pc_mp_master -> $pp_mpm_learn_main(Mode) ; true ).
$pp_learn_core(Mode,Goals) :-
    ( $pc_mp_master -> $pp_mpm_learn_main(Mode,Goals) ; true ).

$pp_vlearn_core(_Mode) :-
    $pp_raise_runtime_error($msg(3500),mp_version_unavailable,$pp_vlearn_core/1).
$pp_vlearn_core(_Mode,_Goals) :-
    $pp_raise_runtime_error($msg(3500),mp_version_unavailable,$pp_vlearn_core/2).

$pp_mpm_learn_main(Mode) :-
    $pp_learn_data_file(FileName),
    load_clauses(FileName,Goals,[]),
    $pc_mpm_bcast_command($pp_mps_learn_core(Mode)),!,
    $pp_mpm_learn_core(Mode,Goals).

$pp_mpm_learn_main(Mode,Goals) :-
    $pp_learn_check_goals(Goals),
    $pc_mpm_bcast_command($pp_mps_learn_core(Mode)),!,
    $pp_mpm_learn_core(Mode,Goals).

%%----------------------------------------

% Master
$pp_mpm_learn_core(Mode,Goals) :-
    $pc_mp_sync(2,1),
    $pc_mp_wtime(Start),
    $pp_clean_learn_info,
    $pp_learn_reset_hparams(Mode),
    $pp_build_count_pairs(Goals,GoalEqCountPairs),
    $pp_learn_message(MsgS,MsgE,MsgT,MsgM),
    $pc_set_em_message(MsgE),
    $pc_mp_wtime(StartExpl),
    global_set($pg_num_goals,0),
    $pc_mpm_share_prism_flags,
    $pp_mpm_find_explanations(GoalEqCountPairs,GoalCountPairs),!,
    global_set($pg_observed_facts,GoalCountPairs),
    $pp_print_num_goals(MsgS),
    $pc_mp_wtime(EndExpl),
    TableSpace = 'N/A',
    $pp_format_if(MsgM,"Gathering and exporting switch information ...~n"),
    $pc_mp_recv_switches,
    $pp_mpm_export_switches,
    $pc_mpm_alloc_occ_switches,
    $pc_mp_send_swlayout,
    $pp_export_sw_info,
    $pc_mp_wtime(StartEM),
    $pp_mpm_em(Mode,Output),
    $pc_mp_wtime(EndEM),
    $pc_import_occ_switches(NewSws,NumSwitches,NumSwVals),
    $pp_decode_update_switches(Mode,NewSws),
    $pc_mpm_import_graph_stats(NumSubgraphs,NumGoalNodes,NumSwNodes,AvgShared),
    $pc_mp_wtime(End),
    $pp_assert_graph_stats(NumSubgraphs,NumGoalNodes,NumSwNodes,AvgShared),
    $pp_assert_learn_stats(Mode,Output,NumSwitches,NumSwVals,TableSpace,
                           Start,End,StartExpl,EndExpl,StartEM,EndEM,1),
    $pp_print_learn_stats_message(MsgT),
    $pp_print_learn_end_message(MsgM,Mode),!.

% Slave
$pp_mps_learn_core(Mode) :-
    $pc_mp_sync(2,1),
    $pp_clean_learn_info,
    $pc_mps_share_prism_flags,
    $pp_mps_find_explanations(GoalCountPairs),
    global_set($pg_observed_facts,GoalCountPairs),
    $pp_collect_sw_info(_Sws),
    $pp_observed_facts(GoalCountPairs,GoalIdCountPairs,0,Len,0,NumOfGoals,-1,FailRootIndex),
    $pc_prism_prepare(GoalIdCountPairs,Len,NumOfGoals,FailRootIndex),
    $pc_mp_send_switches,
    $pc_mp_recv_swlayout,
    $pp_mps_em(Mode),
    $pc_mps_import_graph_stats,!.

%%----------------------------------------

$pp_mpm_em(params,Output) :-
    $pc_mpm_prism_em(Iterate,LogPost,LogLike,BIC,CS,ModeSmooth),
    Output = [Iterate,LogPost,LogLike,BIC,CS,ModeSmooth].
$pp_mpm_em(hparams,Output) :-
    $pc_mpm_prism_vbem(IterateVB,FreeEnergy),
    Output = [IterateVB,FreeEnergy].
$pp_mpm_em(both,Output) :-
    $pc_mpm_prism_both_em(IterateVB,FreeEnergy),
    Output = [IterateVB,FreeEnergy].

$pp_mps_em(params) :-
    $pc_mps_prism_em.
$pp_mps_em(hparams) :-
    $pc_mps_prism_vbem.
$pp_mps_em(both) :-
    $pc_mps_prism_both_em.

%%----------------------------------------

$pp_mpm_find_explanations(GoalEqCountPairs,GoalCountPairs) :-
    $pp_learn_message(MsgS,_,_,_),
    $pp_mpm_expl_goals(MsgS,GoalEqCountPairs,GoalCountPairs),
    $pc_mp_size(N),
    $pp_mpm_expl_complete(N).

$pp_mpm_expl_goals(_,[],[]).
$pp_mpm_expl_goals(MsgS,
                   [Goal=Count|GoalEqCountPairs],
                   [goal(Goal,Count)|GoalCountPairs]) :-
    $pc_mp_send_goal(Goal=Count),
    $pp_print_goal_message(MsgS),!,
    $pp_mpm_expl_goals(MsgS,GoalEqCountPairs,GoalCountPairs).

$pp_mpm_expl_complete(N) :-
    N =< 1,!.
$pp_mpm_expl_complete(N) :-
    $pc_mp_send_goal($done),
    N1 is N - 1,!,
    $pp_mpm_expl_complete(N1).

%%----------------------------------------

$pp_mps_find_explanations(GoalCountPairs) :-
    $pp_mps_expl_goals([],GoalCountPairs).

$pp_mps_expl_goals(GoalCountPairs0,GoalCountPairs) :-
    once($pc_mp_recv_goal(GoalEqCountPair)),
    GoalEqCountPair \== $done,!,
    GoalEqCountPair = (Goal=Count),
    $pp_build_dummy_goal(Goal,DummyGoal),
    ( $pp_expl_one_goal(DummyGoal) -> true
    ; $pp_mps_err_msg("Failed to find solutions for ~w.",[Goal])
    ),
    GoalCountPairs1 = [goal(DummyGoal,Count)|GoalCountPairs0],
    $pc_sleep(1), % enable this for the stability in small-scale learning
    !,
    $pp_mps_expl_goals(GoalCountPairs1,GoalCountPairs).
$pp_mps_expl_goals(GoalCountPairs,GoalCountPairs).

%%----------------------------------------

$pp_mpm_export_switches :-
    $pc_prism_sw_count(N),
    $pp_mpm_export_switches(0,N).

$pp_mpm_export_switches(Sid,N) :-
    Sid >= N,!.
$pp_mpm_export_switches(Sid,N) :-
    $pc_prism_sw_term(Sid,Sw),
    $pp_get_values(Sw,Values),
    $pp_export_switch(Sid,Sw,Values),
    Sid1 is Sid + 1,!,
    $pp_mpm_export_switches(Sid1,N).
