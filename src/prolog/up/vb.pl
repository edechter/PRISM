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

    
    
