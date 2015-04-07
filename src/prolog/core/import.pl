/*
  A simple python-like environment based search path import mechanism
  for PRISM.

  sys_path(PATHS): PATHS contains a list of paths where PRISM will look
  for files loaded with :- import(FILE).

  sys_path_append(PATH): prepends PATH to system paths.

  import(File): attend to compile and load File in current
  directory. If File is absent, looks at PRISM_PATH environment
  variable and searches in each directory for File, compiling and
  loading the first existing file it finds.

*/

sys_path(Paths) :-
        $pp_sys_path(Paths).

sys_path_append(Path) :-
        $pp_path_append(Path).

%--------------------------------------------------

:- dynamic $pp_sys_path/1.
$pp_sys_path(Ps) :-
        expand_environment('$PRISM_PATH', Path),
        write(Path), nl,
        $pp_split_path(Path, Ps).

$pp_split_path(A, As) :-
        atomic_split_at(A, ';', As).

$pp_path_append(Path) :-
        $pp_sys_path(Ps),
        Ps1=[Path|Ps],
        retractall($pp_sys_path(_)),
        assert($pp_sys_path(Ps1)).

get_first_word(Input, Pivot, Word, Rest):-
  append(Word, [Pivot|Rest], Input), !.
get_first_word(Input, _Pivot, Input, []).

split_at([], _, []).
split_at(Input, Pivot, [W|Out]):-
  get_first_word(Input, Pivot, W, Rest),
  split_at(Rest, Pivot, Out).

atomic_split_at(In, Sep, Out) :-
        atom_codes(Sep, [Sep1]),
        atom_codes(In, StringIn),
        split_at(StringIn, Sep1, StringOut),
        atoms_codes(Out, StringOut).
atoms_codes([], []) :- !.
atoms_codes([A|As], [B|Bs]) :-
        atom_codes(A, B),
        atoms_codes(As, Bs).
        

        
%----------------------------------------------------

import(File) :-
        $pp_import(File).

$pp_import(File) :-
        catch(
              cl(File),
              error(file_not_found, _),
              $pp_import_on_path(File)).

$pp_import_on_path(File) :-
        sys_path(Ps),
        $pp_import_on_path(File, Ps).

$pp_import_on_path(File, []) :- 
        throw(file_not_found_on_path, File),
        !.
$pp_import_on_path(File, [P|Ps]) :-
        atom_concats([P, '/', File], FullPath),
        catch(
              cl(FullPath),
              error(file_not_found, _),
              $pp_import_on_path(File, Ps)
             ).
 

