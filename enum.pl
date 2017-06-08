:- set_prolog_flag(verbose, silent).
:- initialization(main).
:- [display].

main :-
 	current_prolog_flag(argv, Argv),
    	Argv =[X],
	format('Computing the traces with all possible prefixes for partial traces :~n'),
    	format('Output in file ~q~n', X),
    	enumeration(CpltTr,GluedTr), 
    	print_full_result(X,CpltTr,GluedTr),
    	halt.
main :-
	format('enum.pl computes the traces with all possible prefixes for partial traces~n'),
	format("Usage : swipl enum.pl <path_to_output_file>~n"),
    	halt(1).

