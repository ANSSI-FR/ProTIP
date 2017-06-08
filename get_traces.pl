:- set_prolog_flag(verbose, silent).
:- initialization(main).
:- [display].

main :-
 	current_prolog_flag(argv, Argv),
    	Argv =[X],
	format('Computing the traces with one possible prefix for partial traces :~n'),
    	format('Output in file ~q~n', X),
    	the_traces(CpltTr,GluedTr), 
    	print_full_result(X,CpltTr,GluedTr),
    	halt.
main :-
	format('get_traces.pl computes the traces with one possible prefix for partial traces~n'),
	format("Usage : swipl get_traces.pl <path_to_output_file>~n"),
    	halt(1).

