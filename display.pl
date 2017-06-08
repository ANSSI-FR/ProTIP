:- [traces].

%%%%%%%%%%%%%% Pretty-printing results  %%%%%%%%%%%%%%%%
displayReason(Stream, Reason) :- Reason = write
                                -> write(Stream, "write");
	                         Reason = compl
                                -> write(Stream, "completion");
        	                 write(Stream, "unknown Reason").
  
displayList(Stream, []):- nl(Stream).
displayList(Stream, [X|L]):-
		write(Stream, X), nl(Stream),
		displayList(Stream, L).

displayHopList(Stream,[],_):- write(Stream,"Empty trace"), nl(Stream).
displayHopList(Stream,[(R,X,Y,Z)],T):-
	 reverse(Z,RZ),
	 write_term(Stream, (X,Y,RZ),[maxlength(0), variable_names([ 'Address'=T ])]), nl(Stream), 
	 (R = end -> write(Stream, " done.");
	 (write(Stream," -> terminating abnormally"), 
	write(Stream, R))),
	nl(Stream).
displayHopList(Stream,[(R,X,Y,Z)],T):-
	 reverse(Z,RZ),
	 write_term(Stream, (R,X,Y,RZ),[maxlength(0), variable_names([ 'Address'=T ])]), nl(Stream).
displayHopList(Stream,[(R,X,Y,Z)|L],T):-
	reverse(Z,RZ),
        write_term(Stream,(X,Y,RZ),[maxlength(0), variable_names([ 'Address'=T ])]),
        nl(Stream),
        write(Stream," -> using "), 
	write(Stream, R),
        write(Stream," -> "), 
        displayHopList(Stream,L,T).

displayCstrList(Stream,[],_):- write(Stream,"No constraints"), nl(Stream).
displayCstrList(Stream,[X],T):-
        write_term(Stream,X,[maxlength(0), variable_names([ 'Address'=T ])]), nl(Stream).
displayCstrList(Stream,[X|L],T):-
        write_term(Stream,X,[maxlength(0), variable_names([ 'Address'=T ])]),
        write(Stream,", "),
        displayCstrList(Stream,L,T).

displayTraceClass(Stream,TraceTuple) :- classify(TraceTuple, Class),
        (Class = usualread -> write(Stream,"usual read trace");
        Class = usualwrite -> write(Stream,"usual write trace");
        write(Stream,"WEIRD TRACE TO BE EXAMINED")).

displayTrace(Stream,(Reason,Address,Port,List,Cstr)) :-
        swritef(Header, "Trace : port %d accepts ", [Port]),
        write_term(Stream,Header,[]),
        displayReason(Stream,Reason),
        write(Stream," with a "),
        displayTraceClass(Stream,(Reason,Address,Port,List,Cstr)),
        write(Stream," :"), nl(Stream),
        displayHopList(Stream,List,Address),
        write(Stream,"and constraints : "), nl(Stream),
        displayCstrList(Stream,Cstr,Address), nl(Stream).

displayTraceList(Stream,[]) :- nl(Stream).
displayTraceList(Stream,[Trace|Tail]) :- displayTrace(Stream,Trace), displayTraceList(Stream,Tail).

displayTracePrefix(Stream, (List,Cstr)) :-
	write(Stream,"Prefix :"), nl(Stream), 
	displayHopList(Stream,List,_), 
	write(Stream,"and constraints : "), nl(Stream),
        displayCstrList(Stream,Cstr,_), nl(Stream).

displayPrefixList(Stream,[]) :- nl(Stream).
displayPrefixList(Stream,[Prefix|Tail]) :- displayTracePrefix(Stream,Prefix), displayPrefixList(Stream,Tail).

displayTraceAndPrefix(Stream,(TraceTuple,PrefList)) :- 
		displayTrace(Stream,TraceTuple), 
		displayPrefixList(Stream,PrefList).

displayTrPrList(Stream, []) :- nl(Stream).
displayTrPrList(Stream, [(TraceTuple,PrefList)|Tail]) :- displayTraceAndPrefix(Stream, (TraceTuple,PrefList)), displayTrPrList(Stream, Tail).

print_one_trace(File,(List,Cstr)) :- open(File, append, Stream, [create([all])]),
                        displayHopList(Stream,List,Address),
                        write(Stream,"and constraints : "), nl(Stream),
                        displayCstrList(Stream,Cstr,Address), nl(Stream),
                        close(Stream).

% To print a list of complete traces (Bag) in a file.
print_result(File, Bag) :-      open(File, append, Stream, [create([all])]),
                        write(Stream, "State configuration"), nl(Stream),
                        current_state([],State, _), write_term(Stream, State,[]), nl(Stream),
                        nl(Stream), nl(Stream), nl(Stream),
                        is_iommu_config_read(IR),
                        is_iommu_config_write(IW),
                        write(Stream, "IOMMU Read configuration"), nl(Stream),
                        write_term(Stream, IR,[]), nl(Stream),
                        write(Stream, "IOMMU Write configuration"), nl(Stream),
                        write_term(Stream, IW,[]), nl(Stream),
                        nl(Stream), nl(Stream), nl(Stream),
                        displayTraceList(Stream,Bag),
                        nl(Stream),
                        close(Stream).

% To print in File a list of complete traces (CBag) and a list of pairs of (complete trace, list of possible prefixes) (GBag).
print_full_result(File, CBag, GBag) :-      open(File, append, Stream, [create([all]), encoding(utf8)]),
                        write(Stream, "State configuration"), nl(Stream),
                        current_state([],State, _), %write_term(Stream, State,[]), nl(Stream),
                        displayList(Stream,State),
			nl(Stream), nl(Stream), nl(Stream),
                        is_iommu_config_read(IR),
                        is_iommu_config_write(IW),
                        write(Stream, "IOMMU Read configuration"), nl(Stream),
                        write_term(Stream, IR,[]), nl(Stream),
                        write(Stream, "IOMMU Write configuration"), nl(Stream),
                        write_term(Stream, IW,[]), nl(Stream),
                        nl(Stream), nl(Stream), nl(Stream),
                        displayTraceList(Stream,CBag),
			displayTrPrList(Stream,GBag),
                        nl(Stream),
                        close(Stream).



% To print in File a list Bag of prefixes.
print_trace_set(File, Bag) :-      open(File, append, Stream, [create([all])]),
                        write(Stream, "State configuration"), nl(Stream),
                        current_state([],State, _), write_term(Stream, State,[]), nl(Stream),
                        nl(Stream), nl(Stream), nl(Stream),
                        is_iommu_config_read(IR),
                        is_iommu_config_write(IW),
                        write(Stream, "IOMMU Read configuration"), nl(Stream),
                        write_term(Stream, IR,[]), nl(Stream),
                        write(Stream, "IOMMU Write configuration"), nl(Stream),
                        write_term(Stream, IW,[]), nl(Stream),
                        nl(Stream), nl(Stream), nl(Stream),
                        displayPrefixList(Stream,Bag),
                        nl(Stream),
                        close(Stream).

