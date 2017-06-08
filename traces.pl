:- [pcie].

%% Current state used for trace computation.
% The empty list here is the empty list of Zs (list of variables to work on configurations
% which are not fully determined). This is soon-to-be pushed work. 
current_state([],State, NbPort) :- my_state(MyState), state(State, NbPort, MyState, []).

%%%%%%%%%%%%%%%%% Trace computation 
%% Traces are computed using backtracking. 
% 

%% Models a hop of port Port on reception of Pck_In in state State
%% and with set of taglists TagList, 
%% which results in TagList being updated to NTagList, and 
%% the sending of Pck_Out to Next_Port.
is_valid_hop(Rule, Zs, Pck_In, Port, TagList, Pck_Out, Next_Port, NTagList, State) :- write_hops(Rule, Zs,Pck_In, Port, TagList, Pck_Out, Next_Port, NTagList, State).
is_valid_hop(Rule, Zs, Pck_In, Port, TagList, Pck_Out, Next_Port, NTagList, State) :- read_hops(Rule, Zs,Pck_In, Port, TagList, Pck_Out, Next_Port, NTagList, State).
is_valid_hop(Rule, Zs, Pck_In, Port, TagList, Pck_Out, Next_Port, NTagList, State) :- completion_hops(Rule, Zs,Pck_In, Port, TagList, Pck_Out, Next_Port, NTagList, State).

% For validation and debugging purposes. 
hoptest(Rule, Zs, Pck_In, Port, TagList, Pck_Out, Next_Port, NTagList) :- current_state(Zs,State,_), is_valid_hop(Rule, Zs, Pck_In, Port, TagList, Pck_Out, Next_Port, NTagList, State).

% trace_to is the backtracking predicate.
% It is terminal recursive : 3rd argument is an accumulator which contains partial results 
% while the 4th argument is the actual result from the computation.
trace_to(_, (generate,_, _,_), Acc, Acc).
trace_to(_, (gen_read(_),_, _,_), Acc, Acc).
trace_to(_, (gen_write(_),_, _,_), Acc, Acc).
trace_to(Zs, (Pck_Out, NPort, NTagList, State), Acc, Res) :-
                        is_valid_hop(Rule, Zs, Pck_In, Port, TagList, Pck_Out, NPort, NTagList, State),
                        trace_to(Zs, (Pck_In,Port, TagList, State), [ (Rule, Pck_In, Port, TagList) | Acc], Res).

% trace_to_test((Pck_In,Port,TagList), (List, Cstr)) holds for every trace List finishing with 
% (Pck_In,Port,TagList), with constraints Cstr. 
trace_to_test((Pck_In,Port,TagList), (List,Cstr)) :- 
			current_state(Zs,State,_), 
			trace_to(Zs, (Pck_In,Port,TagList,State), [(end, Pck_In,Port,TagList)], List), 
			copy_term(append(List,Zs), append(List,Zs), Cstr).

% The traces we are interested in are all acceptance-terminated traces ending with the set of empty
% tag lists. Indeed, 1 - acceptance captures side effects on a component,
%		     2 - acceptance only removes pairs from tag lists, so all traces ending 
%			 with a non-empty tag list have an equivalent ending with a empty tag list.
% 
% Enumeration of all the acceptance-terminated traces : accepted_traces(Bag) holds for Bag 
% being the lists of all these trace tuples (Reason,Address,Port,List,Cstr), 
% where Reason and Address are acceptance reason and address, Port is the termination port, 
% and List and Cstr are the trace and associated constraints.
%
% init_tag_list(TagList) is an auxiliary function holding for an list of empty tag lists
% for each port.
gen_accept_trace(Reason, Address, Port, List, Cstr) :-	init_tag_list(TagList), 
							trace_to_test((accept(Reason,Address),Port, TagList),(List,Cstr)).
accepted_traces(Bag) :- findall((Reason,Address,Port,List,Cstr), gen_accept_trace(Reason, Address, Port, List, Cstr), Bag).

% Traces in the Bag of accepted_traces start with an initial set of tag lists
% which can be all empty, or there exists one tag list which comprises a pair.
% There is no other possibility: only accepting events add elements in 
% tag lists and they never appear as pre-conditions of rules.
% When one tag list in non-empty, the traces are not complete. 
% We need to exhibit a prefix to realize the
% set of tag lists.
%
aux_sort_traces([], URes, CRes, TRes, URes, CRes, TRes).
aux_sort_traces([(Reason,Address,Port, [(R,X,Y,TagList)|Z],Cstr)|Tail], Unrealized, Cplt, TList, URes, CRes, TRes) :- 
	init_tag_list(TagList) -> aux_sort_traces(Tail, Unrealized,[(Reason,Address,Port, [(R,X,Y,TagList)|Z],Cstr)| Cplt], TList, URes, CRes, TRes); 
	aux_sort_traces(Tail,[(Reason,Address,Port, [(R,X,Y,TagList)|Z],Cstr)|Unrealized], Cplt, [TagList|TList], URes, CRes, TRes).

sort_traces(Bag, Unrealized, Cplt, TRes) :- aux_sort_traces(Bag, [], [], [], Unrealized, Cplt, TRes).

equiv([],[],[],[]).
equiv([X],[Y],[X],[Y]).
equiv([ []| T], [ [] |U],A,B) :- equiv(T,U,A,B).
equiv([[X]|T],[[Y]|U],[X],[Y]) :- equiv(T,U,[],[]).


%% simplifying Pref list, to obtain just the right taglists to search trace prefixes to search for.
equivalent_taglists(List1, List2) :- equiv(List1,List2, [(Tag1,Add1)],[(Tag2,Add2)]),fd_dom(Tag1)=fd_dom(Tag2), fd_dom(Add1) = fd_dom(Add2).
%% suppressing taglists appearing several times :
%% simplify_tlist(TList, ResTList)  holds when ResTList is the simplified version of TList
%% auxiliary function : going through the first list representing tag lists, for a fixed element, 
%% the second argument is unpiled to check for occurence in current simplified list, 
%% the third argument is current value of simplified list
%% last argument is used to collect the result.
aux_simplify_tlist([], Acc, Acc, Acc).
aux_simplify_tlist([Elem | Tail], [], Acc, ResTList) :- aux_simplify_tlist(Tail, [Elem|Acc], [Elem|Acc], ResTList). 
aux_simplify_tlist([Elem| Tail], [Elem2 | Tail2], Acc, ResTList) :- 
	equivalent_taglists(Elem, Elem2) -> aux_simplify_tlist(Tail, Acc, Acc, ResTList);  
	aux_simplify_tlist([Elem | Tail], Tail2, Acc, ResTList).
%
simplify_tlist(TList, ResTList) :- aux_simplify_tlist(TList, [], [], ResTList).

% termination cases : we impose to find a prefix of trace 
% starting with our real initial set of tag lists which
% are all empty.
% 
trace_to_init(_, (gen_read(_),_, TagList,_), Acc, Acc) :- init_tag_list(TagList).
trace_to_init(_, (generate,_, TagList ,_), Acc, Acc) :- init_tag_list(TagList).
trace_to_init(_, (gen_write(_),_, TagList,_), Acc, Acc) :- init_tag_list(TagList).
trace_to_init(Zs, (Pck_Out, NPort, NTagList, State), Acc, Tail) :-
                        is_valid_hop(Rule, Zs,Pck_In, Port, TagList, Pck_Out, NPort, NTagList, State),
                        trace_to_init(Zs,(Pck_In,Port, TagList, State), [(Rule,Pck_In, Port, TagList) | Acc], Tail).

do_trace_to_init(Zs,(Pck_In,Port, TagList), (List, Cstr)) :- 
		current_state(Zs,State, _), 
		trace_to_init(Zs,(Pck_In,Port,TagList,State), [(end,Pck_In,Port,TagList)], List), 
		copy_term(append(List,Zs), append(List,Zs), Cstr).


% predicates to find all / one prefix(es) yielding TagList
possible_taglist(TagList, TracePrefix) :- findall((List, Cstr), do_trace_to_init(_,(_,_, TagList),(List,Cstr)), TracePrefix). 
one_possible_taglist(TagList, TracePrefix) :- findnsols(1, List, do_trace_to_init(_,(_,_, TagList), List), TracePrefix). 

% building the list of all/one prefix(es) for each taglist
aux_search_all_prefixes([], Acc, Acc).
aux_search_all_prefixes([TagList|Tail], Acc, PrefTList) :- possible_taglist(TagList,TracePrefix), aux_search_all_prefixes(Tail, [(TagList,TracePrefix)|Acc], PrefTList).
search_all_prefixes(TList,Res) :- aux_search_all_prefixes(TList,[],Res).

aux_search_a_prefix([], Acc, Acc).
aux_search_a_prefix([TagList|Tail], Acc, PrefTList) :- one_possible_taglist(TagList,TracePrefix), aux_search_a_prefix(Tail, [(TagList,TracePrefix)|Acc], PrefTList).
search_a_prefix(TList,Res) :- aux_search_a_prefix(TList,[],Res).

% find the right prefix to be glued in a taglist list :
find_right_prefset(_, [],[]). % if nothing matches, choice to glue empty prefix, should not happen... But better to terminate :)
find_right_prefset(TagList, [(TagList2,TracePrefix)|Tail], Res) :- equivalent_taglists(TagList, TagList2) -> Res = TracePrefix; 
			find_right_prefset(TagList, Tail, Res).


glue_trprefix([], _, Acc, Acc).
glue_trprefix([(Reason,Address,Port,List,Cstr)|Tail], PrefTList, Acc, Res) :- 
		List =[(_,_,_,TagList)|_], 
		find_right_prefset(TagList, PrefTList, TracePrefix), 
		glue_trprefix(Tail, PrefTList, [ ((Reason,Address,Port,List,Cstr), TracePrefix) | Acc], Res). 


% association of prefixes with each suffix of trace in NBag:

%glue_prefix([], Acc, Acc).
%glue_prefix([(Reason,Address,Port,List,Cstr)|Tail], Acc, Res) :- 
%		List =[(_,_,_,TagList)|_], 
%		possible_taglist(TagList, TracePrefix), 
%		glue_prefix(Tail, [ ((Reason,Address,Port,List,Cstr), TracePrefix) | Acc], Res). 

%glue_oneprefix([], Acc, Acc).
%glue_oneprefix([(Reason,Address,Port,List,Cstr)|Tail], Acc, Res) :- 
%		List =[(_,_,_,TagList)|_], 
%		one_possible_taglist(TagList, TracePrefix), 
%		glue_oneprefix(Tail, [ ((Reason,Address,Port,List,Cstr), TracePrefix) | Acc], Res). 

the_traces(CpltTr, GluedTr) :- 	accepted_traces(Bag),
				sort_traces(Bag, UBag, CpltTr, BigTagList),
				simplify_tlist(BigTagList, RTList),
				search_a_prefix(RTList, Prefixes),
				glue_trprefix(UBag,Prefixes,[],GluedTr).

enumeration(CpltTr, GluedTr) :-  accepted_traces(Bag),
                                sort_traces(Bag, UBag, CpltTr, BigTagList),
                                simplify_tlist(BigTagList, RTList),
                                search_all_prefixes(RTList, Prefixes),
                                glue_trprefix(UBag,Prefixes,[],GluedTr).

%%%%%%%%%%%%%%%%% Trace analysis functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% The predicates here allow to sort out traces according to
%% their form. 
%% Currently, only used by the pretty-printing output predicates.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% We define a language (using a DCG) to describe traces we recognize.
% They are non-empty sequances of 
% - either gen_read(Address), mem_read(Address, BDF, Tag, _) iterated a certain number of times,
% 	completion(BDF, CplBDF, Tag) iterated another number of times, and accept(compl,Address);
% - or gen_write(Address), mem_write(Address,_,_) iterated a number of times, and accept(write,Address).
pck(X) --> [X].
writing(X) --> pck(X).
writing(X) --> pck(X), writing(X).
reading(X,Y) --> writing(X), writing(Y).

head_read(Address) --> [gen_read(Address)].
trail_read(Address) --> [accept(compl,Address)].

is_a_complete_read_list(Address,BDF,Tag,CplBDF) --> head_read(Address), reading(mem_read(Address, BDF, Tag,_), completion(BDF, CplBDF, Tag)), trail_read(Address).

head_write(Address) --> [gen_write(Address)].
trail_write(Address) --> [accept(write,Address)].

is_a_complete_write_list(Address) --> head_write(Address), writing(mem_write(Address,_,_)), trail_write(Address).

% Wrapping predicates built on top of the language to discriminate traces.
first_proj_4((_,X,_,_),X).
is_usual_read_trace((Reason,Address,Port,List,_)) :- Reason = compl,
		List = [(_,_,OrigPort,_)|_], Port = ram, OrigPort = cpu, 
		maplist(first_proj_4,List, Res), is_a_complete_read_list(Address,_,_,_, Res, []).
				
 
is_usual_read_trace((Reason,Address,Port,List,_)) :- Reason = compl, 
		List = [(_,_,OrigPort,_)|_], Port = OrigPort, 
		maplist(first_proj_4,List, Res), is_a_complete_read_list(Address,_,_,_, Res, []).


is_usual_write_trace((Reason,Address,_,List,_)) :- Reason = write,
                maplist(first_proj_4,List, Res), is_a_complete_write_list(Address,Res, []).

classify(TraceTuple, usualread) :- is_usual_read_trace(TraceTuple).
classify(TraceTuple, usualwrite) :- is_usual_write_trace(TraceTuple).
classify(_, other).
