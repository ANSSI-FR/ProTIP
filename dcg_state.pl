%%%%%%%%%%% Description of a PCIe network
% The tool works on an instantiated PCIe network. This file describes
% which inputs are valid, how to write them, parse them, 
% and the abstract representation of the network (States)
% used in the rest of the tool. The validity of an input
% is a weak notion, in the sense that it just means
% that the configuration can be successfully parsed - it does
% not imply anything about the fact that the PCIe fabric is 
% realistic or functional !
%%%% Language recognized by the tool for inputs.
% 1 - definitions of things to describe a network : 
% 	1.A - a way to describe physical links
%	1.B - BDFs of every physical port (note that this info is *not* included in configuration
%		headers).
%  	1.C - all fields that can appear in configuration headers (of type 0 and 1)
%		that we model (this encompasses all fields whose modification
%		we take into account using configuration write requests, 
%		and one additional field (because it is important to model
%		the ability of a type 0 configuration write request to reach a port
%		even though we do not take into account an actual modification of some 
%		field since this changes the port BDF).   
%%%%
:- [definitions].
:- [input].
:- use_module(library(clpfd)).
:- use_module(library(lists)).


l_link(Port,up_link(Up),down_link(none)) --> [Port, Up, -1], {portname(Port), portname(Up), all_distinct([Port, Up])}.
l_link(Port,up_link(Up),down_link(Down)) --> [Port, Up, Down], {portname(Port), portname(Up), portname(Down), all_distinct([Port, Up, Down])}.

l_bdf(bdf(B,D,F)) --> [B,D,F], {is_bus_num(B), is_dev_num(D), is_fun_num(F)}.

l_bar(BARBase..BARTop) --> [BARBase..BARTop], {address_val(BARBase), address_val(BARTop)}.

l_bar_c0(nobar) --> [].
l_bar_c0([bar(X)]) --> l_bar(X).
l_bar_c0([bar(X), bar(Y)])--> l_bar(X), l_bar(Y).
l_bar_c0([bar(X), bar(Y), bar(Z)])--> l_bar(X), l_bar(Y), l_bar(Z).
l_bar_c0([bar(X), bar(Y), bar(Z), bar(T)])--> l_bar(X), l_bar(Y), l_bar(Z), l_bar(T).
l_bar_c0([bar(X), bar(Y), bar(Z), bar(T), bar(U)])--> l_bar(X), l_bar(Y), l_bar(Z), l_bar(T), l_bar(U).
l_bar_c0([bar(X), bar(Y), bar(Z), bar(T), bar(U), bar(V)])--> l_bar(X), l_bar(Y), l_bar(Z), l_bar(T), l_bar(U), l_bar(V).

l_bar_c1(nobar) --> [].
l_bar_c1([bar(X)]) --> l_bar(X).
l_bar_c1([bar(X), bar(Y)])--> l_bar(X), l_bar(Y).

l_mem(mem(MemBase..MemTop)) --> [MemBase..MemTop], {address_val(MemBase), address_val(MemTop)}.

l_busn(bus_number(prim(Primary), sec(Secondary), sub(Subordinate))) --> [Primary, Secondary, Subordinate], {is_bus_num(Primary), is_bus_num(Secondary), is_bus_num(Subordinate)}.

% Access Control Services. In specification, two cases (down ports and multi-function devices) and one field.
% We model the two cases separately to simplify term writing.
l_acs_dp(acs(src_val(V), tr_bl(B), p2p_req_red(R), p2p_compl_red(C), up_fwd(U), p2p_eg_control(E), dir_tr(T))) -->
[V,B,R,C,U,E,T], {is_bool(V), is_bool(B), is_bool(R), is_bool(C), is_bool(U), is_bool(E), is_bool(T)}.

l_acs_mfd(acs(p2p_req_red(R), p2p_compl_red(C), p2p_eg_control(E), dir_tr(T))) -->
[R,C,E,T], {is_bool(R), is_bool(C), is_bool(E), is_bool(T)}.

%%%%
% 2 - description of network component configuration :
%%%%
l_endpoint(Port, endpoint(Port, Link1, down_link(none), BDF, BAR, ACS)) --> 	
		[endpoint], 
		l_link(Port,Link1,down_link(none)), 
		l_bdf(BDF), 
		l_bar_c0(BAR), 
		l_acs_mfd(ACS).

l_rciep(Port, rciep(Port, Link1, Link2, BDF, BAR, ACS)) --> 
		[root_integrated_endpoint], 
		l_link(Port,Link1,Link2), 
		l_bdf(BDF), 
		l_bar_c0(BAR), 
		l_acs_mfd(ACS).

          
l_root_port(Port,root_port(Port, Link1, Link2, BDF, MEM, BNUM, BAR, ACS)) --> 
		[root_port_bridge], 
		l_link(Port, Link1, Link2), 
		l_bdf(BDF), 
		l_mem(MEM), 
		l_busn(BNUM), 
		l_bar_c1(BAR), 
		l_acs_dp(ACS).
% example of description of a root port :
% root_port_bridge, 1, 0, 2, 0, 1, 0, 0xc0000000..0xffffffff, 0, 1, 4, 0,0,0,0,0,0,0
%   where : 1, 0, 2 are the physical port number, the port upstream and port downstream
%	    0,1,0 is the B,D,F of the root port
%           0xc0000000..0xffffffff is its MEM range 	
% 	    0,1,4 are the primary, secondary and subordinate bus numbers
%	    it has no BAR,
% 	    and 0,0,0,0,0,0,0 is the ACS setting (no ACS are activated).     

%%%%
% Switches are modeled as an up port and a list of down ports.
% Each down port references a link to a physical port downstream, 
% and the upport references physical port upstream of the switch 
% (i.e. the port to which the upswitchport of the switch is linked).
% The BDF associated to the switch is that of the upswitchport.
% According to the specification, a switch can be seen as a collection 
% of virtual bridges BDFs, or as having an internal bus to which down ports
% are attached. Both of these definitions mean that a switch does not 
% have one BDF, but several : one for its up port, and one per down port. 
% This is why there are BDFs appearing in down ports configuration.
%%%%
l_switch_dp(UpPort, DPort, sw_dp(DPort, up_link(UpPort), Link2, BDF, MEM, BNUM, BAR, ACS)) --> 
		[switch_downstream_port], 
		l_link(DPort,up_link(UpPort),Link2), 
		l_bdf(BDF), 
		l_mem(MEM), 
		l_busn(BNUM), 
		l_bar_c1(BAR), 
		l_acs_dp(ACS).

ll_switch_dp(_,[],[]) --> [switch_end].
ll_switch_dp(UpPort, [DPort|T], [X|L]) --> l_switch_dp(UpPort, DPort, X), ll_switch_dp(UpPort, T, L).			

l_switch(UpPort, UpSwitchPort, DPList, BDF, switch(X)) --> [switch], [UpSwitchPort], l_bdf(BDF), ll_switch_dp(UpPort, DPList, X), {portname(UpSwitchPort)}.

%%%%
% All elements of a PCIe network are describable by terms defined above.
% The combination of all these elements forms the descritpion of a fabric. 
% Predicate basic_state defines all elementary components, while state
% is meant to describe the whole fabric.
% state has four arguments : 
% - abstract term built
% - nb of physical ports in the fabric
% - input to be parsed describing the fabric
% - accumulation term (see DCG in Prolog).
% Typical use of state : state(AbsTerm,NbPort,Input,[]) holds
% when AbsTerm is the abstract representation of the Input parsed, which
% defines NbPort physical ports.
%%%
 
basic_state((X,Port)) --> l_endpoint(Port,X).
basic_state((X,Port)) --> l_rciep(Port,X).
basic_state((X,Port)) --> l_root_port(Port,X).
basic_state((X,UpPort, UpSwitchPort, DPList,BDF)) --> l_switch(UpPort, UpSwitchPort, DPList, BDF, X).
% we recall that UpSwitchPort is the port number of the switch up port 
% while UpPort is that of the port upstream of UpPort 
% (i.e. UpPort IS NOT a port of this switch).

is_nbport((X,L), NbPort):- X = switch(_) -> (L = (_,_,DPList,_), length(DPList,N), NbPort #= N+1) ;
		NbPort = 1.

state([], 0, [],[]).
state([A|C], NbPort, B, E) :-
	basic_state(A, B, D),
	is_nbport(A,NbPort0),
	state(C, NbPort1, D, E),
	NbPort #= NbPort1 + NbPort0.
