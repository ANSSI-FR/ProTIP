%%%%%%%%%%%%%%%%%%%%%% abstract_state_functions.pl %%%%%%%%%%%%%%%%%%
% This file regroups all intermediate functions using the abstract representation
% of the state created from the user input by the parser in dcg_state.pl.
% These functions are meant to be used by code modeling
% higher level aspects of the PCIe network.
%%%%
:- [dcg_state].

%%%%
% Interpretation of configuration of I/OMMU and 
% RRVL (Request Redirect Validation Logic).
% (see input.pl)
%%%%
% generic function checking belonging of (BDF,Address) to a list 
fchecks_domains(BDF,Address, [(BDF1,Dom) |_]) :- BDF=BDF1, Address in Dom.
fchecks_domains(BDF,Address, [_|Domain_List]) :- fchecks_domains(BDF,Address, Domain_List).

% checking whether Address is allowed in R/W for BDF according to the configuration
% - by I/OMMU
iommu_read(BDF,Address) :-      is_bdf(BDF), address_val(Address), is_iommu_config_read(List),
                        fchecks_domains(BDF,Address,List).
iommu_write(BDF,Address) :-     is_bdf(BDF), address_val(Address), is_iommu_config_write(List),
                        fchecks_domains(BDF,Address,List).
% - by RRVL (which we choose to model exactly in the same manner).
% The PCIe ACS specification states that the way RRVL is implemented is not specified there. 
req_redir(BDF, Address, Port) :- is_bdf(BDF), address_val(Address), root_port_authorized(Port, DomList),
                        fchecks_domains(BDF,Address, DomList).
% These predicates allow the root port to decide whether a packet should be routed upstream (possibly 
% checked by the I/OMMU before reaching ram / cpu, depending on configuration) or downstream 
% (possibly checked by the RRVL before being redirected).
% Modelisation made us sort this out but nothing has been found in the specification so as to 
% capture this configuration element. It is possible that the root port memory range is used...
is_req_for_downstream(Address, Port) :- root_port_range(Port, A..B), Address #>=A, Address #=<B.
is_req_for_upstream(Address, Port) :- root_port_up(Port, A..B), Address #>=A, Address #=<B.

%%%% Finding the configuration for a port %%%%
% Seeking of a specific port configuration in the state.
% According to the representation of state built in basic_state (dcg_state.pl)
% states are lists of tuples. Switches gather several configuration 
% headers (one for each of their port). A tuple representing a switch has the following form :
% - (Config, UpPort, UpSwitchPort, DPList, BDF) for switches, where 
%	Config is the switch configuration (the global list of all information for all downstream ports), 
%	UpPort is the physical port number of the switch upstream port, 
% 	UpSwitchPort is the physical port number of the port linked to UpPort (it is the 
%		port up the link to UpPort), 
%	DPList is the list of downstream ports in the switch,
%	BDF is the BDF of the port UpPort. 
% A tuple representing any other device (root port, endpoint) is simply :
% - (Config,Port) for non-switch ports.
%%%%

% get_port_config(Port,State,Config) holds when Config is the configuration tuple
% holding information for Port in State.
% is_port_config(Port,H,Config) holds when H is the configuration tuple matching Port and Config. 
is_port_config(Port, (Config,Port), Config). 
is_port_config(UpSwitchPort, (Config, UpPort, UpSwitchPort, DPList, Busnum), (Config, UpPort, UpSwitchPort, DPList, Busnum)).
is_port_config(Port, (Config, UpPort, UpSwitchPort, DPList, Busnum), (Config, UpPort, UpSwitchPort, DPList, Busnum)) :- member(Port,DPList).

get_port_config(Port, [H|_], Config) :- is_port_config(Port,H,Config).
get_port_config(Port,[_|T],Config) :- get_port_config(Port,T,Config).

% is_dport_acs(Port, SwConfig, ACS, BNUM) holds when Port is configured with ACS and BNUM as a bus aperture, 
% according to SwConfig, which refers to a list of switch down ports configuration.
get_dport_config(DPort, [sw_dp(DPort, up_link(UpPort), Link2, BDF, MEM, BNUM, BAR, ACS)|_], sw_dp(DPort, up_link(UpPort), Link2, BDF, MEM, BNUM, BAR, ACS)).
get_dport_config(Port,[_|T],Config) :- get_dport_config(Port,T,Config).
is_dport_acs(Port,SwConfig, ACS, BNUM) :- get_dport_config(Port, SwConfig, sw_dp(Port,_,_,_,_, BNUM, _, ACS)). 

% is_dport_bdf(Port, SwConfig, BDF) holds when in SwConfig, list of switch down port configurations, 
% Port bears BDF as an ID.
is_dport_bdf(Port, [sw_dp(Port, _, _,BDF, _, _, _, _)|_], BDF).
is_dport_bdf(Port, [_|Tail], BDF) :- is_dport_bdf(Port, Tail, BDF).



%%%% Finding type and configuration for a port %%%%
% These predicates hold when their first argument refers to a port in State matched by the
% configuration detailed in their remaining arguments.
%%%%

is_switch_downport(Port, State, (Config, UpPort, UpSwitchPort, DPList, BDF)) :- 
		get_port_config(Port, State, (Config, UpPort, UpSwitchPort, DPList, BDF)), 
		member(Port, DPList).

is_switch_upport(UpSwitchPort, State, (Config, UpPort, UpSwitchPort, DPList, BDF)) :- 
		get_port_config(UpSwitchPort, State, (Config, UpPort, UpSwitchPort, DPList, BDF)).
is_switch_port(Port, State, (Config, UpPort, UpSwitchPort, DPList, BDF)) :- 
		get_port_config(Port, State, (Config, UpPort, UpSwitchPort, DPList, BDF)).
is_endpoint(Port, State, UpPort, BDF, BAR, ACS) :- get_port_config(Port, State, endpoint(Port, up_link(UpPort), _, BDF, BAR, ACS)).

is_root_port(Port, State, DownPort, BDF, MEM, BUSN, BAR, ACS) :-
                get_port_config(Port, State, root_port(Port, _, down_link(DownPort), BDF, MEM, BUSN, BAR, ACS)).



%%%%
% Useful sub-predicates to compare variables to configuration elements. 
%%%%
% Predicates to characterize the relation between a given Address and a BAR list.
belongs_to_bar(Address,[bar(Add1..Add2)|_]) :- Address #>= Add1, Address #=< Add2.
belongs_to_bar(Address,[_|T]) :- belongs_to_bar(Address,T).


notin_bar(Address, bar(Add1.._)) :- Address #< Add1.
notin_bar(Address, bar(_..Add2)) :- Address #> Add2.
notin_barlist(_,nobar).
notin_barlist(_,[]). 
notin_barlist(Address, [bar(Add1..Add2)|T]) :- notin_bar(Address, bar(Add1..Add2)), notin_barlist(Address,T).


% Relating a BDF and a bus aperture
% NB : belonging to an aperture is taken in the large sense.
% This is what is needed for completion routing and ACS.
% In the ACS Source Validation specification, the aperture
% is defined as an inclusive range.
in_bus_aperture(bdf(B,D,F), bus_number(_, sec(Sec), sub(Sub))) :- is_bdf(bdf(B,D,F)), 
                        l_busn(bus_number(_, sec(Sec), sub(Sub)),_,[]), 
                        Sec #=< B, B #=<Sub.
% Nota : this is the /right/ manner to code not belonging in aperture. 
% Indeed, while everyone is used to have Sub >= Sec, it is not necessarily the case.
notin_bnum(bdf(B,D,F), bus_number(_, sec(Sec), sub(Sub))) :- is_bdf(bdf(B,D,F)),
                        l_busn(bus_number(_, sec(Sec), sub(Sub)),_,[]), 
                        Sec #>B, Sub #>B.
notin_bnum(bdf(B,D,F), bus_number(_, sec(Sec), sub(Sub))) :- is_bdf(bdf(B,D,F)),
                        l_busn(bus_number(_, sec(Sec), sub(Sub)),_,[]), 
                        Sec #<B, Sub #<B.


% Relating an Address to a memory range.  
belongs_to_mem(Address, mem(Add1..Add2)) :- Address #>= Add1, Address #=< Add2. 
notin_mem(Address, mem(Add1.._)) :- Address #< Add1.
notin_mem(Address, mem(_..Add2)) :- Address #> Add2.


%%%%
% Trio of functions to determine whether there is a downstream port
% claiming a given Address in a switch.
% /!\ Only taking downstream ports into account /!\
%%%%

% 1/ int_claims_address(Port, Address, SwConfigList) :
% Holds when according to SwConfigList, Address is in a BAR assigned to Port.
% (Can be true for several ports).
port_bar_claims_addr(Port, Address, [sw_dp(Port, _, _, _, _, _, BAR, _)| _]) :- belongs_to_bar(Address, BAR). 
port_bar_claims_addr(Port, Address, [_|T]) :- port_bar_claims_addr(Port, Address, T).

int_claims_address(Port, Address, SwConfigList) :-
		 port_bar_claims_addr(Port, Address, SwConfigList).

% 2/ ext_claims_address(Port, Address, SwConfigList)
% Holds when according to SwConfigList, Address is in the memory range of Port.
port_mem_claims_addr(Port, Address, [sw_dp(_, _, down_link(Port), _, MEM, _, _, _)| _]) :- belongs_to_mem(Address, MEM). 
port_mem_claims_addr(Port, Address, [_|T]) :- port_mem_claims_addr(Port, Address, T).

ext_claims_address(Port, Address, SwConfigList) :-
		 port_mem_claims_addr(Port, Address, SwConfigList).

% 3/ address_not_claimed(Port, Address) holds when no BAR or memory range
% related to Port claims Address.
% NB : implementing this predicate is necessary to compute the correct ranges
% of addresses relevant to this case.
address_not_claimed(_,[]). 
address_not_claimed(Address, [sw_dp(_, _, _, _, MEM, _, BAR, _)| T]) :- 
	notin_barlist(Address, BAR), 
	notin_mem(Address, MEM), 
	address_not_claimed(Address,T).

%%%%
% Trio of functions to determine whether there is a downstream port
% claiming a given bus number in a switch.
% /!\ Only taking downstream ports into account /!\
%%%%

% 1/ int_claims_ID(Port, BDF, SwConfigList) holds when BDF is 
% the BDF of Port according to SwConfigList.
port_has_bdf(Port, BDF, [sw_dp(Port, _, _, SBDF, _, _, _, _)| _]) :- BDF=SBDF. 
port_has_bdf(Port, BDF, [_|T]) :- port_bnum_has_bdf(Port, BDF, T).

int_claims_ID(Port, BDF, SwConfigList) :-
		 port_has_bdf(Port, BDF, SwConfigList).

% 2/ ext_claims_ID(Port, BDF, SwConfigList) holds when 
% BDF belongs to the bus aperture of Port w.r.t. SwConfigList.
port_bnum_claims_bdf(Port, BDF, [sw_dp(_, _, down_link(Port), _, _, BNUM, _, _)| _]) :- in_bus_aperture(BDF, BNUM). 
port_bnum_claims_bdf(Port, BDF, [_|T]) :- port_bnum_claims_bdf(Port, BDF, T).

ext_claims_ID(Port, BDF, SwConfigList) :-
		 port_bnum_claims_bdf(Port, BDF, SwConfigList).

% 3/ bdf_not_claimed(BDF, SwConfigList) : true when  
% no downstream port claims it and none has it.
% /!\ ONLY taking DOWNSTREAM PORTS into account.
not_port_bdf(bdf(B,D,F), sw_dp(_, _, _, bdf(B1,D1,F1), _, BNUM, _, _)) :- diff_bdf(B,D,F,B1,D1,F1), notin_bnum(bdf(B,D,F), BNUM).
bdf_not_claimed(_,[]).
bdf_not_claimed(BDF,[H|T]) :- not_port_bdf(BDF, H), bdf_not_claimed(BDF,T). 


%%%% Taglist implementation
% A taglist is a list of fixed length, it is meant to map ports
% to the list of tags marking packets that await completions.
% The ports are assumed to be numbered sequentially from 1.
% In a taglist, the i-th list is representing the tag list of port i.
%%%%

% init_tag_list(TagList) holds when TagList represents an list of 
% empty tag lists for every port in the state used by the tool.
aux_empty(0, Acc, Acc).
aux_empty(Nb, Acc, Res) :- Nb#>=1, Nb1#>=0, Nb1 #= Nb - 1, aux_empty(Nb1, [ [] | Acc], Res).
init_tag_list(TagList) :- current_state(_,_, NbPort), aux_empty(NbPort, [], TagList).

% coincide_but_nth(N, Elem1, Elem2, List1, List2, Len) is true when
% List1 and List2 have length Len, and coincide on all elements but their N-th, which is
% Elem1 for List1 and Elem2 for List2.
coincide_but_nth(_, _, _, [], [], 0).
coincide_but_nth(Nth, Nth1, Nth2, [H1|Tail1], [H2|Tail2], Len) :- 
			Nth #>=1, Len#>=1, Len2 #= Len -1,
			length(Tail1, Len2), length(Tail2, Len2),  
			(Len #= Nth -> (H1 = Nth1, H2 = Nth2);
			H1 = H2),
			coincide_but_nth(Nth, Nth1, Nth2, Tail1, Tail2, Len2).

% The following predicate is meant to capture that a pair (Tag,Address) 
% appears in the tag list for a port - which is a condition for accepting a completion. 
% outstanding_request(Port, Tag, TagList, Address, NTagList, NbPort) holds
% when :
% - NbPort is the length of TagList and NTagList, 
% - according to TagList, Port stores the pair (Tag,Address),
% - NTagList is the same as TagList except for Port, which has a tag list
% 	where (Tag,Address) is removed. 
outstanding_request(Port, Tag, TagList, Address, NTagList, NbPort) :-
			coincide_but_nth(Port, PortTagList, NPortTagList, TagList, NTagList, NbPort), 
			is_tag(Tag), 
			select((Tag, Address), PortTagList, NPortTagList).



%%%%
% ACS testing functions
% For the time being, only ACS on down stream ports are implemented.
% The other case concerns multi-function devices.
% Not all ACS are modeled : services names P2P Egress Control (enabling
% selection of P2P traffic allowed) and 
% Direct Translated P2P (enabling selection of traffic with bit AT set
% between peers) are not implemented yet. 
%%%%

% source validation
acs_src_val(acs(src_val(X),_,_,_,_,_,_),X).
% is_src_valid(ACS,BUSN,ReqBDF) holds if the Service should
% accpet the request (either validation is disabled or it checks out).
is_src_valid(ACS, _, _) :- acs_src_val(ACS,0). 
is_src_valid(ACS, BUSN, ReqBDF) :- acs_src_val(ACS,1), in_bus_aperture(ReqBDF, BUSN). 

% request redirection
do_p2p_req_redir(acs(_,_,p2p_req_red(1),_,_,_,_)).
no_p2p_req_redir(acs(_,_,p2p_req_red(0),_,_,_,_)).

% completion rediretion
do_p2p_compl_redir(acs(_,_,_,p2p_compl_red(1),_,_,_)).
no_p2p_compl_redir(acs(_,_,_,p2p_compl_red(0),_,_,_)).

% upstream forwarding
do_acs_up_fwd(acs(_,_,_,_,up_fwd(1),_,_)).
no_acs_up_fwd(acs(_,_,_,_,up_fwd(0),_,_)).

% Packets that can go through are : 
% - any packet when translation blocking is off; 
% - packets with AT set to its default value (here modeled as 0).
% acs_blocks_AT(ACS,ATbit) holds when the ACS setting commands
% to drop a packet with ATbit.
acs_blocks_AT(acs(_,tr_bl(0),_,_,_,_,_), _).
acs_blocks_AT(acs(_,tr_bl(1),_,_,_,_,_), 0).

% acs_sends_up(ACS, Port, DPort) holds when P2P request redirection
% or upstream forwarding are activated.
% The ACs specification is not so clear as to the definition of 
% "peer-to-peer requests", the way it is written leads to think
% P2P Request Redirect is independent of Upstream Forwarding,
% so that to be sent up (if it checks out ACS policy so that it is not dropped),
% a request TLP should either concern different ingress and egress ports and P2P Req Redir
% is actiavted OR have same ingress and egress port and Upstream Fwd is activated.
acs_sends_up(ACS, Port, DPort) :- do_p2p_req_redir(ACS), Port #\= DPort.
acs_sends_up(ACS, Port, Port) :- do_acs_up_fwd(ACS).

% acs_sends_down is the contrary of acs_sends_up.
% /!\ This is an implementation choice, the first behavior 
% below is undefined; when ACS Upstream Forwarding is not positioned
% requests TLPs with identical ingress and egress ports are not
% necessarily re-emitted downwards.
% Commenting the second line below corresponds
% to modeling that packets are dropped when Upstream Forwarding is not 
% activated. 
acs_sends_down(ACS, Port, DPort) :- no_p2p_req_redir(ACS), Port #\=DPort.
acs_sends_down(ACS, Port, Port) :- no_acs_up_fwd(ACS). 

% similar predicates for completions:
acs_sends_cpl_up(ACS, Port, DPort) :- do_p2p_compl_redir(ACS), Port #\= DPort.
acs_sends_cpl_up(ACS, Port, Port) :- do_acs_up_fwd(ACS).

acs_sends_cpl_down(ACS, Port, DPort) :- no_p2p_compl_redir(ACS), Port #\=DPort.
acs_sends_cpl_down(ACS, Port, Port) :- no_acs_up_fwd(ACS). 
