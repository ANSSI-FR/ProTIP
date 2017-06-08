:- [abstract_state_functions].
:- discontiguous write_hops/9.
:- discontiguous read_hops/9.
:- discontiguous completion_hops/9.
% The discontiguous requisites above allow us to specify 
% these predicates in different parts of the file.
% It is useful to describe rules associated to a port 
% in a contiguous manner, to get a sense of how it deals with events.

%%%%
% Current limitations or implementation choices impacting the result : 
% - no traffic between root ports; 
% - ACS only implemented for downstream ports and not multi-function devices;
% - ACS P2P Egress Control and Direct Translated P2P are not implemented (which
%    mimics the behavior of having them unset all the time);
% - (refered to as (*)) in the ACS specification, impact on routing packets in a switch is defined in
% terms of "peers", e.g. "peer-to-peer requests are redirected". We have not found
% a definition of peer that allows us to decide whether a switch is a peer. In practise, 
% it is possible that a device addresses a write request to an address that belongs to a BAR in a 
% switch upstream from the device. Is it a "peer-to-peer" request ? We have decided that it is not,
% for the time being. When a switch receives a request it is a "peer-to-peer request" if it is meant
% to exit by a down port when not taking ACS possible redirections into account.
% - (refered to as (**)) we choose to not check against ACS Upstream Forwarding policy IN ROOT PORTS.
% This makes the assumption that its deactivation behaves as its activation
% When actually, the behavior is UNDEFINED /!\ and it is optimistic... :)
% --> TODO future work : adapt that to reflect that it probably means that root ports do not
%	send to validation logic before redirection.
% - (***) At the moment, switches do not generate requests (read or write) or accept completions.  
% This does not prevent rogue endpoints to assume their ID, but packets bearing such IDs
% do not have side effects on switches. This is future work.
% 
% All rules are of the form :
% *_hops(Zs, <some trigger event>, Port, TagList, <some resulting event>, NPort, NewTagList, State) :- 
%				current_state(Zs, State, Port), <other conditions for the hop to occur>.
% where : 
%	- the hop predicate is meant to capture the fact that when a trigger event
%	happens in port Port, it results in another event at NPort. TagList is the list of 
%	tag lists for all ports before treatment of trigger event, NewTagList is the resulting 
%	list of tag list;
% 	- current_state(Zs, State, NbPort) captures that State is the state on which the tool
% 	is working. It is fixed in a search (see the paper for details on search strategies). 
%	Zs is the list of varaibles occuring in State and NbPort is the number of ports 
%	described in the input configuration parsed as State.
% 
% Two constant ports (non PCIe) and not in State are used : cpu and ram.
%%%%

%%%%							%%%%
% Rules of routing for a root port			   %	
% nb : no routing between root ports is implemented yet.   %
%%%%							%%%%
%%%%
% Reading rules
% (1) On reception of a reading command from the CPU :
% 	if the address to be read belongs in its memory range, 	
%	the root port generates a read request with its ID as a requesterID, and
%	updates the port tag list with a new (Tag, Address) pair.
%	The request is sent downstream.
%	For details concerning tag generation, see the paper where we justify that
%	our search algorithm only requires one element per tag list.
%	For this reason, we do not bother to take into account generating
%	distinct values. It only matters that a tag can exist, for the time being.
%%%%
read_hops(rootport_r1, Zs, gen_read(Address), cpu, TagList, mem_read(Address, BDF, Tag, AT), Down_Port, NTagList, State) :-  
                current_state(Zs, State, NbPort),
                is_root_port(Port, State, Down_Port, BDF, MEM, _, _,_),
		belongs_to_mem(Address, MEM),
		outstanding_request(Port, Tag, NTagList, Address, TagList, NbPort), % not that outstanding request is used 
						% with NTagList and TagList "reversed" : NTagList has a pair that 
						% TagList does not have.
                mem_read(Address, BDF, Tag, AT).

%%%%
% (2) On reception of a read request from dowstream :
% 	- If activated, source validation is checked (is_src_valid(ACS, BUSN, ReqBDF) appears in all rules).
% 	- (2A) If the request is meant for upstream *and* the AT bit is unset, 
%	the request is checked against the iommu policy. When OK, the right completion
%	is generated and sent downstream.
% 	(transfer to and back from RAM is abstracted here).
%	- (2B)  If the request is meant for upstream *and* the AT bit is set,
%	if Translation blocking policy allows the packet to go through, 
%	then no iommu check occurs and the right completion is sent downstream. 
%	- (2C) If the request is meant for dwonstream (can happen when received as
%	a result of legitimate redirection), then AT bit policy is checked, 
%	and the request has to check against Request Validation logic 
%	and it is redirected downstream.
%	/!\ Here we choose to not check against ACS Upstream Forwarding Policy 
%	This makes the assumption that its deactivation behaves as its activation
%	when actually, the behavior is UNDEFINED /!\ 
%%%%
% (2A)
read_hops(rootport_r2a, Zs, mem_read(Address, ReqBDF, Tag, 0), Port, TagList, completion(ReqBDF, CplBDF, Tag), Down_Port, TagList, State) :-
                current_state(Zs,State,_),
		is_root_port(Port, State, Down_Port, CplBDF, _, BUSN, _,ACS),
		is_src_valid(ACS, BUSN, ReqBDF),
		%acs_blocks_AT(ACS, 0), (should be here, but harmlessly commented out : it is always true).
		is_req_for_upstream(Address, Port),
		iommu_read(ReqBDF,Address).
% (2B)
read_hops(rootport_r2b, Zs, mem_read(Address, ReqBDF, Tag, 1), Port, TagList, completion(ReqBDF, CplBDF, Tag), Down_Port, TagList, State) :-
                current_state(Zs,State,_),
                is_root_port(Port, State, Down_Port, CplBDF, _, BUSN, _,ACS),
                acs_blocks_AT(ACS,1),  
                is_src_valid(ACS, BUSN, ReqBDF),
                is_req_for_upstream(Address, Port).
 
% (2C)
read_hops(rootport_r2c, Zs, mem_read(Address, ReqBDF, Tag, AT), Port, TagList, mem_read(Address, ReqBDF, Tag, AT), Down_Port, TagList, State) :-
                current_state(Zs,State,_),
                is_root_port(Port, State, Down_Port, _, _, BUSN, _,ACS),
		acs_blocks_AT(ACS, AT),
                is_src_valid(ACS, BUSN, ReqBDF),
                is_req_for_downstream(Address, Port),
		req_redir(ReqBDF, Address, Port).

%%%%
% Writing rules
% (3) On reception of a writing command from the CPU :
%	if the address to be written belongs to its memory range, 	
%	the root port generates a write request with its ID as a requesterID, and
%	the request is sent downstream.
%%%%
write_hops(rootport_r3, Zs, gen_write(Address), cpu, TagList, mem_write(Address, BDF, AT) , Down_Port, TagList, State) :-
                current_state(Zs,State,_),
                is_root_port(_, State, Down_Port, BDF, MEM, _,_,_),
                belongs_to_mem(Address, MEM),
                mem_write(Address, BDF, AT).

%%%%
% (4) On reception of a mem_write request from downstream :
% 	- If activated, source validation is checked (is_src_valid(ACS, BUSN, ReqBDF) appears in all rules).
% 	- (4A) If the request if meant for upstream *and* the AT bit is unset, 
%	the request is checked against the iommu policy, and when OK ram accepts it.
%	- (4B) If the request if meant for upstream *and* the AT bit is set, 
%	if Translation blocking policy allows the packet to go through, 
%	then no iommu check occurs and ram accepts it. 
%	- (4C) If the request is meant for downstream (can happen when received as
%	a result of legitimate redirection), then AT bit policy is checked, 
%	and the request has to check against Request Validation logic 
%	and it is redirected downstream.
%	/!\ Here we choose to not check against ACS Upstream Forwarding Policy 
%	This makes the assumption that its deactivation behaves as its activation
%	when actually, the behavior is UNDEFINED /!\ 
%%%%

% (4A) 
write_hops(rootport_r4a, Zs, mem_write(Address, ReqBDF, 0), Port, TagList, accept(write,Address), ram, TagList, State) :- 
		current_state(Zs,State,_),
		is_root_port(Port, State, _, _, _,BUSN, _,ACS),
		%acs_blocks_AT(ACS, 0), always true for 0
		is_src_valid(ACS, BUSN, ReqBDF), 
		is_req_for_upstream(Address, Port),
		iommu_write(ReqBDF, Address).

% (4B)
write_hops(rootport_r4b, Zs, mem_write(Address, ReqBDF, 1), Port, TagList, accept(write,Address), ram, TagList, State) :- 
		current_state(Zs,State,_),
		is_root_port(Port, State, _, _, _,BUSN, _,ACS),
		acs_blocks_AT(ACS, 1), 
		is_src_valid(ACS, BUSN, ReqBDF), 
		is_req_for_upstream(Address, Port).

% (4C)
write_hops(rootport_r4c, Zs, mem_write(Address, ReqBDF, AT), Port, TagList, mem_write(Address, ReqBDF, AT), Down_Port, TagList, State) :-
                current_state(Zs,State,_),
                is_root_port(Port, State, Down_Port, _, _,BUSN, _,ACS),
		acs_blocks_AT(ACS, AT),
                is_src_valid(ACS, BUSN, ReqBDF),
                is_req_for_downstream(Address, Port),
		req_redir(ReqBDF, Address, Port).


%%%%
% Completion rules
% (5) On reception of a completion :
%	- (5A) when the requester ID in the completion is the root port BDF, 
%	and the tag matches an outstanding request, completion is returned to port cpu.
%	- (5B) otherwise, since we do not implement communication between root ports, 
%	the completion is meant for downstream. As above, no checking against ACS Upstream
%	Forwarding.
%%%%
% (5A)
completion_hops(rootport_r5a, Zs, completion(ReqBDF, _, Tag), Port, TagList, accept(compl,Address), cpu, NTagList, State) :- 
		current_state(Zs,State,NbPort),
		is_root_port(Port, State,_, ReqBDF, _, _,_,_),
		outstanding_request(Port, Tag, TagList, Address, NTagList, NbPort), 
		address_val(Address). 
% (5B)
completion_hops(rootport_r5b, Zs, completion(bdf(B1,D1,F1), BDF, Tag), Port, TagList, completion(bdf(B1,D1,F1), BDF, Tag), Down_Port, TagList, State) :-
                current_state(Zs,State,_),
                is_root_port(Port, State,Down_Port, bdf(B,D,F), _, _,_,_),
		diff_bdf(B,D,F,B1,D1,F1).	

%%%%
% Routing rules in a switch 
% - Traffic inside a switch is not observable on a PCIe fabric.
% Even though there is a virtual bus in a switch, we choose to model switches as
% wholes.
% - nb : everytime several ports verify the conditions and claim a packet, 
% a rule applies. Hence, if multiple ports claim a packet, 
% the tool lists all possible traces in the end.
%%%%
%%%%
% Rule for writing requests
% (1) On reception of a write request for an address claimed internally (i.e. by a BAR) :
% 	- (1A) received from downstream : 
%	if ACS policy checks out (here, Source Validation and Translation Blocking; 
%	Upstream Fwd does not apply by definition and Request Redirect is ill-defined
%	(see (*) above)), then the packet is accepted.
%	- (1B) received from upstream : packet is accepted.
% (2) On reception of a write request for an address claimed externally (in a memory range MEM) :  	
%	- (2A) received from downstream :
%		it depends on the ACS policy on the down port, but in every case, Source Validation and
%		Translation Blocking policy are checked. If they pass, the packet can be routed up OR 
%		to a port that claims it, according to P2P Req Redirect and Upstream Forwarding.
%		(see acs_sends_up and acs_sends_down predicates in abstract_state_functions.pl)
%		- (2AA) the ACS policy implies redirection upstream, packet is sent upstream. 
%		- (2AB) everything is off : packet is sent to any port that claims it. 
%	- (2B) received from upstream : 
%	packet routed to a port that claims it.
% (3) On reception of a write request for an address not managed by the switch :
% 	the packet is dropped if it comes from upstream (no rule), otherwise, if
%	it is received on a down port, respect of the ACS policy is checked before forwarding
%	upstream.
%%%%
% (1A)
write_hops(switch_r1a,Zs,mem_write(Address,ReqBDF, AT), Port, TagList, accept(write,Address), Port, TagList, State) :- 
		current_state(Zs,State,_),
		mem_write(Address,ReqBDF, AT), 
		is_switch_downport(Port, State, (switch(SwConfig),_,_,_,_)),
		is_dport_acs(Port, SwConfig, ACS, BNUM), is_src_valid(ACS, BNUM, ReqBDF),
		acs_blocks_AT(ACS, AT),
		int_claims_address(_, Address, SwConfig). 
% (1B)
write_hops(switch_r1b, Zs,mem_write(Address,Y, AT), Port, TagList, accept(write,Address), Port, TagList, State) :- 
		current_state(Zs,State,_),
		mem_write(Address,Y, AT), 
		is_switch_upport(Port, State, (switch(SwConfig),_,_, _,_)),
		int_claims_address(_, Address, SwConfig). 

% (2AA) 
write_hops(switch_r2aa, Zs,mem_write(Address,ReqBDF, AT), Port, TagList, mem_write(Address,ReqBDF, AT), UpPort, TagList, State) :-
		current_state(Zs,State,_),
                mem_write(Address,ReqBDF, AT),
		is_switch_downport(Port, State, (switch(SwConfig),UpPort,_,_,_)),
		is_dport_acs(Port, SwConfig, ACS, BNUM), is_src_valid(ACS, BNUM, ReqBDF),
		acs_blocks_AT(ACS, AT),
                ext_claims_address(DPort, Address, SwConfig), 
		acs_sends_up(ACS, Port, DPort).

% (2AB) 
write_hops(switch_r2ab, Zs,mem_write(Address,ReqBDF, AT), Port, TagList, mem_write(Address,ReqBDF, AT), DPort, TagList, State) :-
		current_state(Zs,State,_),
                mem_write(Address,ReqBDF, AT),
		is_switch_downport(Port, State, (switch(SwConfig),_,_,_,_)),
		is_dport_acs(Port, SwConfig, ACS, BNUM), is_src_valid(ACS, BNUM, ReqBDF),
		acs_blocks_AT(ACS, AT),
                ext_claims_address(DPort, Address, SwConfig), 
		acs_sends_down(ACS,Port,DPort).

% (2B) 
write_hops(switch_r2b, Zs,mem_write(Address,Y, AT), Port, TagList, mem_write(Address,Y, AT), DPort, TagList, State) :-
		current_state(Zs,State,_),
                mem_write(Address,Y, AT),
		is_switch_upport(Port, State, (switch(SwConfig),_,_,_,_)),
                ext_claims_address(DPort, Address, SwConfig).

% (3) 
write_hops(switch_r3, Zs,mem_write(Address,ReqBDF, AT), Port, TagList, mem_write(Address,ReqBDF, AT), UpPort, TagList, State) :-
		current_state(Zs,State,_),
                mem_write(Address, ReqBDF, AT),
                is_switch_downport(Port, State, (switch(SwConfig),UpPort,_,_,_)),
		is_dport_acs(Port, SwConfig, ACS, BNUM), is_src_valid(ACS, BNUM, ReqBDF),
		acs_blocks_AT(ACS, AT),
                address_not_claimed(Address, SwConfig).

%%%%
% Reading requests rules
% (4) On reception of a reading request bearing an address claimed internally :
%	- no rule for the case when the Requester ID is an internal port.
%	While this case is not documented particularly, it could happen
%	that such a packet is received by a switch port as a result of
%	an usurpation of ID. However, whether the switch locally answers the request
%	is invisible to us outside the switch... 
%	- (4A) received from downstream : 
%	ACS policy has to be checked.
%	If Source Validation and Direct Translation allow packet processing, then :
%		- (4AA) if the Requester ID is managed by the switch 
%		(in the bus aperture of a down port) 
%		then assuming (*) we do not check for potential redirection (switches 
%		are not peers).
%		- (4AB) if the Requester ID is not managed by the switch 
%		a completion is sent upstream. 
%		(/!\ UNSPECIFIED /!\
%		This is weird, it likely means ID usurpation and will be prevented 
%		by ACS Source Validation when activated.
%		However, to our knowledge, nothing really states what happens when 
%		Source Validation is not set, so that we choose the worst case
%		scenario and suppose an eager response :) ).
%	- (4B) received from upstream : 
%		- (4BA) if the Requester ID is managed by the switch, 
%		(can happen after validation by RRVL) :
%		Send the right completion downstream.
%		- (4BB) if the Requester ID is not managed by the switch
%		send the right completion upstream.
%%%%
%(4AA)
read_hops(switch_r4aa,Zs,mem_read(Address,ReqBDF,Tag, AT), Port, TagList, completion(ReqBDF,BDF,Tag), NPort, TagList, State) :-
                               current_state(Zs,State,_),
                               mem_read(Address, ReqBDF, Tag, AT),
                               is_switch_downport(Port, State, (switch(SwConfig),_,_,_,_)),
                               is_dport_acs(Port, SwConfig, ACS, BNUM), is_src_valid(ACS, BNUM, ReqBDF),
                               acs_blocks_AT(ACS, AT),
                               int_claims_address(DPort, Address, SwConfig),
                               ext_claims_ID(NPort, ReqBDF, SwConfig),
                               is_dport_bdf(DPort, SwConfig, BDF),
                               completion(ReqBDF, BDF, Tag).

% (4AB)
read_hops(switch_r4ab, Zs,mem_read(Address,ReqBDF,Tag, AT), Port, TagList, completion(ReqBDF,BDF,Tag), NPort, TagList, State) :-
				current_state(Zs,State,_),
                                mem_read(Address, ReqBDF, Tag, AT),
				is_switch_downport(Port, State, (switch(SwConfig),NPort,_,_,_)), 
                               	is_dport_acs(Port, SwConfig, ACS, BNUM), is_src_valid(ACS, BNUM, ReqBDF),
				acs_blocks_AT(ACS, AT),
				int_claims_address(DPort, Address, SwConfig),
                                is_dport_bdf(DPort, SwConfig, BDF),
                                completion(ReqBDF, BDF, Tag), 
				bdf_not_claimed(ReqBDF,SwConfig).

% (4BA)
read_hops(switch_r4ba, Zs,mem_read(Address,ReqBDF,Tag, AT), Port, TagList, completion(ReqBDF,BDF,Tag), NPort, TagList, State) :-
				current_state(Zs,State,_),
                                mem_read(Address, ReqBDF, Tag, AT),
                                is_switch_upport(Port, State, (switch(SwConfig),_,_,_,_)),
				int_claims_address(DPort, Address, SwConfig),
				is_dport_bdf(DPort, SwConfig, BDF),
                                completion(ReqBDF, BDF, Tag), 
				ext_claims_ID(NPort, ReqBDF, SwConfig). 

% (4BB)
read_hops(switch_r4bb, Zs,mem_read(Address,ReqBDF,Tag, AT), Port, TagList, completion(ReqBDF,BDF,Tag), NPort, TagList, State) :-
				current_state(Zs,State,_),
                                mem_read(Address, ReqBDF, Tag, AT),
                                is_switch_upport(Port, State, (switch(SwConfig),NPort,_,_,_)),
                                int_claims_address(DPort, Address, SwConfig),
                                is_dport_bdf(DPort, SwConfig, BDF),
                                completion(ReqBDF, BDF, Tag),
				bdf_not_claimed(ReqBDF,SwConfig). 
 
%%%%
% (5) On reception of a request to an address managed by the switch (in the MEM range of a down port) :
%	- (5A) received from upstream : 
%		request routed through down port claiming the address
%	- (5B) received from downstream : 
%	Check ACS policy regarding Source Validation and Direct Translation.
%	Check ACS policy reagrding possible redirection : 
%		- (5BA) redirection upstream
%		- (5BB) request routed through down port claiming the address
%	
%%%%
% (5A)
read_hops(switch_r5a,Zs,mem_read(Address,ReqBDF,Tag, AT), Port, TagList, mem_read(Address,ReqBDF,Tag, AT), DPort, TagList, State) :-
				current_state(Zs,State,_),
                                mem_read(Address, ReqBDF, Tag, AT),
                                is_switch_upport(Port, State, (switch(SwConfig),_,_,_,_)),
                                ext_claims_address(DPort, Address, SwConfig).

% (5BA)
read_hops(switch_r5ba, Zs,mem_read(Address,ReqBDF,Tag, AT), Port, TagList, mem_read(Address,ReqBDF,Tag, AT), NPort, TagList, State) :-
				current_state(Zs,State,_),
                                mem_read(Address, ReqBDF, Tag, AT),
                                is_switch_downport(Port, State, (switch(SwConfig), NPort,_,_,_)),
                               	is_dport_acs(Port, SwConfig, ACS, BNUM), is_src_valid(ACS, BNUM, ReqBDF),
				acs_blocks_AT(ACS, AT),
                                ext_claims_address(DPort, Address, SwConfig), 
				acs_sends_up(ACS, Port, DPort).


% (5BB)
read_hops(switch_r5bb, Zs,mem_read(Address,ReqBDF,Tag, AT), Port, TagList, mem_read(Address,ReqBDF,Tag, AT), DPort, TagList, State) :-
				current_state(Zs,State,_),
                                mem_read(Address, ReqBDF, Tag, AT),
                                is_switch_downport(Port, State, (switch(SwConfig),_,_,_,_)),
                               	is_dport_acs(Port, SwConfig, ACS, BNUM), is_src_valid(ACS, BNUM, ReqBDF),
				acs_blocks_AT(ACS, AT),
                                ext_claims_address(DPort, Address, SwConfig), 
				acs_sends_down(ACS, Port, DPort).
%%%%
% (6) On reception of a request for an address not managed :
%	- (norule) on an upstream port, it is specified as an unsupported request
%	- on a downstream port, route upstream if ACS policy checks out 
%	(only Source Validation and Direct Translation apply).
%%%%
read_hops(switch_r6, Zs,mem_read(Address,ReqBDF,Tag, AT), Port, TagList, mem_read(Address,ReqBDF,Tag, AT), NPort, TagList, State) :-
				current_state(Zs,State,_),
		                mem_read(Address,ReqBDF,Tag, AT),
				is_switch_downport(Port, State, (switch(SwConfig),NPort,_,_,_)), 
                               	is_dport_acs(Port, SwConfig, ACS, BNUM), is_src_valid(ACS, BNUM, ReqBDF), 
				acs_blocks_AT(ACS, AT),
				address_not_claimed(Address,SwConfig).

%%%%
% Rules for completion requests
% nb : ACS Source Validation and Translation Blocking do not apply to completions.
% nb : according to (***), completions are not accepted by switches. This is future work.
% As a result, we do not write rules for the case when a completion bearing a Requester ID 
% claimed by a switch reaches the switch, thus capturing that such a packet is not accepted.
%%%%
%%%%
% (7) On reception of a completion bearing a requester ID managed by the switch :
%	- (7A) received from upstream : routed normally
%	- (7B) received from downstream : 
%		- (7BA) if ACS dictates sending up 
%		(P2P completion redir OR Upstream FWD)
%		then send upstream.
%		- (7BB) if ACS does not redirect, then route normally
% (8) On reception of a completion bearing an ID not managed by the switch :
%	- (norule) this is a specified error when the completion comes
%	from upstream
%	- received from downstream : routed upstream. 
%
%%%%
% (7A) 
completion_hops(switch_r7a, Zs,completion(ReqBDF,BDF,Tag), Port, TagList, completion(ReqBDF,BDF,Tag), NPort, TagList, State) :-  
				current_state(Zs,State,_),
				completion(ReqBDF,BDF,Tag), 
				is_switch_upport(Port, State, (switch(SwConfig),_,_,_,_)),
                                ext_claims_ID(NPort, ReqBDF, SwConfig).


% (7BA) 
completion_hops(switch_r7ba, Zs,completion(ReqBDF,BDF,Tag), Port, TagList, completion(ReqBDF,BDF,Tag), NPort, TagList, State) :-  
				current_state(Zs,State,_),
				completion(ReqBDF,BDF,Tag), 
				is_switch_downport(Port, State, (switch(SwConfig),NPort,_,_,_)),
				is_dport_acs(Port, SwConfig, ACS, _), 
                                ext_claims_ID(DPort, ReqBDF, SwConfig), 
				acs_sends_cpl_up(ACS, Port, DPort).

% (7BB) 
completion_hops(switch_r7bb, Zs,completion(ReqBDF,BDF,Tag), Port, TagList, completion(ReqBDF,BDF,Tag), DPort, TagList, State) :-  
				current_state(Zs,State,_),
				completion(ReqBDF,BDF,Tag), 
				is_switch_downport(Port, State, (switch(SwConfig),_,_,_,_)),
				is_dport_acs(Port, SwConfig, ACS, _), 
                                ext_claims_ID(DPort, ReqBDF, SwConfig), 
				acs_sends_cpl_down(ACS, Port, DPort).


% (8) 
completion_hops(switch_r8, Zs,completion(ReqBDF,BDF,Tag), Port, TagList, completion(ReqBDF,BDF,Tag), NPort, TagList, State) :-
				current_state(Zs,State,_),
		                completion(ReqBDF,BDF,Tag), 
				is_switch_downport(Port, State, (switch(SwConfig),NPort,_,_,_)), 
				bdf_not_claimed(ReqBDF,SwConfig).

%%%%					%%%%
% Routing rules for endpoint devices	   %
%%%%					%%%%							
% (1) On reception of a read request for an address claimed, emission of matching completion
%   	(norule) for an unclaimed address, unsupported request.
% (2) On reception of a write request for an address claimed, packet accepted
%	(norule) for an unclamied address, unsupported request
% (3) On reception of a completion from Requester ID claimed, packet accepted if existing Tag
% 	stored, dropped otherwise (norule).
% (4) Generation of reading requests conforming to specification
% (5) Generation of writing requests conforming to specification
%
% Emission of non-conformant traffic
% nb : no condition are verified to emit or accept a packet in such conditions.
% (6) Generation of arbitrary reading requests 
% (7) Generation of arbitrary writing requests 
% (8) Generation of arbitrary completions  
% (9) Unconditional acceptation of completions
%%%%
% (1)
read_hops(endpoint_r1, Zs,mem_read(Address,ReqBDF,Tag, AT), Port, TagList, completion(ReqBDF,BDF,Tag), UpPort, TagList, State) :-
				current_state(Zs,State,_),
                                mem_read(Address, ReqBDF, Tag, AT),
                                is_endpoint(Port, State, UpPort, BDF, BAR,_),
                               	belongs_to_bar(Address, BAR), 
                                completion(ReqBDF, BDF, Tag).

% (2)
write_hops(endpoint_r2, Zs, mem_write(Address, Y, AT), Port, TagList, accept(write,Address), Port, TagList, State) :-
				current_state(Zs,State,_),
                                mem_write(Address, Y, AT),
                                is_endpoint(Port, State, _, _, BAR,_),
                               	belongs_to_bar(Address, BAR).

% (3)
completion_hops(endpoint_r3, Zs, completion(ReqBDF,BDF,Tag), Port, TagList, accept(compl,Address), Port, NTagList, State) :-
				current_state(Zs,State,NbPort),
                                completion(ReqBDF, BDF, Tag),
                                is_endpoint(Port, State, _, ReqBDF, _,_), 
				outstanding_request(Port, Tag, TagList, Address, NTagList, NbPort), 
				address_val(Address).

% (4)
read_hops(endpoint_r4, Zs, gen_read(X), Port, TagList, mem_read(X, BDF, Tag, AT), UpPort, NTagList, State) :- 
				current_state(Zs,State,NbPort),
				is_endpoint(Port, State, UpPort, BDF, _,_), 
				mem_read(X, BDF, Tag, AT), 
				outstanding_request(Port, Tag, NTagList, X, TagList, NbPort).
% (5)
write_hops(endpoint_r5, Zs, gen_write(X), Port, TagList, mem_write(X, BDF, _), UpPort, TagList, State) :- 
				current_state(Zs,State,_),
				is_endpoint(Port, State, UpPort, BDF, _,_).

% (6)
read_hops(endpoint_r6, Zs, generate, Port, TagList, mem_read(X, BDF, Tag, AT), UpPort, TagList, State) :- 
				current_state(Zs,State,_),
				is_endpoint(Port, State, UpPort,_,_,_), 
				mem_read(X, BDF, Tag, AT).
% (7)
write_hops(endpoint_r7, Zs, generate, Port, TagList, mem_write(Address,BDF,AT), UpPort, TagList, State) :- 
				current_state(Zs,State,_),
				is_endpoint(Port, State, UpPort, _,_,_), 
				mem_write(Address,BDF,AT).

% (8)
completion_hops(endpoint_r8, Zs, generate, Port, TagList, completion(ReqBDF,BDF,Tag), UpPort, TagList, State) :-
				current_state(Zs,State,_),
                                is_endpoint(Port, State, UpPort, _,_,_), 
				completion(ReqBDF,BDF,Tag).

% (9)
completion_hops(endpoint_r9, Zs, completion(ReqBDF,BDF,Tag), Port, TagList, accept(compl,Address), Port, TagList, State) :-
                                current_state(Zs,State, _),
                                completion(ReqBDF, BDF, Tag),
                                is_endpoint(Port, State, _,_,_,_), 
				address_val(Address).
