%%%%
% input.pl contains the elements of configuration of the system
% analysed by ProTIP
% This file must be edited to reflect the system you want to analyze.
% In future work, we have the plan to develop a configuration parser for Linux-based systems
% using setpci, which would allow to automatically fill in the state part.
%%%%

% When Request Redirect Validation logic exists, it is meant to check on request TLPs 
% arriving at a root port by result of a redirection. To decide whether a TLP
% should be directed upstream (at CPU/RAM) or go through the RRVL before being
% redirected downstream (if not dropped by the RRVL), a criterion is needed.
% It can be the root port memory range, but does not /have to be/.
% There is in the specification an explicit mention that RRVL does not belong in
% the PCIe specification.
% As a result, we have chose to use to model this choice 
% two predicates : 
% 1/ root_port_range(Port, Range) captures that requests in Range should
% be redirected downstream
% 2/ root_port_up(Port, Range) captures that requests in Range are meant
% to exit the root complex for cpu/ram.
% example of configuration where everything is routed upstream :
root_port_range(-1, 0..0). %one instance of the predicate /has/ to exist, but the -1 cannot be a rela Port value.
root_port_up(1, 0..0xffffffff).

%%%%
% I/OMMU configuration is captured as two lists.
%%%%
% Configuration example to forbid everything :
%is_iommu_config_read([]).
%is_iommu_config_write([]).

% Configuration example to allow everything :
is_iommu_config_read([(BDF, 0..0xffffffff)]) :- is_bdf(BDF).
is_iommu_config_write([(BDF, 0..0xffffffff)]) :- is_bdf(BDF).

% Configuration example to allow specific ranges for specific IDs :
%is_iommu_config_read([(bdf(6,0,0), 42..45), (bdf(7,0,0), 0..59)]).
%is_iommu_config_write([(bdf(6,0,0), 42..47), (bdf(7,0,0), 0..59)]).
%

%%%%
% RRVL configuration : as said above, this is not specified,
% but we need to model it, so we choose to copy the I/OMMU modelization :
% a list contains the address range that a request bearing a given ID is allowed to 
% use. 
%%%%
root_port_authorized(1, [(bdf(_,_,_), 0..0xffffffff)]).

%%%%						%%%%
% This is the description of the PCIe network 	   %
%%%%						%%%%
%%% This is where the file should be edited to reflect the PCIe configuration of your system :
% An example configuration is commented on to allow you to modify it,
% other elements (root ports, switches, switch downports inside of switches, and endpoints)
% can be added, separated by commas.
% Questions concerning this ad-hoc format are likely to be solved reading dcg_state.pl, where
% the parser is written.
my_state([%
%%% ROOT PORT
% A root port description starts with the keyword, followed by port number, port upstream, port downstream, 
% ID represented as bus number, device number and function number, then memory range, 
% then primary, secondary and subordinate bus numbers, BARs (or nothing when no BAR), and ACS configuration
% (no ACS = everything deactivated).
root_port_bridge, 1, 0, 2, 0, 1, 0, 0xc0000000..0xffffffff, 0, 1, 4, 0,0,0,0,0,0,0,
%   where : 1, 0, 2 are the physical port number, the port upstream and port downstream
%           0,1,0 is the B,D,F of the root port
%           0xc0000000..0xffffffff is its MEM range     
%           0,1,4 are the primary, secondary and subordinate bus numbers
%           it has no BAR,
%           and 0,0,0,0,0,0,0 is the ACS setting (no ACS are activated).     
%
%%% SWITCH
% A switch description starts with the keyword, followed by port number (number of the up port of the switch)
% then there is this port BDF and 
% switch downports configurations, before the keyword to mark the end.
switch, 2, 1,0,0,
switch_downstream_port, 3, 1, 4, 2, 4, 0, 0xf1000000..0xf1003fff, 1, 3, 3, 0xc0000000..0xc0000fff, 0,0,0,0,0,0,0,
% for a switch downstream port, after the keyword there is the port number, port upstream number, port downstream number, 
% the BDF, the MEM range, the primary, secondary and subordinate bus numbers, BARs if there are some, and ACS configuration.
switch_downstream_port, 5, 1, 6, 2, 8, 0, 0xf1004000..0xf1005fff, 1, 4, 4, 0,0,0,0,0,0,0,
switch_end,
%%% ENDPOINTS
% endpoint configuration consists of : port number, port upstream number, port downstream number (none, so -1), 
% BARs if there are some, and ACS configuration.
endpoint, 4, 3, -1, 3, 0, 0, 0xf1000000..0xf1003fff, 0,0,0,0,
endpoint, 6, 5, -1, 4, 0, 0, 0xf1004000..0xf1005fff, 0,0,0,0
]).
