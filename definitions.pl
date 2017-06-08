:- use_module(library(clpfd)).
%%%%%%%%%%%%%%%% Global parameters definitions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Throughout the code, port is used to mean physical PCIe port.
% Ports are designated by non-negative integers, which refer to a physical reality that 
% has NOTHING to do with any other number assigned via configuration.
% The physical electric reality of the PCIe network studied is translated
% using this port number and the notion of up_link and down_link (see below).
%%%%
% arbitrary maximal number of ports in a fabric : 
maxport(10).
% physical ports : 
portname(Port) :- maxport(M), Port in 0..M.

% Addresses treated in the PCIe network (usually between 0 and 2^32-1) : 
addr_min(0).
addr_max(0xfffffffff).

address_val(Address) :- addr_min(X), addr_max(Y), Address #>=X, Address #=<Y.

% BDF describe the Bus, Device and Function number assigned to
% a function in a device. Every port has (at least) one.
% bus (8 bits)
maxbusnum(256).
is_bus_num(X) :- maxbusnum(A), X in 0..A.
% device (5 bits)
maxdevnum(32).
is_dev_num(X) :- maxdevnum(A), X in 0..A.
% function (3 bits)
maxfunnum(8).
is_fun_num(X) :- maxfunnum(A), X in 0..A.

is_bdf(bdf(B,D,F)) :- is_bus_num(B), is_dev_num(D), is_fun_num(F).

% Predicate diff(B,D,F,B1,D1,F1) holds when 
% B,D,F does not define the same BDF as B1,D1,F1.
diff_bdf(B,_,_,B1,_,_) :- (B #\= B1).
diff_bdf(_,D,_,_,D1,_) :- (D #\= D1).
diff_bdf(_,_,F,_,_,F1) :- (F #\= F1).


is_bool(0).
is_bool(1).

is_tag(Tag) :- Tag in 0..15.

is_AT(AT) :- AT in 0..1.

%%%%
% event predicates
%%%%

mem_read(Address, BDF, Tag, AT) :- is_bdf(BDF), is_tag(Tag), address_val(Address), is_AT(AT).
mem_write(Address, BDF, AT) :- is_bdf(BDF), address_val(Address), is_AT(AT).
% The AT bit is not conserved in completion predicates because no test is ever 
% performed on it.
completion(ReqID, ComplID, Tag) :- is_bdf(ReqID), is_bdf(ComplID), is_tag(Tag).

% non-TLP events
gen_read(Address) :- address_val(Address).
gen_write(Address) :- address_val(Address).
accept(compl,Address) :- address_val(Address).
accept(write,Address) :- address_val(Address).
