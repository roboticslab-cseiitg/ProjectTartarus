% THIS IS A NAIVE PROGRAM TO DEMONSTRATE THE ABILITY OF TARTARUS TO CLONE AGENTS (EVEN REMOTELY)
% AN AGENT KEEPS SHUTTLING FROM ONE NODE TO ANOTHER IN A 2-NODE NETWORK
% EVERY TIME IT JUMPS THE ENERGY CONFERRED TO IT INITIALLY DECREMENTS
% FINALLY WHEN ITS ENERGY BECOMES ZERO, IT MAKES A CLONE OF ITSELF IN THE OTHER NODE
:-dynamic energy/2,thanos_task/3.

node1:-	
	consult('platform.pl'),
	start_tartarus(localhost,50000,1111),
	verbose(0),
	writeln('BTW: This is Node 1').

agent_init:-
	create_mobile_agent(thanos,(localhost,50000),thanos_task,[1111]),     % Creates an agent named "thanos" in localhost at port# 50000 with handler thanos_task.Provides platform token 1111 too.
	retractall(energy(_,_)), 											  % Retract any clauses for energy (just in case) 	
	assert(energy(guid,5)),												  % and initialize value of energy as 5.
	retractall(cloned(_,_)),
	assert(cloned(guid,0)),												  % 0 indicates the agent has not yet been cloned
	%add_token(thanos,[1111]),
	add_payload(thanos,[(energy,2),(cloned,2)]),						  % Adds the predicates (& thus facts or programs) for energy and cloned, to the agent
	move_agent(thanos,(localhost,50001)).								  % Make thanos move to the other node at Port# 50001

thanos_task(guid,(localhost,Port),main):-								  % Agent Thanos' task handler: Describes what Thanos needs to do when it enters a node
	writeln('Thanos' :guid:' has arrived here ':Port),nl,
	(Port=50000, NextNode is 50001;										  % Point the next node to go to, to the other node
	Port=50001, NextNode is 50000),
	energy(guid,E),														  % Get the current energy
	writeln('Energy of the agent is ':E),nl,
	cloned(guid,X),														  % Get the no. of times the agent has been cloned
	
	(
		E=0, 
		(
		 X=0,
		 retractall(cloned(_,_)),assert(cloned(guid,1)),			  % If Energy is 0 and X=0 (i.e. agent is not cloned) then make X=1 (i,e, agent is cloned) 
		 clone_agent(guid,(localhost,NextNode),Clone_name), 			  % and create a clone of Thanos in the OTHER NODE. This shows we can make a clone remotely 
		 writeln('Thanos cloned with the name ':Clone_name:' on the node ':NextNode),nl 
		 ;
		 writeln('Thanos is already cloned'),nl
	    )							  									  % Else it means the agent has already been cloned 
		;
		 E1 is E-1,														  % Decrement energy and assert it
		 retractall(energy(guid,_)),
		 assert(energy(guid,E1)),
		 writeln('Thanos is preparing to leave this node!'),nl,
		 sleep(3),														  % Idle for 3 sec
		 move_agent(guid,(localhost,NextNode))							  % Thanos migrates to the other node
	).

