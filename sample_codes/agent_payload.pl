:-dynamic number/2,hammer/3.  											%here number is the name of the payload which the agent will carry and the "hammer" is the name of the handler associated with the agent 

																		%starting the movement of the agent from node1. Node2 should be instantiated before running this predicate
node1:-
	consult('platform.pl'),												%this consults the Tartarus platform file, similar to consulting the file in the swi prolog window by File -> consult
	start_tartarus(localhost,70000,1111),								%instantiates a tartarus platform with port number 70000 and the IP as localhost and the token being 1111
	writeln('This is the node1 of the network'),
	create_mobile_agent(thor,(localhost,70000),hammer,[1111]),			%this creates a mobile agent with the name "thor" at the current port and IP address (70000 and localhost) with the handler as "hammer" and the tokens associated with agent being 1111					
	retractall(number(_,_)),											%this is the payload which will be added to the agent. We need to retract before asserting to clear the past values (if any)
	assert(number(guid,150)),											%asserting the payload with the value 150 (number). the first argument is a keyword "guid" and the second argument is the value of payload
	add_payload(thor,[(number,2)]),										%adding the payload "number" to the agent "thor". (number,2) indicates that number is the payload with 2 arguments
	writeln('Agent is moving to node 70001'),
	move_agent(thor,(localhost,70001)).									% here the agent is moved to the node2 whose ip is localhost and port is 70001		

%handler for the agent
hammer(guid,(_,_),main):-      											%Note that in the second argument, (_,_) the underscore is an anonymous variable. This is used when you do not need to use the variables for any purpose
	writeln('I am agent from the other node'),
	number(guid,A),														%the payload number which the agent carried is extracted in the variable A
	writeln('Number which I have carried is ':A).