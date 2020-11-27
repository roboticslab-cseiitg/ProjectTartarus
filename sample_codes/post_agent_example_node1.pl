%this is an example for post agent - post the message from node1 to node2
%first run node2_start from the file post_agent_example_node2.pl and
% then run node1_start from the current file

node1:-
	consult('platform.pl'),													%this consults the Tartarus platform file, similar to consulting the file in the swi prolog window by File -> consult
	start_tartarus(localhost,9999,10), 										%9999 is the port and 10 is the token
	writeln('This is the node1 of the network').

post_msg:-
	post_agent(platform,(localhost,8888),[hulk,_,(localhost,9999),"Hello"]),   %here the last argument is the message to be posted to the destination with IP localhost and the port 8888
	writeln("Message posted to the destination").

another_msg:-
	A=['say','green'],
	post_agent(platform,(localhost,8888),[hulk1,_,(localhost,9999),A]),   %here the last argument is the message to be posted to the destination with IP localhost and the port 8888
	writeln("Message posted to the destination").

/*
hulk(_,(localhost,9999),W):-    					%the variable W holds the message posted from the source
	writeln('I have received message from localhost 9999 ':W).

hulk(_,(localhost,55555),W):-    					%the variable W holds the message posted from the source
	writeln('Received message from localhost 55555 ':W).

hulk1(_,(localhost,9999),W):-    					%the variable W holds the message posted from the source
	writeln('this is hulk1 from localhost 9999 ':W).*/



