%this is an example for post agent - this is node2 where the message is received

%this is to initiate the platform
node2:- 
	consult('platform.pl'),							%this consults the Tartarus platform file, similar to consulting the file in the swi prolog window by File -> consult
	start_tartarus(localhost,8888,10), 				%10 is the token for the platform
	writeln('This is the node2 of the network').
													%this is the handler where the posted message from node1 is received
hulk(_,(localhost,9999),W):-    					%the variable W holds the message posted from the source
	writeln('I have received message from localhost 9999 ':W).

hulk(_,(localhost,55555),W):-    					%the variable W holds the message posted from the source
	writeln('Received message from localhost 55555 ':W).

hulk1(_,(localhost,9999),X):-    					%the variable W holds the message posted from the source
	writeln('this is hulk1 from localhost 9999 ':X).