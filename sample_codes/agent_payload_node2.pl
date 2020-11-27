%setting up a destination node
node2:-
	consult('platform.pl'),						%this consults the Tartarus platform file, similar to consulting the file in the swi prolog window by File -> consult
 	start_tartarus(localhost,70001,1111),       %instantiates a tartarus platform with port number 70001 and the IP as localhost
	writeln('This is the node2 of the network').