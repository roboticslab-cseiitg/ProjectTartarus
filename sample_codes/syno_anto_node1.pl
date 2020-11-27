%this is program to get the synonyms and antonyms
:-dynamic wordhandler/3,word/2,synlist/2,antlist/2.
node1:-
	consult('platform.pl'),  											%this consults the Tartarus platform file, similar to consulting the file in the swi prolog window by File -> consult
	start_tartarus(localhost,7777,20),   								%instantiates a tartarus platform with port number 7777 and the IP as localhost and the token being 20
	writeln('This is the node1 of the network').

agent_init:-
	create_mobile_agent(syn_agent,(localhost,7777),wordhandler,[20]),	%this creates a mobile agent with the name "syn_agent" at the current port and IP address (7777 and localhost) with the handler as "wordhandler" and the tokens associated with agent being 20
	writeln('Enter the word whose synonyms and antonyms are to be fetched'),
	read(Word),															%the input word is read from the user and assigned to variable Word
	writeln('The word read is ':Word),
	retractall(word(_,_)),												%the "word" (notice the case) is a dynamic predicate, is cleared to clear all the past values if any
	assert(word(guid,Word)),											%it assigns payload "word" with the value read from the user in the variable Word
	retractall(synlist(_,_)),											%synlist is the predicate which will collect the synonyms of the word entered and here it is cleared of any past values
	assert(synlist(guid,[])),											%synlist is initialised to an empty list
	retractall(antlist(_,_)),											%antlist is the predicate which will collect the antonyms of the word entered and here it is cleared of any past values
	assert(antlist(guid,[])),											%antlist is initialised to an empty list
	add_payload(syn_agent,[(word,2),(synlist,2),(antlist,2)]),    		%all the three payloads word,synlist and antlist is added to the agent 
	move_agent(syn_agent,(localhost,8888)).								%agent syn_agent is moved to the port 8888 


wordhandler(guid,(_,Port),main):-
	writeln('The agent has landed at ':Port),
	(
		(	Port=8888,														% if the port the agent has landed on is 8888
			word(guid,Word),    											% here the word is extracted into the variable Word 
			NextNode is 9999,   											% next node which the agent visits from this node is 9999 and is assigned to the variable 9999
			slist(WList),													% the synonym list is extracted to WList variable of all words
			nth0(_,WList,(Word,Slist)),										%in the variable Slist, the synonyms of the word in the variable Word is extracted.
			writeln('The list of the synonyms is ':Slist),
			retractall(synlist(guid,_)),									%the synlist is cleared of the old value which was an empty list. Note that the first argument guid is not cleared. guid indicates the agent name, hence this retractall only clears the value associated with this agent
			assert(synlist(guid,Slist)),									%the extracted synonym list of the Word is asserted in the payload. Note tthat this is not added to the agent again, because it is already added in the agent 
			move_agent(guid,(localhost,NextNode))  							%now the agent is moved to the next node 9999
		)
		;
		(	Port=9999,															%if the port is 9999 then the following statements are executed
			word(guid,Word),												%the word is extracted in the variable Word
			NextNode is 7777,												%next node where the agent should travel to is the node 1 with the port 7777
			alist(WList),													%in the variable WList all the antonyms of all the words
			nth0(_,WList,(Word,Aflist)),      								%in the variable Aflist, the antonyms of the Word are extracted
			writeln('The list of the antonyms is ':Aflist),
			retractall(antlist(guid,_)),									%the antlist predicate is cleared before asserting the antonyms extracted
			assert(antlist(guid,Aflist)),									%the antonyms extracted are asserted in the payload and since the payload is already added to the agent
			move_agent(guid,(localhost,NextNode))  							%here the agent is moved to the next node which is 7777
		);
			synlist(guid,S), 													%here the agent is back to the source node and the synonyms are extracted in the variable S
			antlist(guid,A),  													%here the antonyms are extracted to the variable A	
			word(guid,W),	  													%the word is extracted in the variable W				
			writeln('Entered word is ':W),
			writeln('List of synonyms is ':S),
			writeln('List of antonyms is ':A)
	),
	!.

