
:-dynamic slist/1.  									%this is the predicate for the list of synonyms

node2:-
	consult('platform.pl'),								%this consults the Tartarus platform file, similar to consulting the file in the swi prolog window by File -> consult
	start_tartarus(localhost,8888,20),					%instantiates a tartarus platform with port number 8888 and the IP as localhost and the token being 20
	writeln('This is the node2 of the network'),
	retractall(slist(_)),								%clears the slist predicate 
	assert(slist([])),									%asserts the slist predicate with an empty list
	slist(BList),										%the asserted value is extracted in the variable BList
	Wlist=[(intelligence,[intellect,intel,wisdom,wit]),(kind,[considerate,benevolent,gracious])],	%Wlist variable is assigned with the words and the synonyms
	append(BList,Wlist,AList),							%in the Alist the words are asserted  - this appends the BList and Wlist in the variable AList
	retractall(slist(_)),								%past values are cleared from the slist predicate
	assert(slist(AList)),								%slist predicate is assigned with the Alist variable which has the synonyms of the words
	writeln('The Synonym list is ':AList),!.