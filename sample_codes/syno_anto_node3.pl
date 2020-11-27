:-dynamic alist/1.									%this is the predicate for the list of antonyms

node3:-
	consult('platform.pl'),							%this consults the Tartarus platform file, similar to consulting the file in the swi prolog window by File -> consult
	start_tartarus(localhost,9999,20),				%instantiates a tartarus platform with port number 9999 and the IP as localhost and the token being 20
	writeln('This is the node3 of the network'),
	retractall(alist(_)),							%clears the alist predicate 
	assert(alist([])),								%asserts the alist predicate with an empty list
	alist(BList),									%the asserted value is extracted in the variable BList
	Wlist=[(intelligence,[stupidity,foolishness,ignorance]),(kind,[inconsiderate,cruel,harsh])],		%Wlist variable is assigned with the words and the antonyms
	append(BList,Wlist,AList),						%in the Alist the words (along with antonyms) are asserted - this appends the BList and Wlist in the variable AList
	retractall(alist(_)),							%past values are cleared from the alist predicate
	assert(alist(AList)),							%alist predicate is assigned with the Alist variable which has the antonyms of the words
	writeln('The Antonym list is ':AList),!.