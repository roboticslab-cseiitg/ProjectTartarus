tart_start:- 
	writeln('************************************************************'),
	ansi_format([bold,fg(blue)],'                               Tartarus has been setup! ~w.',[.]),nl,nl,
	ansi_format([bold,fg(blue)],'                Proceed to launch Tartarus using: start_tartarus/2~w',[.]),nl,nl,
	ansi_format([fg(red)],'Note: Relevant manuals are available inside the User Manuals folder~w',[.]),nl,
	ansi_format([fg(red)],'           of the installation directory of Tartarus.~w',[.]),nl,
	writeln('************************************************************'),
	consult('platform.pl'),
	consult('nxt_interface.pl').
	
