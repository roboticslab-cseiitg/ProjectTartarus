:-use_module(library(socket)).


prolog_server_test(Port, Options) :-
	consult('C:\\Tartarus\\platform.pl'),
	platform_start(localhost,8989),
	set_log_server(localhost,8989),
 	tcp_socket(ServerSocket),
  	tcp_setopt(ServerSocket, reuseaddr),
  	tcp_bind(ServerSocket, Port),
  	tcp_listen(ServerSocket, 5),
  	thread_create(server_loop(ServerSocket, Options), _,
  		      [ alias(prolog_server)
  		      ]).
  
  server_loop(ServerSocket, Options) :-
  	tcp_accept(ServerSocket, Slave, Peer),
  	tcp_open_socket(Slave, InStream, OutStream),
  	
  	tcp_host_to_address(Host, Peer),
  	(   Postfix = []
  	;   between(2, 1000, Num),
  	    Postfix = [-, Num]
  	),
  	atomic_list_concat(['client@', Host | Postfix], Alias),
  	catch(thread_create(
  		  service_client(InStream, OutStream, Slave, Options),
  		  _,
  		  [ alias(Alias),attach_console
  		  ]),
 	      error(permission_error(create, thread, Alias), _),
 	      fail), !,
 	server_loop(ServerSocket, Options).
 

  service_client(InStream, OutStream, Slave, _):-
 	thread_self(Id),
	readWord(InStream,Cmd),
	%term_to_Atom(D,Cmd),
	%atom_string(D,String),
	
	open('server_log.txt',append,X,[alias(input)]),
	current_output(Y),
	set_output(X),
	write(Cmd),
	close(X),	
 	format(OutStream, '~p', [Cmd]),
 	close(InStream),
 	close(OutStream),
 	thread_detach(Id).

	
readWord(InStream,W) :-
        get0(InStream,Char),
        checkCharAndReadRest(Char,Chars,InStream),
        atom_chars(W,Chars).
 
checkCharAndReadRest(10,[],_) :- !.  % Return
checkCharAndReadRest(32,[],_) :- !.  % Space
checkCharAndReadRest(-1,[],_) :- !.  % End of Stream
checkCharAndReadRest(end_of_file,[],_) :- !.
checkCharAndReadRest(Char,[Char|Chars],InStream) :-
        get0(InStream,NextChar),
        checkCharAndReadRest(NextChar,Chars,InStream).
 
 
  
