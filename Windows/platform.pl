
  
/*
Copyright (C) 2017  Robotics Lab., Dept. of Computer Science and Engg., Indian Institute of Technology Guwahati, India
    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.
    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
    
 */
% <copyright>Indian Institute of Technology Guwahati Assam, India 781039</copyright>
% <author>Vivek Singh, Manoj Bode, Tushar Semwal</author>
% <email>manojbode@gmail.com</email>
% <email>semwaltushar@gmail.com</email>
% <date>28-01-2017</date>
% <summary>Tartarus: A Multi-Agent Platform</summary>
% From 2019 being maintained and developed by: Menaxi Bagchi, Suraj Pandey and Divya D. Kulkarni
%---------------------------------------------------------------------------

%--------------------------instructions-----------------------------
% + means input parameter
% - means output parameter
% ? means it could be input or output
% # means optional parameter
%------------------------------------------------------

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(library(socket)).                                 %% using TCP/IP connection


% store the socket id of platform
:-dynamic platform_socket_Tartarus_IITG/1.
:-dynamic create_static_agent/4.
:-dynamic platform_state/1.
:-dynamic clean_static_agent/1.
:-dynamic server_Ip/1.
:-dynamic server_Port/1.
:-dynamic server_Ip1/1.
:-dynamic server_Port1/1.
:-dynamic server_Ip2/1.
:-dynamic server_Port2/1.
:-dynamic temp_list3/2.
:-dynamic temp_list2/2.
:-dynamic tt/1,subt/1,sysbit/1.
:-dynamic verbose1/1.
:-dynamic in_hop_times_info/1.
:-dynamic out_hop_times_info/1.
:-dynamic length1/1. 
:-dynamic in_hop_time/2.
:-dynamic out_hop_time/2.
:-dynamic out_hop_times_details/1. 
:-dynamic out_hop_time/5.
:-dynamic date1/1.
:-dynamic day/1.
:-dynamic month/1.
:-dynamic year/1.
:-dynamic time_info/1.
:-dynamic hour/1.
:-dynamic min/1.
:-dynamic in_hop_times_details/1.
:-dynamic length2/1.
:-dynamic length3/1.
:-dynamic length5/1. 
:-dynamic in_hop_time/5.
:-dynamic min_val_in/1.
:-dynamic max_val_in/1.
:-dynamic min_val_out/1.
:-dynamic max_val_out/1.
:-dynamic in_hop_time_stat/3.
:-dynamic out_hop_time_stat/3.
:-dynamic incoming_hoptime_stat/2.
:-dynamic outgoing_hoptime_stat/2.
:-dynamic overall_hop_time_stat/3.
:-dynamic minimum1/1.
:-dynamic maximum1/1.
:-dynamic inc_agent/1.
:-dynamic inc_date/1.
:-dynamic inc_stime/1.
:-dynamic inc_htime/1.
:-dynamic inc_sip/1.
:-dynamic inc_sp/1.
:-dynamic inc_dip/1.
:-dynamic inc_dp/1.
:-dynamic inc_agent1/1.
:-dynamic inc_htime1/1.

verbose1(0).
out_hop_times_details([]).
in_hop_times_details([]).
out_hop_times_info([]).
in_hop_times_info([]).
min_val_in(1.0Inf).
max_val_in(-1.0Inf).
min_val_out(1.0Inf).
max_val_out(-1.0Inf).
incoming_hoptime_stat(0,0).
outgoing_hoptime_stat(0,0).
in_hop_time_stat(0,0,0).
out_hop_time_stat(0,0,0).
overall_hop_time_stat(0,0,0).

% default values of Ip and port
platform_socket_Tartarus_IITG(_).

% fact which identifies the acknowledge received from other platforms
% command_acknowledge_Tartarus_IITG have 1 variable which can be used to identify the sender information
command_acknowledge_Tartarus_IITG(_).

%-----------------------------------------Platform-----------------------------------------------

% platform is created at input Ip address and port number it removes port number already in use
% and removes old ip address in system asserts new ip address and port number
% creates a global queue to store the messages received from other platforms or itself
% creates a mutex for sending messages in atomic way

%-------------------------------------------------------------------------------------------------


make_platform_Tartarus_IITG(Ip,Port) :-                                         %% create platform at +port +Ip
   (platform_state(on)->                                                        %% To verify
        nl,write('            Platform already created           '),nl
        ;
        create_socket_Tartarus_IITG(Port, Socket),                              %% create socket
        retractall(platform_socket_Tartarus_IITG(_)),                           %% retract all values of platform socket id
        assert(platform_socket_Tartarus_IITG(Socket)),                          %% assert new value of socket id
        tcp_setopt(Socket,reuseaddr),                                           %% enable tcp to reuse same address, seting option for reusblility
        retractall(platform_port(_)),                                           %% retract old port numbers
        retractall(platform_Ip(_)),                                             %% retract old ip address of platform
        assert(platform_port(Port)),                                            %% assert new port number of platform
        assert(platform_Ip(Ip)),                                                %% assert new ip address of platform
        catch((
        message_queue_create(clean_platform),
        message_queue_create(global_queue),                                     %% global queue to store received messages from other platforms
        message_queue_create(client_queue),                                     %% created a 'client_queue' to store all client connection
        message_queue_create(clean_agent_queue),
        mutex_create(message_queue_mutex),                                      %% mutex to handle global_queue operation
        mutex_create(send_message_mutex)),                                      %% mutex for handling message sending function
        Error,writeln(Error)),
        catch(thread_create(agent_clean(platform,(Ip,Port)),Cleanup_id, [detached(false)]),Error2,writeln(Error2)),
        catch(thread_create(worker_Tartarus_IITG(client_queue,Port), Worker_id,[detached(false)]),Error,writeln(Error)),                %% workers process each message independently
        catch(thread_create(acceptor_Tartarus_IITG(Socket, client_queue),Acceptor_id, [detached(false)]),Error2,writeln(Error2)),!,     %% acceptor_Tartarus_IITG thread to wait for incomming connections
        asserta(guid_threadid(t_clean,Cleanup_id)),
        asserta(guid_threadid(t_acceptor,Acceptor_id)),
        asserta(guid_threadid(t_worker,Worker_id)),
        retractall(platform_state(_)),
        assert(platform_state(on)),
      	writeln('****************************************************************'),
	ansi_format([bold,fg(green)],'        Welcome to Tartarus Multi-Agent Platfo~w',[rm]),nl,
	ansi_format([bold,fg(green)],'            (Ver.2020.1.1, 15th September 2020~w',[')']),nl,nl,
	ansi_format([fg(blue)],'                 Copyrights 2017: Robotics Lab~w',[.]),nl,
	ansi_format([fg(blue)],'               Dept. of Computer Science & Engg~w',[.]),nl,
	ansi_format([fg(blue)],'            Indian Institute of Technology G~w',[uwahati]),nl,nl,
	ansi_format([fg(blue)],'     (For any query contact: tartarus.iitg@gmail.com~w',[')']),nl, 
	%ansi_format([fg(blue)],'tartarus.iitg@gmail.~w',[com]),nl, 
	%ansi_format([fg(blue)],'semwaltushar@gmail.~w',[com]),nl, 
	%ansi_format([fg(blue)],'sbnair.tartarus@gmail.~w',[com]),nl,
	%ansi_format([fg(red)],'Note: Relevant manuals are available inside the User Manuals folder~w',[.]),nl,
	%ansi_format([fg(red)],'           of the installation directory of Tartarus.~w',[.]),nl,
        writeln('****************************************************************'),
        nl,
        write('=========================================='),nl,
        write('                   Tartarus Platform launched!'),nl,
        %write('                                @             '),nl,
        write('                   IP = '),write(Ip),write(', '),write('Port = '),write(Port),nl,
        write('=========================================='),nl,
        nl).


make_platform_Tartarus_IITG(_,_):- print_message(error,'make_platform_Tartarus_IITG(IP,Port) failed'),fail,!.


% setup_Tartarus_IITG/0 sets up the dynamic clauses used later on
% 1. database, execs are from old version of platform. Do not pay much attention to them
% 2. platform_port holds the port on which the platform is running. Form : agent_port(port)
% 3. agent_GUID holds the identifier, handler predicate and port information about all the agents running on the current platform. Form : agent_GUID(id,handler_predicate,Port)
% 4. agent_payload holds the name of the predicates added to the agent as payload. Form : agent_payload(id, [(predicate1,arity1),(predicate2,arity2),...])
% 5. outgoing_agent holds the information about the outgoing agent (moving outside of the platform). The information is identifier
%    and the timestamp. This is later used for calculating hop times for outgoing agents. Form : outgoing_agent(id, Top, Res). Take a look at time/2 in WIN_REF.pdf
% 6. hoptime_stat holds the hop time information. Holds the statistics.
% 7. agent_token holds the list of token a agent has. Form : agent_token(id,[token list])
% 8. transit_GUID, transit_payload, transit_token and transit_code are same as agent_GUID,agent_payload .. respectively.
%    When agents are moving, then transit predicates are used to hold information till we are sure that the agent has reached its destination.

setup_Tartarus_IITG:-
        (dynamic (database/1)),
        (dynamic (platform_port/1)),
        (dynamic (platform_Ip/1)),
        (dynamic (execs/1)),
        (dynamic (agent_GUID/3)),
        (dynamic (agent_payload/2)),
        (dynamic (outgoing_agent/3)),
        (dynamic (hoptime_stat/4)),
        (dynamic (agent_token/2)),
        (dynamic (platform_token/1)),
        (dynamic (transit_GUID/3)),
        (dynamic (transit_payload/2)),
        (dynamic (transit_token/2)),
        (dynamic (transit_code/2)),
        (dynamic (transfer_queue/1)),
        (dynamic (transit_agent/2)),
        (dynamic (start_agent/3)),
        (dynamic (code_base/2)),
        (dynamic (guid_threadid/2)),
        (dynamic (static_agent_details/5)).


%-----------------------------------------------------------------------

% Initialize the dynamic facts.

do:-
        assert(database(assert((execs(X):-dynamic X)))).

start_db:-
        setup_Tartarus_IITG,do.

%-----------------------------------------------------------------------

% start_tartarus/3 starts the agent platform at a port which can be specified by the user. It creates a platform agent, writes some output for
% the user. It also hides some of the code, so that only the interface predicates are visible to the user and not the entire code. This is achieved by
% hide_mycode/0. Hoptime statistics are set to 0, and port number is also stored.It calls the predicate make_platform_Tartarus_IITG for platform creation

:-dynamic start_tartarus/2.
:-dynamic start_tartarus/3.

start_tartarus(P,Token):-
		start_tartarus(localhost,P,Token).


espeak_present_bit_versions:-																			%by Suraj Kumar Pandey
		retractall(sysbit(_)),
		exists_file("C:\\Program Files (x86)\\eSpeak\\command_line\\espeak.exe")->asserta(sysbit(64));
		exists_file("C:\\Program Files\\eSpeak\\command_line\\espeak.exe")->asserta(sysbit(32));
		false.

espeak_present:-
		
		retractall(tt(_)),retractall(subt(_)),					
		espeak_present_bit_versions-> 
		asserta(subt(0)),
		asserta(tt(1));
		asserta(subt(0)),
		asserta(tt(0)).

start_tartarus(Ip,Port,Token):-																		
		check_syntax(start_tartarus,_Ip,Port,Token),       
		start_db,
		make_platform_Tartarus_IITG(Ip,Port),nl,
		retractall(hoptime_stat(_,_,_,_)),
		assert(hoptime_stat(0,0,0,0)),
		retractall(transfer_queue(_)),
		assert(transfer_queue([])),ttyflush,
		set_token(Token),espeak_present,!.		
		
		

start_tartarus(_Ip,_Port,_Token):-
        print_message(error,'start_tartarus(Ip,Port,Token) failed, Please enter valid IP address and unused Port number'),fail.
			

% ----------------------------------------------------------------------			
% close_tartarus/0 closes the platform agent, retracts the port information, kills all the agent that are running and retracts all the code
% previously loaded by the platform using platform_assert_file. That code is stored in code_base.

close_tartarus :-
        platform_state(on),
        forall(agent_GUID(GUID,_,_),agent_kill(GUID)),
        forall(code_base(_,FileCodeList),retract_code_Tartarus_IITG(FileCodeList)),
        retractall(code_base(_,_)),
        close_stream_pool,
        catch(forall(guid_threadid(_,ThreadID),(
                (thread_property(ThreadID,status(S)),S=running)->
                        thread_signal(ThreadID,thread_exit(_)),thread_join(ThreadID,_);
                        (retractall(guid_threadid(_,ThreadID)))
                )
        ),Error,writeln(Error)),

        catch(forall(static_agent_details(Port,_,_,_,_),clean_static_agent(Port)),Error,writeln(Error)),

        retractall(guid_threadid(_,_)),
        platform_socket_Tartarus_IITG(Socket),
        retractall(platform_socket_Tartarus_IITG(Socket)),                      %% retract current socket
        tcp_close_socket(Socket),
        %write('socket is ':Socket),
        message_queue_destroy(global_queue),
        message_queue_destroy(client_queue),
        message_queue_destroy(clean_platform),
        message_queue_destroy(clean_agent_queue),
        mutex_destroy(message_queue_mutex),
        mutex_destroy(send_message_mutex),
        retractall(platform_port(_)),
        retractall(platform_Ip(_)),
        retractall(platform_state(_)),
        writeln('Platform has been closed.'),nl,ttyflush,!.

close_tartarus:-
        print_message(warning,'close_tartarus/0 failed!@! Check if Tartarus platform is already running.').

%----------------------------------------------------------------------

%%INTERFACE FUNCTION%%
% reset_tartarus/0 closes and starts the platform at the same port
:- dynamic reset_tartarus/0.

reset_tartarus:-
        platform_reset.
        
reset_tartarus:-
        print_message(warning,'reset_tartarus/0 failed!').
        

platform_reset:-
        handler(platform,(_,_),platform_restart).

platform_reset:-
        print_message(warning,'platform_reset failed!').

%% Gives current platform IP address,port number and the type of OS
get_tartarus_details(IP,Port):-
        get_platform_details(IP,Port).

%get_tartarus_details(_IP,_Port):-
        %print_message(warning,'get_tartarus_details/2 failed!').
        
get_platform_details(Ip,Port):-
        check_syntax(get_tartarus_details,Ip,Port),
        platform_Ip(Ip),
        platform_port(Port).

/*get_platform_details(Ip,Port,OS):-
    check_syntax(get_tartarus_details,Ip,Port,OS),                % added by Menaxi J Bagchi %
	platform_Ip(Ip),
    platform_port(Port),
   (
    current_prolog_flag(unix,true),OS = linux; (current_prolog_flag(windows,true),OS = windows;current_prolog_flag(apple,true),OS = apple)
   ).*/


%----------------------------------------------------------------------

% set_token/1 sets the token of the current platform. Incoming agent having this token is allowed entry, others are denied entry. This
% implements a security feature of the platform. However this is not complete and is just a prototype. *Need to finish security module*

set_token(Token):-
        retractall(platform_token(_)),assert(platform_token(Token)).

%----------------------------------------------------------------------

% platform_assert_file loads the content of the file into Prolog. Using consult etc are the same thing, but we need to keep track of
% the code that has been asserted. Hence this function has been made. It uses file_to_list_Tartarus_IITG/2, assert_code_Tartarus_IITG/1 which are predicates defined
% below in utilities. Its asserts a mapping of the code vs FILE to keep track of this code. This is used later for saving platform state.

assert_file_to_tartarus(FILE):-
        check_syntax(assert_file_to_tartarus,FILE),   %added by Menaxi J Bagchi
        platform_assert_file(FILE).
        
assert_file_to_tartarus(_):-
        print_message(warning,'assert_file_to_tartarus/1 (FILE) failed!').


platform_assert_file(FILE):-
        file_to_list_Tartarus_IITG(FILE,L),
        assert_code_Tartarus_IITG(L),
        assert(code_base(FILE,L)).

platform_assert_file(_FILE):-
        print_message(warning,'platform_assert_file(FILE) failed').

%-----------------------------------------------------------------------


%==================================================%
%                Creating socket                   %
%==================================================%

create_socket_Tartarus_IITG(Port, Socket) :-                            %% create socket at port +Port -Socket
   tcp_socket(Socket),                                                  %% get socket value from tcp_socket inbuilt function
   catch((
   tcp_bind(Socket, Port),                                              %% bind socket to given port number
   tcp_listen(Socket, 5)),                                              %% listen to Socket provided by socket
   Error,(tcp_close_socket(Socket),writeln(Error),fail)),!.

%---------------------------------------------------

acceptor_Tartarus_IITG(Socket, Queue) :-                                %% predicate that will wait for client to connect +Socket +Queue
   tcp_accept(Socket, Client,From),                                     %% accept the incomming -Client and Ip of connection -From is ip of incomming connection
        thread_send_message(Queue, connection([Client,From])),          %% send the incomming connection properties to +Queue as 'connection' variable name
        !,acceptor_Tartarus_IITG(Socket, Queue).                        %% again wait for other connection after sending previous connection.


%---------------------------------------------------

% worker_Tartarus_IITG is main scheduler , the commands posted on connection is handled in FCFS basis

worker_Tartarus_IITG(Queue,Port):-                                      %% Read connection one by one from queue +Queue and process their messages as input
   thread_get_message(Queue, connection([Client,From])),                %% get top connection from +queue

   tcp_open_socket(Client, In, Out),                                    %% bind socket with +client variable to get streams +In +Out
   %format('getting client connection ~q',[From,Client]),nl,            %% Display msg
   read(In, Command),
      %% Read -Command from other platform from +In stream
   % thread_join(Thread,Status),                                        %% check the -status of thread
   % Status,
   [_,Agent_name|_] = Command,
   Functor =.. Command,
   (agent_GUID(Agent_name,_,(_,Port))->
        %catch(thread_create(execute_msg_Tartarus_IITG(Functor,Out,From),Thread,[]),Error,writeln(Error))
        
        execute_msg_Tartarus_IITG(Functor,Out,From)
        ;
        ((Agent_name = platform; Agent_name= server; Agent_name=serverr; Agent_name=serverrr)->
                %catch(thread_create(execute_msg_Tartarus_IITG(Functor,Out,From),Thread,[]),Error,writeln(Error));
                execute_msg_Tartarus_IITG(Functor,Out,From);
                format(Out,'~q.~n',[fail]),
                flush_output(Out))),
   close(In),                                                           %% close input buffer of socket
   close(Out),                                                          %% close ouput buffer of socket
   !,worker_Tartarus_IITG(Queue,Port).                                  %% process other message


%===========================================================%
%             Execute or process message received           %
%===========================================================%

% Call the command as message and send acknowledge to sender platform as command_acknowledge_Tartarus_IITG function
% if message is end_of_file in case of error still send acknowledge

execute_msg_Tartarus_IITG(Command,Out,From) :-                                          %% execute the message as command +Command +Out +Fro
   %join_threads,
   (Command = end_of_file->
        (true,format(Out,'command_acknowledge_Tartarus_IITG(~q).~n',[From]));           %% sending acknowledge in case of eof
        catch(thread_create(call(Command),_,[detached(true)]),Error,writeln(Error)),    %% else create independent thread to process the +command/message
        format(Out,'command_acknowledge_Tartarus_IITG(~q).~n',[From])                   %% send acknowledgment after thread creation
   ),
   %writeln('Ack sent successfully'),                                                   %% Display
   true,!.


execute_msg_Tartarus_IITG(_Command,_Out,_From) :-
        print_message(warning,'execute_msg_Tartarus_IITG(Command,Out,From) failed'),!.

% -------------------Message Sending Module---------------------

% Send messages to other platform/Agents as agent_post predicate we have to provide
% receiver agent name , its ip address , its port number , sender agent name , function/command
% here function is command/handlers of receiver agent which it have to execute
% Every time a post agent fails, an error thrown is caught in a variable
:- dynamic post_agent/3.
:-dynamic error_cnt/1.
:-dynamic conn_refused_er/1.

error_cnt(0).

post_agent(platform,(Ip_receiver,Port_receiver),FunctionList):-
        agent_post(platform,(Ip_receiver,Port_receiver),FunctionList).

agent_post(platform,(Ip_receiver,Port_receiver),FunctionList):-                         %% +Receiver_Agent +Ip_receiver +Port_receiver +Sender_agent +Function
        %repeat,                                                                                                                                                                %% Keep sending message until -Ack is received from other side
        catch(send_message(Ip_receiver,Port_receiver,FunctionList,Ack),Error,(writeln(Error),mutex_unlock(send_message_mutex),fail)),   %% if present then send message
        (Ack->                                                                                                                                                          %% -Ack is acknowlegde reciveied from other side should be true
                true;                                                                                                                                                   %% ack is command_acknowledge_Tartarus_IITG(_) predicate which is being received as true
                (sleep(1),writeln('Ack' :Ack),fail)
                %true
        ),!.


agent_post(_Platform,(_Ip_receiver,_Port_receiver),_FunctionList):-
        %print_message(error,'post_agent(platform,(Ip_receiver,Port_receiver),FunctionList) failed'),
        catch((throw('error(socket_error(Connection refused),_)')),C,(retractall(conn_refused_er(_)), assert(conn_refused_er(C)), error_cnt(E), retractall(error_cnt(_)), E1 is E + 1, assert(error_cnt(E1)))),		   %added by Menaxi J Bagchi
		fail,!.  

%--------------------------------------------------

% Independent predicate to send message without Sender agent name or port etc

send_message(Host, Port,Message,Ack) :-                                 %% Send Message to +Host/IP +Port +Message -Ack
        mutex_lock(send_message_mutex),                                 %% Hold the lock while sending message
        setup_call_catcher_cleanup(tcp_socket(Socket),                  %% Get free -socket to send//recieve
        tcp_connect(Socket, Host:Port),                                 %% Bind the socket to +Ip and +Port
        exception(_),                                                   %% Exception value is unused
        tcp_close_socket(Socket)),                                      %% Close Socket
        setup_call_cleanup(tcp_open_socket(Socket, In, Out),
        chat_to_server_Tartarus_IITG(In, Out,Message,Ack),              %% call predicate to send message//command
        close_connection_Tartarus_IITG(In, Out)),                       %% after sending message close In Out stream
        mutex_unlock(send_message_mutex).                               %% release mutex to be used by other functions

        
%--------------------------------------------------

close_connection_Tartarus_IITG(In, Out) :-                              %% close the streams input and output at client +In +Out
        close(In, [force(true)]),
        close(Out, [force(true)]),!.


%-------------------------------------------------

chat_to_server_Tartarus_IITG(In, Out,Message,Ack) :-                    %% send messages to server with in out stream +In +Out +Message -Ack
        Term = Message,                                                 %% Assign +Message to term variable
        (Term = end_of_file                                             %% If term = end_of_file then return
                -> true
        ;
                format(Out,'~q.~n',[Term]),                             %% if message is not null then write to +Out stream the +term and append new line character in the end
                flush_output(Out),
                read(In,Reply),                                         %% wait to get the reply from other side i.e command_acknowledge_Tartarus_IITG(_) as message
                (call(Reply)                                            %% call command_acknowledge_Tartarus_IITG(_) predicate
                        ->Ack = true;                                   %% if ack if valid the Ack is true else fail is returned
                        Ack = fail
                )
                %writeln('got message successfully')                    %% Display
        ),!.

chat_to_server_Tartarus_IITG(_In, _Out,_Message,_Ack) :-
        print_message(warning,'chat_to_server_Tartarus_IITG(In, Out,Message,Ack) failed'),!.

combine_Tartarus_IITG(Agent_name,Ip,Port,Agent_new_name):-              %% predicate to combine_Tartarus_IITG +Agent_name , +Ip and +Port to get -Agent_new_name
        atom_concat(Agent_name,'_',A1),                                 %% combine agent_name with '-'
        atom_concat(A1,Ip,A2),                                          %% combine above result with Ip
        atom_concat(A2,'_',A3),                                         %% combine above result with '-'
        atom_concat(A3,Port,Agent_new_name).                            %% combine above result with Port number


%-----------------------------------------------------------

get_new_name(Agent_name,Agent_new_name):-
        gensym(Agent_name,Agent_name2),                                 %% generate a unique -agent_name2 from +agent_name
        platform_port(Port),                                            %% check if the port is same as plaform port
        platform_Ip(Ip),                                                %% check if the ip of platform is same as platform ip
        combine_Tartarus_IITG(Agent_name2,Ip,Port,Agent_new_name),!.    %% generate new agent name by combining with its ip and port

get_new_name(_Agent_name,_Agent_new_name):-
        print_message(warning,'get_new_name(Agent_name,Agent_new_name) failed'),!.


%-----------------------------------------------------------

get_new_name_alpha(Agent_name):-
        check_syntax(get_new_name_alpha,Agent_name),     % added by Menaxi J Bagchi
        X1 is (random(26) + 97),
        X2 is (random(26) + 97),
        X3 is (random(26) + 97),
        X4 is (random(26) + 97),
        X5 is (random(26) + 97),
        X6 is (random(26) + 97),
        atom_codes(Agent_name,[X1,X2,X3,X4,X5,X6]).

%---------------------------------------------

%======================================================%
%=                  Static agent                      =%
%======================================================%


  
create_static_agent(Agent_name,(Platform_Ip,Port),Handler,List):- 
        check_syntax(create_static_agent,_Agent_name,_Platform_Ip,Port,Handler,List),      %added by Menaxi J Bagchi%
		check_handler(Handler,3),                                                          % added by Menaxi J Bagchi%
        platform_state(on),
        platform_Ip(Platform_Ip),
        %platform_port(Platform_Port),
        create_socket_Tartarus_IITG(Port, Socket),
        atom_concat('client_queue_', Port, ClientQueue),
        message_queue_create(ClientQueue),
        catch(thread_create(worker_Tartarus_IITG(ClientQueue, Port), WorkerTr,[detached(false)]),Error,writeln(Error)),         %% workers process each message independently
        catch(thread_create(acceptor_Tartarus_IITG(Socket, ClientQueue),AccepterTr, [detached(false)]),Error2,writeln(Error2)), %% acceptor_Tartarus_IITG thread to wait for incomming connections
        (var(Agent_name)->
                get_new_name_alpha(Agent_name);
                true),
        ((atom(Agent_name),atom(Handler))->nothing;abort),
        (thread_peek_message(clean_agent_queue,agent_guid(Agent_name))->
                cleanup_wait(Agent_name);
                %handler(platform,(Platform_Ip,Platform_Port),clean_agent(Agent_name));
                true),
        (agent_GUID(Agent_name,_,(Platform_Ip,_))->
                agent_kill(Agent_name);
                true),
        genhandler(Agent_name,Handler,Result),
        assert_code_Tartarus_IITG(Result),
        assert(agent_GUID(Agent_name,Handler,(Platform_Ip,Port))),
        assert(agent_payload(Agent_name,[])),
        assert(agent_token(Agent_name,[])),
        assert(static_agent_details(Port, ClientQueue, Socket, WorkerTr, AccepterTr)),!,
		add_token(Agent_name,List),!.
        %format(atom(Str),'Static agent with name ~w created',[Agent_name]),
        %nl,writeln(Str),!.
        
clean_static_agent(Port):-
        retract(static_agent_details(Port, ClientQueue, Socket, WorkerTr, AccepterTr)),
        catch(((thread_property(WorkerTr,status(S)),S=running)->
                thread_signal(WorkerTr,thread_exit(_));
                writeln(WorkerTr))
                ,Error,writeln(Error)),
        catch(((thread_property(AccepterTr,status(S)),S=running)->
                thread_signal(AccepterTr,thread_exit(_));
                writeln(AccepterTr)),
                Error,writeln(Error)),
        tcp_close_socket(Socket),
        message_queue_destroy(ClientQueue).
		



%--------------default handler list------------------------------------

answer(Reciver_Agent_name,Sender_Agent_name,Data):-                                             %% send answer from other agents to this agent
        mutex_lock(message_queue_mutex),                                                        %% hold the global_queue mutex
        thread_send_message(global_queue,answer(Reciver_Agent_name,Sender_Agent_name,Data)),    %% send 'answer' variable in global_queue with arguments as Agent_name_sender,Data
        mutex_unlock(message_queue_mutex),!.                                                    %% unlock the global_queue mutex

answer(_Reciver_Agent_name,_Sender_Agent_name,_Data):-
        print_message(warning,'answer(Reciver_Agent_name,Sender_Agent_name,Data) failed'),!.

%-----------------------------------------

% Any agent can get answer or wait for answer at platform by calling this method, data is the value sent by other agent.
% The answer is stored in global answer queue from where it is extracted

get_answer(Receiver_name,Sender_name,Data):-                                    %% get the answer sent by sender agent to self agent
        repeat,                                                                                                         %% loop untill answer is found
        writeln('Waiting for answer...'),
        (thread_peek_message(global_queue,answer(Receiver_name,Sender_name,Data))->
                true;
                sleep(1),
                fail
        ),
        mutex_lock(message_queue_mutex),
        thread_get_message(global_queue,answer(Receiver_name,Sender_name,Data)),
        mutex_unlock(message_queue_mutex),ttyflush,!.

get_answer(_Receiver_name,_Sender_name,_Data):-
        print_message(warning,'get_answer(Receiver_name,Sender_name,Data) failed'),!.


:-dynamic temp_list4/1.
temp_list4([]).

add_element4(H,Old,[H|Old]).

list_code4(H):-
        forall(
        clause(H,Code),
        (
        term_to_atom(H,H2),
        term_to_atom(Code,Code2),
        atom_concat(H2,':-',H3),
        atom_concat(H3,Code2,Code3),
        atom_to_term(Code3,Code4,_),
        temp_list4(UserHandlerCode),
        add_element4(Code4,UserHandlerCode,Result),
        retractall(temp_list4(_)),
        assert(temp_list4(Result))
        )
        ).

clauses(Head,List):-
        (list_code4(Head),temp_list4(List),retractall(temp_list4(_)),assert(temp_list4([]))).

%----------------------------------------------------------------------------

member(H,[H|_]).
member(H,[_|T]):- member(H,T).

%-------------------------------------------

% save_tartarus_state/1 saves all the code that has been loaded previously using platform_assert_file and
% the agent code(its handler code and payload code) into a file. Its a kind of local backup. Can be loaded back into the system
% The interface function loads up an event for the platform. The platform handler then handles the event. For the sake of readability, I have written
% the handler code here instead of writing it later. {PS : the position of the code does not matter.}
% Why event based? Because when you are saving a state, you open a file and push stuff into it. The problem occurs when other event occurs and interrupt.
% These events may issue 'write' calls, which would go into our open stream. But having the entire process in a event handling (which is not interruptable)
% gives you a sense of 'mutual exclusion'.


% Intially it looks up at code_base to find out all the generic code, saves it to the file. Then for all the agents, captures the code
% using assimilate_code and saves it. At the last, it stores the meta info such as what agent is linked to what code etc which it uses
% later on to resume the state. Ex : agent_payload contains the names of all predicates linked to each agent. This needs to be fed back
% when we resume from the 'saved state'.
save_tartarus_state(FileName):-
        save_platform_state(FileName).
        
save_tartarus_state(_FileName):-
        print_message(warning,'save_tartarus_state/1(FileName) failed!').
        
save_platform_state(FileName):-
        (atom(FileName)->nothing;abort),
        platform_Ip(Ip),
        platform_port(Port),
        agent_post(platform,(Ip,Port),[handler,platform,_,save_state(FileName)]),!.

save_platform_state(_FileName):-
        print_message(warning,'save_platform_state(FileName) failed').

%------------------------------------------------------------------------------------------------

% load_tartarus_state/1 loads the state back. It loads the predicates, loads the agent code(their handler and payload) and starts the agents.
% Basically, read the file, assert everything, and using the meta information, start the agents at their respective ports. Load the meta information
% as well
load_tartarus_state(FileName):-
        load_platform_state(FileName).

load_tartarus_state(_):-
        print_message(warning,'load_tartarus_state/1(FileName) failed!').
        
load_platform_state(FileName):-
        (atom(FileName)->nothing;abort),
        platform_Ip(Ip),
        platform_port(Port),
        agent_post(platform,(Ip,Port),[handler,platform,_,load_state(FileName)]),!.

load_platform_state(_FileName):-
        print_message(warning,'load_platform_state(FileName) failed').


%==================================================== Agent predicates ===============================================================

% agent_create/3 creates a agent. It uses agent_create/3 of Chimera to create an agent. A unique 8character GUID is generated using gensym
% and handler code for the agent is generated and asserted. Assume that abc123 is the name of the agent, then agent_create(abc123,user_handler,Port) is
% invoked.
% The handler code provided by user is ' user_handler(guid,Link,Event):- .. '. Instead a handler code of the form ' user_handler(abc123,Link,Event):- ..'
% is asserted where abc123 is the actual name of the agent. So when the Event is posted on the agent, then since
% its name is abc123, only that agent carries the task and no other. Refer to 'how it works.pdf'

%======================================================================================================================================

cleanup_wait(Agent_name):-
        (thread_peek_message(clean_agent_queue,agent_guid(Agent_name))->
                sleep(1),
                !,cleanup_wait(Agent_name);
                true).

%========================================================%
%                 Create mobile agent                    %
%========================================================%

%agent_create/2 --> 
%Date:06 Nov. 2016
%Added by Tushar: Usually mobile agents are created on current platform and hence unnecessary insertion of IP and Port

:-dynamic create_mobile_agent/3.
:-dynamic create_mobile_agent/4.

create_mobile_agent(GUID,Handler,List):- check_syntax(create_mobile_agent,_GUID,Handler,List),                              %added by Menaxi J Bagchi% 
										 check_handler(Handler,3),                                                         %added by Menaxi J Bagchi% 
                                         agent_create(GUID,Handler),
                                         add_token(GUID,List).

create_mobile_agent(GUID,(Ip,Port),Handler,List):- 
             check_syntax(create_mobile_agent,_GUID,_Ip,Port,Handler,List),  %added by Menaxi J Bagchi
			 check_handler(Handler,3),                                       % added by Menaxi J Bagchi%
             agent_create(GUID,(Ip,Port),Handler),
			 add_token(GUID,List).

:-dynamic agent_create/2.

agent_create(GUID,Handler):-
		get_platform_details(Ip,Port),
		agent_create(GUID,(Ip,Port),Handler).

agent_create(GUID2,(Ip,Port),Handler):-
        verbose1(Il30),
        ( Il30=:=1, 
        platform_state(on),
        (var(GUID2)->get_new_name_alpha(GUID2);true),
        (platform_Ip(Ip)->
                (platform_port(Port)->
                        true;
                        fail)
                ;
                get_ip(Ip1),
                ((Ip1=Ip), (platform_port(Port))-> 
                        true;
                        ((Ip = localhost), (platform_port(Port))-> 
                                true;
                        fail))),        
        ((atom(GUID2),atom(Handler))->nothing;abort),
        (thread_peek_message(clean_agent_queue,agent_guid(GUID2))->
         %clean_agent(GUID2);
        %handler(platform,(Ip,Port),clean_agent(GUID2));
                cleanup_wait(GUID2);
                true
        ),
        (agent_GUID(GUID2,_,(Ip,_))-> 
                        agent_kill(GUID2), 
                        is_exist(GUID2), 
        true),
        genhandler(GUID2,Handler,Result),
        assert_code_Tartarus_IITG(Result),
        assert(agent_GUID(GUID2,Handler,(Ip,Port))),
        assert(agent_payload(GUID2,[])),
        assert(agent_token(GUID2,[])),
        format(atom(Str),'Agent with name ~w created',[GUID2]),
        nl,writeln(Str),!;
        
        platform_state(on),
        (var(GUID2)->get_new_name_alpha(GUID2);true),
        (platform_Ip(Ip)->
                (platform_port(Port)->
                        true;
                        fail)
                ;
                get_ip(Ip1),
                ((Ip1=Ip), (platform_port(Port))->
                        true;
                        ((Ip = localhost), (platform_port(Port))->
                                true;
                        fail))),        
        ((atom(GUID2),atom(Handler))->nothing;abort),
        (thread_peek_message(clean_agent_queue,agent_guid(GUID2))->
         %clean_agent(GUID2);
        %handler(platform,(Ip,Port),clean_agent(GUID2));
                cleanup_wait(GUID2);
                true
        ),
        (agent_GUID(GUID2,_,(Ip,_))->
                        agent_kill(GUID2),
                        is_exist(GUID2);
        true),
        genhandler(GUID2,Handler,Result),
        assert_code_Tartarus_IITG(Result),
        assert(agent_GUID(GUID2,Handler,(Ip,Port))),
        assert(agent_payload(GUID2,[])),
        assert(agent_token(GUID2,[])),
        %format(atom(Str),'Agent with name ~w created',[GUID2]),
        %nl,writeln(Str),
        !).



agent_create(GUID,(Ip_other_end,Port_other_end),Handler):-
        platform_state(on),
        nl,write('Creating agent on platform with IP '),write(Ip_other_end),write(' and port number '),write(Port_other_end),nl,
        agent_post(platform,(Ip_other_end,Port_other_end),[agent_create,GUID,(Ip_other_end,Port_other_end),Handler]).


agent_create(_GUID2,(_Ip,_Port),_Handler):-
        print_message(warning,'agent_create(GUID2,(Ip,Port),Handler) failed'),fail.
		

%========================================================%
%                 Execute/run agent                      %
%========================================================%
%added by tushar
%27 Jan 2016

:-dynamic execute_agent/3.
:-dynamic execute_agent/4.

execute_agent(Agent_name,(Ip,Port),Handler):- 
                         check_syntax(execute_agent3,Agent_name,_Ip,Port,Handler),   %added by Menaxi J Bagchi
                         agent_execute(Agent_name,(Ip,Port),Handler). % -- adding a simple name --%

agent_execute(Agent_name,(Ip,Port),Handler):-                                           %% create agent +Agent_name +Handler +Ip +Port
        (agent_GUID(Agent_name,Handler,(Ip,Port))->
				Functor =.. [Handler,Agent_name,(Ip,Port),main],                        %% create Functor with handler as predicate name, and arguments as +Agent_new_name, +Ip, +Port
				%writeln(Functor),
                catch(((guid_threadid(Agent_name,X),integer(X),thread_property(X,status(Y)),Y=running)->
                        guid_threadid(Agent_name,ThID),
                        retractall(guid_threadid(Agent_name,X)),
                        thread_signal(X,thread_exit(true)),
                        thread_join(ThID,_),
                        writeln('old agent thread stopped')
                        ;
                        (guid_threadid(Agent_name,ThID)-> 
                                retractall(guid_threadid(Agent_name,_)),
                                thread_join(ThID,_);
                                true)
                        ),
                        Error,writeln(Error)),
				
				catch(thread_create(call(Functor),ThreadID,[]),Error2,writeln(Error2)), %% call the main handler of agent which will start execution
				catch(thread_signal(ThreadID,sleep(0)),Error3,writeln(Error3)),
                asserta(guid_threadid(Agent_name,ThreadID)),
				
				ttyflush,!;
                print_message(error,'Either agent name provided does not exist or check port number and handler name.'),!).

agent_execute(_Agent_name,(_Ip,_Port),_Handler):-
        print_message(warning,'execute_agent(Agent_name,(Ip,Port),Handler) failed!'),!.


execute_agent(Agent_name,(Ip,Port),Handler,Start_function):- 
                                  check_syntax(execute_agent4,Agent_name,_Ip,Port,Handler,Start_function),  %added by Menaxi J Bagchi
                                  agent_execute(Agent_name,(Ip,Port),Handler,Start_function). % -- adding a simple name --%

agent_execute(Agent_name,(Ip,Port),Handler,Start_function):-                    %% create agent +Agent_name +Handler +Ip +Port
        %writeln('-------agent_execute--------'),
        Functor =.. [Handler,Agent_name,(Ip,Port),Start_function],              %% create Functor with handler as predicate name, and arguments as +Agent_new_name, +Ip, +Port
		%writeln(Functor),
        catch(((guid_threadid(Agent_name,X),integer(X),thread_property(X,status(Y)),Y=running)->
                (retractall(guid_threadid(Agent_name,X)),thread_signal(X,thread_exit(_)),writeln('old agent thread stopped'));
                (retractall(guid_threadid(Agent_name,_)))),
                Error,writeln(Error)),
        catch(thread_create(call(Functor),ThreadID,[]),Error2,writeln(Error2)), %% call the main handler of agent which will start execution
        catch(thread_signal(ThreadID,sleep(0.03)),Error3,writeln(Error3)),
        asserta(guid_threadid(Agent_name,ThreadID)),
        ttyflush,!;
		print_message(error,'Either agent name provided does not exist or check port number and handler name.'),!.                                                             %% Display

agent_execute(_Agent_name,(_Ip,_Port),_Handler,_Start_function):-
        print_message(warning,'execute_agent(Agent_name,(Ip,Port),Handler,Start_function) failed!'),!.



%========================================================%
%                    Agent post                          %
%========================================================%

agent_post(GUID,Event):-
        agent_GUID(GUID,_,(Ip,Port)),
        agent_post(platform,(Ip,Port),Event).


%=======================================================%
%                   Agent kill                          %
%=======================================================%

% agent_kill closes the agent using agent_close/3 of Chimera, retracts all the relevent code and deletes the meta
% informations. assimilate_code/2 is used for gathering all the code, retract_code_Tartarus_IITG/1 retracts it. Other calls are mainly
% to remove agent meta info about payloads and tokens.


%added by tushar
%27 Jan 2016

:-dynamic purge_agent/1. % -- a sober naming convention (less gore than agent_kill/1!!) 

purge_agent(Guid):- agent_kill(Guid).



agent_kill(GUID):-
        platform_Ip(Ip),
        platform_port(Port),
        (agent_GUID(GUID,_,(Ip,UsedPort))->
                catch(thread_create(agent_kill_platform(platform,GUID),_,[detached(true)]),Error2,(writeln(Error2),fail)),
                (UsedPort=Port->
                        true;
                        clean_static_agent(UsedPort));
                true 
				%writeln('Agent with the name '),write(GUID),write(' does not exist.')    
        ),!.

agent_kill(_GUID):-
        print_message(warning,'agent_kill(GUID) failed').
		%writeln('Agent with the name '),write(GUID),write(' does not exist.').

%--------------------------------------------------------------------------------------------

agent_kill_platform(platform,GUID):-
        %writeln('--------killing agent start----------'),
        (atom(GUID)->nothing;abort),
        catch((guid_threadid(GUID,X)->
                        ((integer(X),thread_property(X,status(Y)),Y=running)->
                                (retractall(guid_threadid(GUID,_)),thread_signal(X,thread_exit(true)),thread_join(X,_));
                                (retractall(guid_threadid(GUID,_)),thread_join(X,_)));
                                true),
                Error,(write(Error),fail)),
        assimilate_code(GUID,AgentCode),
        retractall(agent_GUID(GUID,_Handler,(_Ip,_Port))),
        retractall(agent_payload(GUID,_PayloadList)),
        retractall(agent_token(GUID,_ListOfTokens)),
        retract_code_Tartarus_IITG(AgentCode),
        %writeln('--------killing agent end----------'),
        ttyflush,
        !.

agent_kill_platform(platform, _GUID):-
        print_message(error, 'Agent kill failed').
        
%=============================================================================================%
%                                   Agent save                                                %
%                                                                                             %
% agent_save/2 writes the agent information into a file. This is for possible usage later on. %
% assmilate_code/2 for capturing the code, and then writing the code into the file<SIMPLE>.   %
%=============================================================================================%
:- dynamic save_agent/2.

save_agent(GUID,FileName):-
        check_syntax(save_agent,GUID,FileName),         %added by Menaxi J Bagchi
        agent_save(GUID,FileName).

agent_save(GUID,FileName):-
        ((atom(GUID),atom(FileName))->nothing;abort),
        assimilate_code(GUID,Result),
        open(FileName,write,X),
        current_output(Y),
        set_output(X),
        forall(
                (member(Code,Result)),
                (with_output_to(atom(Data),portray_clause(Code)),write(Data))
                ),
        set_output(Y),close(X),!.

agent_save(_GUID,_FileName):-
        print_message(warning,'agent_save(GUID,FileName) failed').

%-------------------------------------------------------------------------------

list([]):-false.
list([_|_]):-true.

%================================================================%
%                        Add payload                             %
%================================================================%

% add_payload /2 adds the payload to the agent. To achieve this, it creates a unique copy of the payload. The payload predicate in its
% parameter contains a term 'guid'[user_code]. Example : ability(guid,X):-print(X). So we replace the 'guid' with the actual identifier/name of the
% agent. Now comes the question why?
% Note that when I made the handler of the agent, i subsitituted all 'guid's by actual identifiers. So in the handler of a agent, if
% user writes something like this :
% myhandler(guid, Link, message(X)):-ability(guid,X).
% Then instead of this handler, something like this is created and asserted [Assume abc123 is the identifier alloted by the system]
% myhandler(abc123, Link, message(X)):- ability(abc123,X).
% So when this agent is notified of the Event 'message(X)'; then
% myhandler with abc123 as its first parameter is invoked. (DUE TO
% CHIMERA) and so when adding ability as payload I create
% ability(abc123,X):- print(X). as the payload, and assert it. THUS we
% have a unique handler code, and unique payload code, their uniqueness
% defined by identifier.

add_payload(GUID,AddList):-
        check_syntax(add_payload,GUID,AddList),                  % added by Menaxi J Bagchi
        %((atom(GUID),list(AddList))->nothing;abort),
        agent_payload(GUID,List),
        subtract(AddList,List,FinalList),
        generatePayloadCode(GUID,FinalList,PayloadCode),
        assert_code_Tartarus_IITG(PayloadCode),
        append(FinalList,List,NewList),
        retractall(agent_payload(GUID,List)),
        assert(agent_payload(GUID,NewList)),!.
		

add_payload(_GUID,_AddList):-
        print_message(warning,'add_payload(GUID,AddList) failed').
		



%================================================================%
%                        Remove payload                          %
%================================================================%

% remove_payload/2 captures the payload code that is specific to the agent. (having the identifier in defination instead of 'guid').
% After capturing the code,  it retracts it. The predicate_header here creates payload_predicate(agent_name,_,_...) sort of header which
% later is later used by clauses/2 to fetch the body of the payload. The permanent list of payload for that agent is edited finally.

remove_payload(GUID,ToRemove):-
          check_syntax(remove_payload,GUID,ToRemove),   %added by Menaxi J Bagchi
        %((atom(GUID),list(ToRemove))->nothing;abort),
        agent_payload(GUID,List),
        forall((
                 member((Predicate,Arity),ToRemove)),
                (predicate_header(GUID,Predicate,Arity,Result),
                        (clauses(Result,Body)->
                                retract_code_Tartarus_IITG(Body);
                                nothing)
                        )
                ),
        delete_list(ToRemove,List,NewList),
        retractall(agent_payload(GUID,List)),
        assert(agent_payload(GUID,NewList)),!.

remove_payload(_GUID,_ToRemove):-
        print_message(warning,'remove_payload(GUID,ToRemove) failed').

%--------------------------------------------------------------------------------

% shed_payload/2 basically removes the payload from the agent and puts it in the platform. What ? Let's consider an example
% ability(abc123,X):-write(abc123),write(X). The <- is payload attached to a agent named abc123.
% If u shed, ability; the above code is retracted. And ability(guid,X):-write(guid),write(X) is asserted back.
% This would be the original way the user might have written the ability predicate in his/her file.
% ability is now again available to be attached to some other agent.

shed_payload(GUID,ToShed):-
        ((atom(GUID),list(ToShed))->nothing;abort),
        copy_payload(GUID,ToShed),
        remove_payload(GUID,ToShed).

%--------------------------------------------------------------------------------

% copy_payload copies the payload. Does not retract it. So the tyhphlet retains its payload while platform also gains the generic defination
% of the payload. The rest of the predicates are pretty much similar and self explanatory

copy_payload(GUID,ToCopy):-
        ((atom(GUID),list(ToCopy))->nothing;abort),
        forall((
                member((Predicate,Arity),ToCopy)),
                (
                        generate_generic_predicate(GUID,Predicate,Arity,Result),       %% Make generic defination
                        assert_code_Tartarus_IITG(Result)
                )
                ),!.
%-------------------------------------------------------------------------------

removeall_payload(GUID):-
        (atom(GUID)->nothing;abort),
        agent_payload(GUID,List),
        remove_payload(GUID,List).

%--------------------------------------------------------------------------------

shedall_payload(GUID):-
        (atom(GUID)->nothing;abort),
        agent_payload(GUID,List),
        shed_payload(GUID,List).                        % !!!!!!!!!!!!!!!!!!!! CHECK if defination already exists ?

%--------------------------------------------------------------------------------%

copyall_payload(GUID):-
        (atom(GUID)->nothing;abort),
        agent_payload(GUID,List),
        copy_payload(GUID,List).

%--------------------------------------------------------------------------------

%==========================================================================%
%                              Add token                                   %
%                                                                          %
% add_token adds the a token ( presented as a list [token1,token2] ) with  %
% the agent. while remove_token removes token from the list.               %
%==========================================================================%

add_token(GUID,List):-
        (list(List)->nothing;abort),
        agent_token(GUID,OldList),                              % %need to put error chceks here
        subtract(List,OldList,FinalList),                       %% Add token only if token already not added
        append(OldList,FinalList,NewList),                      %% //
        retractall(agent_token(GUID,OldList)),                  %% //
        assert(agent_token(GUID,NewList)),!.                    %% //

add_token(_GUID,_List):-
        print_message(warning,'add_token(GUID,List) failed').


%==========================================================================%
%                              Remove token                                %
%==========================================================================%

remove_token(GUID,ToRemove):-                                   %% INTERFACE FUNCTION
        (list(List)->nothing;abort),                            %//
        agent_token(GUID,List),                                 %//
        delete_list(ToRemove,List,NewList),                     %//
        retractall(agent_token(GUID,List)),                     %//
        assert(agent_token(GUID,NewList)),!.                    %//

remove_token(_GUID,_ToRemove):-
        print_message(warning,'remove_token(GUID,ToRemove) failed').


%%=====================================================MOBILITY=============================================================================%%
%                                                                                                                                            %
%                                                     Agent clone                                                                            %
%                                                                                                                                            %
% Cloning means creating a copy of the agent at a 'remote' location. Of course, that's the usual way of doing things.                        %
% {{Future}} feature note for developer : make a agent_clone/1 ..for making a clone and starting it here and not somewhere else.             %
% Would that be a better feature than this ? .. But again the same can be achieved by doing agent_clone(xyz,[]).. try this my man !          %
%                                                                                                                                            %
% agent_clone creates a new identifier first for the copy of the agent. Then all the code of agent is assimilated and all the places,        %
% where ID1 is written, it is replaced by ID2. Then you send the code to the target destination. The code includes both, Handler code,       %
% and payload code. To calculate time taken for the HOP, a timestamp is saved by time(Top,Res) and it is saved with outgoing_agent/3.        %
% Later on when the clone reaches the destination, an ack is sent which is used to calculate the time taken for hop.[more on hop time later] %
%%==========================================================================================================================================%%

:- dynamic clone_agent/3.

clone_agent(GUID1,(Send_Ip,Send_Port),GUID2):-
        check_syntax(clone_agent,GUID1,_Send_Ip,Send_Port,GUID2),     %added by Menaxi J Bagchi
        agent_clone(GUID1,(Send_Ip,Send_Port),GUID2).

agent_clone(GUID1,(Send_Ip,Send_Port),GUID2):-
        retractall(get_destination_Ip(_)), assert(get_destination_Ip(Send_Ip)),
        retractall(get_destination_port(_)), assert(get_destination_port(Send_Port)),
        %((atom(GUID1),integer(Send_Port),var(GUID2))->nothing;abort),
        gensym(GUID1,GUID2),
        get_time(TT),                                                                                                                           %%time in seconds from begining of os
        assert(outgoing_agent(GUID2,TT,_)),
        assimilate_code(GUID1,AgentCode),
        replaceGUID(AgentCode,GUID1,GUID2,CloneCode),
        agent_payload(GUID1,PayloadList),
        agent_GUID(GUID1,Handler,_),
        agent_token(GUID1,ListOfTokens),
        list_to_set(ListOfTokens,ListOfTokens2),
        get_platform_details(CloneIp,ClonePort),
        agent_post(platform,(Send_Ip,Send_Port),[handler,platform,(CloneIp,ClonePort),send(GUID2,Handler,PayloadList,CloneCode,ListOfTokens2,TT)]),!.

agent_clone(GUID1,(_Ip,_Port),_):-
        print_message(error,'Clone agent Failed for: '),write(GUID1),nl,fail.		


%=============================================================%
%                        Move agent                           %
%=============================================================%

% agent_move is just assmilating the code, and sending it to the destination. The code is retracted after sending it.
% The code however is not discarded right away. It is stored as a temporary. This is because if a negative ack comes from the recieving platform
% the code, would be activated once again, and the agent would be started again.
:- dynamic move_agent/2.
:- dynamic get_destination_Ip/1.
:-dynamic get_destination_port/1.

move_agent(GUID,(Ip_other_end,Port_other_end)):-
        check_syntax(move_agent,GUID,_Ip_other_end,Port_other_end),   %added by Menaxi J Bagchi
        agent_move(GUID,(Ip_other_end,Port_other_end)).
        
agent_move(GUID,(Ip_other_end,Port_other_end)):-
        platform_Ip(Ip1),
        get_ip(Ip),
        platform_port(Port),
		
		retractall(get_destination_Ip(_)), assert(get_destination_Ip(Ip_other_end)),
        retractall(get_destination_port(_)), assert(get_destination_port(Port_other_end)),
        
        ((Ip = Ip_other_end,Port = Port_other_end)->writeln('Agent is already on same platform.'),abort;true),
        ((Ip1 = Ip_other_end,Port = Port_other_end)->writeln('Agent is already on same platform.'),abort;true),
        ((Ip_other_end=localhost,Port = Port_other_end)->writeln('Agent is already on same platform.'),abort;true),
        
        %((atom(GUID),integer(Port_other_end))->nothing;abort),
        
        assimilate_code(GUID,AgentCode),
        agent_payload(GUID,PayloadList),
        agent_GUID(GUID,Handler,(Ip1,Port)),
        agent_token(GUID,ListOfTokens),
        list_to_set(ListOfTokens,ListOfTokens2),
        get_time(TT),
        assertz(outgoing_agent(GUID,TT,_)),
        (agent_post(platform,(Ip_other_end,Port_other_end),[handler,platform,(Ip1,Port),send(GUID,Handler,PayloadList,AgentCode,ListOfTokens2,TT)])->
                (thread_peek_message(clean_agent_queue,agent_guid(GUID))->thread_get_message(clean_agent_queue,agent_guid(GUID));true),
                thread_send_message(clean_agent_queue,agent_guid(GUID)),
                thread_send_message(clean_platform,agent_guid(GUID))
                %Functor=.. [handler,platform,(Ip,Port),clean_agent(GUID)],
                %catch(thread_create(call(Functor),_,[detached(true)]),Error,writeln(Error))
                ;
                retractall(outgoing_agent(GUID,_,_))),!.
        %agent_post(platform,(Ip,Port),[handler,platform,(Ip,Port),clean_agent(GUID)]).


agent_move(GUID,(_Ip,_Port)):- print_message(error,'Move agent Failed for ':GUID),nl.
                               %print_message(error,'Please enter a valid destination token for the moving agent.'),nl.



%==========================================================%
%                      Agent list                          %
%==========================================================%

:- dynamic list_agents/0.


list_agents:-
        agent_list.
        
agent_list:-
        writeln('================Agent list==============='),
        forall(agent_GUID(GUID,Handler,(_IP,Port)),(
        write('Agent Name ':GUID),write(', handler name':Handler),write(', residing on port':Port),nl)),
        writeln('=========================================').
 
:- dynamic list_agents/1.

list_agents(R):-
        agent_list(R).
 
agent_list(ResultList):-
        get_time(TT),
        assert(temp_agent_list(TT,[])),
        forall(agent_GUID(GUID,Handler,(_IP,Port)),(
        retract(temp_agent_list(TT,List)),
        NewList = [(GUID,Handler,Port)|List],
        assert(temp_agent_list(TT,NewList)))),
        retract(temp_agent_list(TT,ResultList)).


:- dynamic isexist_agent/3.

isexist_agent(GUID,(IP,Port),Handler):-
        agent_isexist(GUID,(IP,Port),Handler).


agent_isexist(GUID,(IP,Port),Handler):-
        (agent_GUID(GUID,Handler,(IP,Port))->
                %write('Agent ':GUID),write(' with handler':Handler),write(' exists on port ':Port)
                true;fail).
				%write(GUID),write(' does not exist.'),nl,fail).           %added by Menaxi J Bagchi


%Added by Tushar 07 NOV 2015
%agent_list_new/1 returns list of GUIDs of currently present agents on a running platform
:-dynamic agent_list_new/1.
agent_list_new(AgentList):-
                        get_time(TT),
        assert(temp_agent_list(TT,[])),
        forall(agent_GUID(GUID,_,(_IP,_)),(
        retract(temp_agent_list(TT,List)),
        NewList = [GUID|List],
        assert(temp_agent_list(TT,NewList)))),
        retract(temp_agent_list(TT,AgentList)).
                        

%===========================================================================%
%                New Clean code with separate thread                        %
%===========================================================================%           
                
agent_clean(platform,(Ip,Port)):-
        ttyflush,
        %writeln('-----------cleaning start----------':GUID),
        thread_get_message(clean_platform, agent_guid(GUID)),
        (thread_peek_message(clean_agent_queue,agent_guid(GUID))->
                (catch((guid_threadid(GUID,X)->
                        ((integer(X),thread_property(X,status(Y)),Y=running)->
                                (thread_signal(X,thread_exit(true)),retractall(guid_threadid(GUID,X)),thread_join(X,_));
                                (retractall(guid_threadid(GUID,_)),thread_join(X,_)))
                                                                ;
                                                                true
                ),Error,(write('Clean_error: '),writeln(Error))),
                agent_GUID(GUID,Handler,(Ip,Port)),
                assimilate_code(GUID,AgentCode),
                agent_payload(GUID,PayloadList),
                agent_token(GUID,ListOfTokens),

                assert(transit_GUID(GUID,Handler,(Ip,Port))),
                assert(transit_payload(GUID,PayloadList)),
                list_to_set(ListOfTokens,ListOfTokens2),
                assert(transit_token(GUID,ListOfTokens2)),
                assert(transit_code(GUID,AgentCode)),
                transit_GUID(GUID,_H,_P),

                retractall(agent_GUID(GUID,Handler,(Ip,Port))),
                retractall(agent_payload(GUID,PayloadList)),
                retractall(agent_token(GUID,ListOfTokens)),
                retract_code_Tartarus_IITG(AgentCode),
                %write('Assembled transit code for '),write(GUID),
                (thread_peek_message(clean_agent_queue,agent_guid(GUID))->thread_get_message(clean_agent_queue,agent_guid(GUID));true),
                %write('Transit code is asserted for '),write(GUID),nl,
                !;
                print_message(error,'agent_clean(platform,(Ip,Port)) failed'));
                print_message(warning,'Cleaning already done')),
                %join_threads,
                %writeln('-----------cleaning end-----------'),ttyflush,
                !,agent_clean(platform,(Ip,Port)).              
                
                
is_exist(Agent_name):-
        (agent_GUID(Agent_name,_Handler,(_Ip,_Port))->
                        !,is_exist(Agent_name);
                        format(atom(Str),'Old agent with name ~q is killed',[Agent_name]),
                        writeln(Str)).
        
                
%================================================HANDLER CODE FOR PLATFORM==================================================


 
handler(platform,(_,_),platform_restart):- 
        platform_state(on),
        platform_Ip(Ip),
        platform_port(Port),
        forall(agent_GUID(GUID,_,_),agent_kill(GUID)),
        forall(code_base(_,FileCodeList),retract_code_Tartarus_IITG(FileCodeList)),
        retractall(code_base(_,_)),
        close_stream_pool,
        catch(forall(guid_threadid(_,ThreadID),(
                (thread_property(ThreadID,status(S)),S=running)->
                        thread_signal(ThreadID,thread_exit(_)),thread_join(ThreadID,_);
                        (retractall(guid_threadid(_,ThreadID)))
                )
        ),Error,writeln(Error)),

        catch(forall(static_agent_details(Port,_,_,_,_),clean_static_agent(Port)),Error,writeln(Error)),

        retractall(guid_threadid(_,_)),
        platform_socket_Tartarus_IITG(Socket),
        retractall(platform_socket_Tartarus_IITG(Socket)),                      %% retract current socket
        tcp_close_socket(Socket),
        message_queue_destroy(global_queue),
        message_queue_destroy(client_queue),
        message_queue_destroy(clean_platform),
        message_queue_destroy(clean_agent_queue),
        mutex_destroy(message_queue_mutex),
        mutex_destroy(send_message_mutex),
        retractall(platform_port(_)),
        retractall(platform_Ip(_)),
        retractall(platform_state(_)),
        start_db,
        writeln('-------restarting platform in 2 seconds------'),
        sleep(2),                                               %% wait for 2 second for other threads to fail/complete
        make_platform_Tartarus_IITG(Ip,Port),nl,
        writeln('-------platform restarted sucessfully-------'),
        retractall(hoptime_stat(_,_,_,_)),
        assert(hoptime_stat(0,0,0,0)),
        retractall(transfer_queue(_)),
        assert(transfer_queue([])),ttyflush,!.
        
        
        %------------------------------------------------------------------------------------

handler(platform,(_,_),platform_restart(New_ip,New_port)):- 
        platform_state(on),
        %platform_Ip(Ip),
        %platform_port(Port),
        forall(agent_GUID(GUID,_,_),agent_kill(GUID)),
        forall(code_base(_,FileCodeList),retract_code_Tartarus_IITG(FileCodeList)),
        retractall(code_base(_,_)),
        close_stream_pool,
        catch(forall(guid_threadid(_,ThreadID),(
                (thread_property(ThreadID,status(S)),S=running)->
                        thread_signal(ThreadID,thread_exit(_)),thread_join(ThreadID,_);
                        (retractall(guid_threadid(_,ThreadID)))
                )
        ),Error,writeln(Error)),

        catch(forall(static_agent_details(Port,_,_,_,_),clean_static_agent(Port)),Error,writeln(Error)),

        retractall(guid_threadid(_,_)),
        platform_socket_Tartarus_IITG(Socket),
        retractall(platform_socket_Tartarus_IITG(Socket)),                      %% retract current socket
        tcp_close_socket(Socket),
        message_queue_destroy(global_queue),
        message_queue_destroy(client_queue),
        message_queue_destroy(clean_platform),
        message_queue_destroy(clean_agent_queue),
        mutex_destroy(message_queue_mutex),
        mutex_destroy(send_message_mutex),
        retractall(platform_port(_)),
        retractall(platform_Ip(_)),
        retractall(platform_state(_)),
        start_db,
        writeln('-------restarting platform in 2 second on new ip port------':New_ip:New_port),
        sleep(2),
        make_platform_Tartarus_IITG(New_ip,New_port),
        writeln('-------platform restarted sucessfully on new ip port-------':New_ip:New_port),
        retractall(hoptime_stat(_,_,_,_)),
        assert(hoptime_stat(0,0,0,0)),
        retractall(transfer_queue(_)),
        assert(transfer_queue([])),ttyflush,!.
        
        

%-----------------------------Agent Create Module----------------------------------------

handler(platform,(_,_),handler_file):-                                  %% enter the handler file name for agent definition and its handler
        writeln('Enter the file of handler'),                           %% promt to get file name
        read(File_name),                                                %% read in filename
        consult(File_name),                                             %% consult predicates in file
        writeln('handler asserted successfully').                       %% Display

%----------------------------------------------

handler(platform,(_,_),handler_file(File_name)):-                       %% enter the handler file name for agent definition and its handler
        consult(File_name),                                             %% consult predicates in file
        writeln('handler asserted successfully').                       %% Display

%----------------------------------------------

handler(platform,(_,_),save_state(FileName)):-
        open(FileName,write,X),
        current_output(Y),
        set_output(X),
        forall(code_base(_,FileCodeList),
                        (forall(member(FileCode,FileCodeList),
                                (with_output_to(atom(Code_write),
                                portray_clause(FileCode)),write(Code_write)))
                        )
                ),
        forall(agent_GUID(GUID,_Handler,(_Ip,_Port)),
                (assimilate_code(GUID,Result),
                        forall((member(Code,Result)),
                                (with_output_to(atom(Code_write2),portray_clause(Code)),write(Code_write2)))

                        )
                ),

        (clauses(agent_payload(_Gg,_Ll),PayloadMeta)->
                forall(member(M,PayloadMeta),(with_output_to(atom(Ss),portray_clause(M)),write(Ss)));nothing
        ),

        (forall(agent_GUID(G1,H1,P1),(with_output_to(atom(S1),portray_clause((start_agent(G1,H1,P1)))),write(S1))),
                clauses(agent_GUID(_G,_H,_P),List)->
                forall(member(L,List),(with_output_to(atom(S),portray_clause(L)),write(S)));nothing
        ),
        (clauses(code_base(_FName,_FCode),FCodeMeta),
                forall(member(FM,FCodeMeta),(with_output_to(atom(Ff),portray_clause(FM)),write(Ff)));nothing
        ),
        set_output(Y),
        close(X),!;
        print_message(warning,'handler(platform,(_,_),save_state(FileName)) failed').

%--------------------------------------------------------------------------------------------------------------------------

handler(platform,_Link,load_state(FileName)):-
        file_to_list_Tartarus_IITG(FileName,L),
        forall(
                member(Code,L),
                        (Code = (start_agent(G,H,P))-> 
                                (call(agent_execute(G,P,H)),assert(agent_GUID(G,H,P)));
                                assert(Code)
                                )
                ),!;
                print_message(warning,'handler(platform,_Link,load_state(FileName)) failed').

                
%=========================================

                
                
                
%======================================================================================================%
%                                       Clean agent                                                    %
%                                                                                                      %
% This predicate is used to clean agent with given guid, it retracts code, relase allocated resources. %
%======================================================================================================%
handler(platform,(Ip,Port),clean_agent(GUID)):-
        ttyflush,
        writeln('-----------cleaning start----------':GUID),
        mutex_lock(clean_agent_mutex),
        (thread_peek_message(clean_agent_queue,agent_guid(GUID))->
                (catch((
                        (guid_threadid(GUID,X),integer(X),thread_property(X,status(Y)),Y=running)->
                                (guid_threadid(GUID,X1),thread_signal(X1,thread_exit(true)),retractall(guid_threadid(GUID,X1)));
                                (guid_threadid(GUID,X2),retractall(guid_threadid(GUID,_)),thread_join(X2,_))
                ),Error,(write('clean_error: '),writeln(Error))),
                agent_GUID(GUID,Handler,(Ip,Port)),
                assimilate_code(GUID,AgentCode),
                agent_payload(GUID,PayloadList),
                agent_token(GUID,ListOfTokens),

                assert(transit_GUID(GUID,Handler,(Ip,Port))),
                assert(transit_payload(GUID,PayloadList)),
                list_to_set(ListOfTokens,ListOfTokens2),
                assert(transit_token(GUID,ListOfTokens2)),
                assert(transit_code(GUID,AgentCode)),
                transit_GUID(GUID,_H,_P),

                retractall(agent_GUID(GUID,Handler,(Ip,Port))),
                retractall(agent_payload(GUID,PayloadList)),
                retractall(agent_token(GUID,ListOfTokens)),
                retract_code_Tartarus_IITG(AgentCode),
                write('Assembled transit code for '),write(GUID),
                (thread_peek_message(clean_agent_queue,agent_guid(GUID))->thread_get_message(clean_agent_queue,agent_guid(GUID));true),
                write('Transit code is asserted for '),write(GUID),nl,
                !;
                print_message(error,'handler(platform,(Ip,Port),clean_agent(GUID)) failed'));
                print_message(warning,'Cleaning already done')),
                %join_threads,
                mutex_unlock(clean_agent_mutex),
                writeln('Unlocking mutex'),
                writeln('-----------cleaning end-----------'),ttyflush.


%--------------------------------------------------------------------------------------------------

% send_ackm(GUID) event is posted to a platform when a migration of agent is complete. The handler here computes the time it took for the hop
% Intially, at the time of sending the code, a timestamp was saved. Now when this ackm arrives, we check the time and find the difference. This
% is done by time/2[CHECK OUT WIN_REF.pdf for how time/2 can be used to find time difference]. After time is calculated, it is posted into the console
% and all the temporary data of the outgoing agent is removed by retracting the transit predicates.

/*handler(platform,(Ip,Port),send_ackm(GUID,_X)):-                                                                
        ttyflush,
        writeln('--------- positive ACK received----------'),
	    writeln('token matched'),
        %writeln('Got ACK from the destination for '),write(GUID),write(Ip),write(Port),
        (outgoing_agent(GUID,TT,_)-> 
        get_time(Finish_TT), 
        Time_taken is (Finish_TT - TT),                                                      		 
	    get_destination_Ip(Dest_ip), 
	    L8 = (GUID,Dest_ip,TT,Time_taken), 
		L = (GUID,Dest_ip,_,_), L3 = [(GUID,Dest_ip,_,_)], get_hoptime(L1),      %L1 will contain the empty list.
        (member(L,L1)->delete_list(L3,L1,NewList),add_tail(NewList,L8,New),retractall(get_hoptime(_)),assert(get_hoptime(New));add_tail(L1,L8,New),retractall(get_hoptime(_)),assert(get_hoptime(New))),                            		
        
        (thread_peek_message(clean_agent_queue,agent_guid(GUID))->
                %%handler(platform,(Ip,Port),clean_agent(GUID)),
                cleanup_wait(GUID);
                %writeln('Cleanning called');
                                true
        ),
        nl,write('Migration time for agent '),write(GUID),write(':'),write(Time_taken),write(' seconds'),nl;true),
        retractall(outgoing_agent(GUID,_Top,_Res)),
        % nl,
        %write('Agent Bearing '),write(GUID),
        % write(' reached destination : Time : '),
        % write(X),
        % nl,
        retractall(transit_GUID(GUID,_Handler,(Ip,Port))),
        retractall(transit_payload(GUID,_PayloadList)),
        retractall(transit_token(GUID,_ListOfTokens)),
        retractall(transit_code(GUID,_AgentCode)),
        writeln('--------positive ack processed---------'),
		ttyflush,
        !.
handler(platform,(_Ip,_Port),send_ackm(_GUID,_X)):- print_message(error,'handler(platform,(Ip,Port),send_ackm(GUID,_X)) failed'),!.
*/

handler(platform,(Ip,Port),send_ackm(GUID,_X)):-  
        verbose1(Il2), 
        (Il2=:=1,                                                           
        ttyflush,
        writeln('--------- positive ACK received----------'),
        %writeln('token matched'),
        %writeln('Got ACK from the destination for '),write(GUID),write(Ip),write(Port),

        (outgoing_agent(GUID,TT,_)-> 
        get_time(Finish_TT), 
        Time_taken is (Finish_TT - TT),  
        hoptime_rec1(Time_taken),                                                              
        get_platform_details(Source_ip,Source_port),                                                            
        get_destination_Ip(Dest_ip), 
        get_destination_port(Dest_port),
        stamp_date_time(TT, date(Ye,Mo,Da,H,Mn,_S,_Off,_TZ,_DST), 'local'), 
        retractall(day(_)), retractall(month(_)), retractall(year(_)),
        assert(day(Da)), assert(month(Mo)), assert(year(Ye)), atom_concat(Da,'/',Za), atom_concat(Za, Mo,Za1), atom_concat(Za1,'/',Ya1), 
        atom_concat(Ya1,Ye,Ya2),retractall(date1(_)), assert(date1(Ya2)),
        retractall(hour(_)), retractall(min(_)), assert(hour(H)), assert(min(Mn)), atom_concat(H,':',Aans), atom_concat(Aans, Mn , Start_time),
        retractall(time_info(_)), assert(time_info(Start_time)),
        L8 = (GUID,(Source_ip,Source_port),(Dest_ip,Dest_port),(Ya2,Start_time),Time_taken), 
        %L = (GUID,(_,_),(Dest_ip,_),(_,_),_), L3 = [(GUID,(_,_),(Dest_ip,_),(_,_),_)], 
        out_hop_times_details(L1),      %L1 will contain the empty list.
        %(member(L,L1)->delete_list(L3,L1,NewList),add_tail(NewList,L8,New),retractall(out_hop_times_details(_)),assert(out_hop_times_details(New));add_tail(L1,L8,New),retractall(out_hop_times_details(_)),assert(out_hop_times_details(New))),
        add_tail(L1,L8,New),retractall(out_hop_times_details(_)),assert(out_hop_times_details(New)),
        length(New,Len), retractall(length1(_)),assert(length1(Len)), retractall(out_hop_time(_,(_,_),(_,_),(_,_),_)),hoptime_info,                                                              
         
        L88 = (GUID,Time_taken), 
        %L82 = (GUID,_), L83 = [(GUID,_)], 
        out_hop_times_info(L81),      
        %(member(L82,L81)->delete_list(L83,L81,NewList100),add_tail(NewList100,L88,New100),retractall(out_hop_times_info(_)),assert(out_hop_times_info(New100));add_tail(L81,L88,New100),retractall(out_hop_times_info(_)),assert(out_hop_times_info(New100))),
        add_tail(L81,L88,New100),retractall(out_hop_times_info(_)),assert(out_hop_times_info(New100)),
        length(New100,Len3), retractall(length3(_)),assert(length3(Len3)), retractall(out_hop_time(_,_)),hoptime_info3,
        
        post_agent(platform,(Dest_ip,Dest_port),[predicate_tar,_Receive,(localhost,50),GUID,Source_ip,Source_port,Dest_ip,Dest_port,Ya2,Start_time,Time_taken]),
        post_agent(platform,(Dest_ip,Dest_port),[predicate_tar1,_Receive1,(localhost,60),GUID,Time_taken]),

        (thread_peek_message(clean_agent_queue,agent_guid(GUID))->
                %%handler(platform,(Ip,Port),clean_agent(GUID)),
                cleanup_wait(GUID);
                %writeln('Cleanning called');
                                true
        ),
        nl,write('Migration time for agent '),write(GUID),write(':'),write(Time_taken),write(' seconds'),nl;true),
                
        
        retractall(outgoing_agent(GUID,_Top,_Res)),
        % nl,
        %write('Agent Bearing '),write(GUID),
        % write(' reached destination : Time : '),
        % write(X),
        % nl,
        retractall(transit_GUID(GUID,_Handler,(Ip,Port))),
        retractall(transit_payload(GUID,_PayloadList)),
        retractall(transit_token(GUID,_ListOfTokens)),
        retractall(transit_code(GUID,_AgentCode)),
        writeln('--------positive ack processed---------'),
        ttyflush,
        !
        ;
        ttyflush,
        %writeln('--------- positive ACK received----------'),
        %writeln('token matched'),
        %writeln('Got ACK from the destination for '),write(GUID),write(Ip),write(Port),

        (outgoing_agent(GUID,TT,_)-> 
        get_time(Finish_TT), 
        Time_taken is (Finish_TT - TT),  
        hoptime_rec1(Time_taken),                                                              
        get_platform_details(Source_ip,Source_port),                                                            
        get_destination_Ip(Dest_ip), 
        get_destination_port(Dest_port),
        stamp_date_time(TT, date(Ye,Mo,Da,H,Mn,_S,_Off,_TZ,_DST), 'local'), 
        retractall(day(_)), retractall(month(_)), retractall(year(_)),
        assert(day(Da)), assert(month(Mo)), assert(year(Ye)), atom_concat(Da,'/',Za), atom_concat(Za, Mo,Za1), atom_concat(Za1,'/',Ya1), 
        atom_concat(Ya1,Ye,Ya2),retractall(date1(_)), assert(date1(Ya2)),
        retractall(hour(_)), retractall(min(_)), assert(hour(H)), assert(min(Mn)), atom_concat(H,':',Aans), atom_concat(Aans, Mn , Start_time),
        retractall(time_info(_)), assert(time_info(Start_time)),
        L8 = (GUID,(Source_ip,Source_port),(Dest_ip,Dest_port),(Ya2,Start_time),Time_taken), 
        %L = (GUID,(_,_),(Dest_ip,_),(_,_),_), L3 = [(GUID,(_,_),(Dest_ip,_),(_,_),_)], 
        out_hop_times_details(L1),      %L1 will contain the empty list.
        %(member(L,L1)->delete_list(L3,L1,NewList),add_tail(NewList,L8,New),retractall(out_hop_times_details(_)),assert(out_hop_times_details(New));add_tail(L1,L8,New),retractall(out_hop_times_details(_)),assert(out_hop_times_details(New))),
        add_tail(L1,L8,New),retractall(out_hop_times_details(_)),assert(out_hop_times_details(New)),
        length(New,Len), retractall(length1(_)),assert(length1(Len)), retractall(out_hop_time(_,(_,_),(_,_),(_,_),_)),hoptime_info,                                                              
         
        L88 = (GUID,Time_taken), 
        %L82 = (GUID,_), L83 = [(GUID,_)], 
        out_hop_times_info(L81),      
        %(member(L82,L81)->delete_list(L83,L81,NewList100),add_tail(NewList100,L88,New100),retractall(out_hop_times_info(_)),assert(out_hop_times_info(New100));add_tail(L81,L88,New100),retractall(out_hop_times_info(_)),assert(out_hop_times_info(New100))),
        add_tail(L81,L88,New100),retractall(out_hop_times_info(_)),assert(out_hop_times_info(New100)),
        length(New100,Len3), retractall(length3(_)),assert(length3(Len3)), retractall(out_hop_time(_,_)),hoptime_info3,
        
        post_agent(platform,(Dest_ip,Dest_port),[predicate_tar,_Receive,(localhost,50),GUID,Source_ip,Source_port,Dest_ip,Dest_port,Ya2,Start_time,Time_taken]),
        post_agent(platform,(Dest_ip,Dest_port),[predicate_tar1,_Receive1,(localhost,60),GUID,Time_taken]),

        (thread_peek_message(clean_agent_queue,agent_guid(GUID))->
                %%handler(platform,(Ip,Port),clean_agent(GUID)),
                cleanup_wait(GUID);
                %writeln('Cleanning called');
                                true
        );true),
        %nl,write('Migration time for agent '),write(GUID),write(':'),write(Time_taken),write(' seconds'),nl;true),
                
        
        retractall(outgoing_agent(GUID,_Top,_Res)),
        % nl,
        %write('Agent Bearing '),write(GUID),
        % write(' reached destination : Time : '),
        % write(X),
        % nl,
        retractall(transit_GUID(GUID,_Handler,(Ip,Port))),
        retractall(transit_payload(GUID,_PayloadList)),
        retractall(transit_token(GUID,_ListOfTokens)),
        retractall(transit_code(GUID,_AgentCode)),
        %writeln('--------positive ack processed---------'),
        ttyflush,
        !).
        

handler(platform,(_Ip,_Port),send_ackm(_GUID,_X)):- print_message(error,'handler(platform,(Ip,Port),send_ackm(GUID,_X)) failed'),!.

%------------------------------------------------------------------------------

% send_nackm(GUID) is posted to a platform, when a migration is not successful. This happens incase the agent does not carry the correct token
% required to enter the destination platform[NEGATIVE AUTHENTICATION]. By default, this comes into use when using the security feature of the platform
% The inactive code is retrieved from the transit predicates and the agent is started again.

handler(platform,(Ip,Port),send_nackm(GUID)):-
        ttyflush,
        writeln('--------negative ACK received-----------'),
        outgoing_agent(GUID,_TT,_),
        %get_platform_details(Platform_Ip,Platform_Port),
        (thread_peek_message(clean_agent_queue,agent_guid(GUID))->
                        cleanup_wait(GUID)
                        %handler(platform,(Platform_Ip,Platform_Port),clean_agent(GUID))
                        ;true
        ),
        retractall(outgoing_agent(GUID,_Top,_Res)),
        nl,
        write('Agent '),write(GUID),write(' was not allowed to enter the destination.'),
        nl,
        transit_GUID(GUID,Handler,(Ip,Port)),
        transit_payload(GUID,PayloadList),
        transit_token(GUID,ListOfTokens),
        transit_code(GUID,AgentCode),
        assert(agent_GUID(GUID,Handler,(Ip,Port))),
        assert(agent_payload(GUID,PayloadList)),
        assert(agent_token(GUID,ListOfTokens)),
        assert_code_Tartarus_IITG(AgentCode),
        platform_Ip(Ip),
        platform_port(Port),
        retractall(transit_GUID(GUID,Handler,(Ip,Port))),
        retractall(transit_payload(GUID,PayloadList)),
        retractall(transit_token(GUID,ListOfTokens)),
        retractall(transit_code(GUID,AgentCode)),
        %agent_execute(GUID,(Ip,Port),Handler),
        writeln('--------negative ack processed-----------'),ttyflush,
        !.
		
handler(platform,(_Ip,_Port),send_nackm(_GUID,_Handler,_PayloadList,_ListOfTokens,_AgentCode)):- print_message(error,'handler(platform,(Ip,Port),send_nackm(GUID)):-'),!.



%------------------------------------------------------------------------------------

% This is the main handler which handles the incoming agent. The identifier(GUID), its Handler name(handler), its code and token comes as
% parameters. In case a platform has a security token set(via set_token/1), the incoming agents token list is checked. If it contains the
% matching token, the execution proceeds, else the entire thing is discarded and a negative ackm is sent to the sending platform. In case the platform
% does not have a security token set, then no checking is done. A postive ackm is send and the agent  is started using agent_create/3.

handler( platform, (Ip_sender,Port_sender), send(GUID,Handler,PayloadList,[Code|CodeList],ListOfTokens,_BackTime)):-                            %//
        ttyflush,
        %write('Receiving an Agent Posted by link'),writeln(Ip_sender),writeln(Port_sender),writeln(BackTime),nl,
        (platform_token(_)->
                nothing;
                agent_post(platform,(Ip_sender,Port_sender),[handler,platform,(Ip_sender,Port_sender),send_nackm(GUID)]),fail
        ),
        (platform_token(PToken)->
                (member(PToken,ListOfTokens)->
                        nothing;
                        (agent_post(platform,(Ip_sender,Port_sender),[handler,platform,(Ip_sender,Port_sender),send_nackm(GUID)]),fail)
                );
                nothing
        ),
        (agent_GUID(GUID,Handler,(_,_))->   % Tushar - added to kill agent already present with same name and handler name
                writeln(' '),
                agent_kill(GUID)
                ;
                nothing
        ),
        write(' '),
        write(' '),nl,
        get_platform_details(Platform_Ip,Platform_Port),
        (thread_peek_message(clean_agent_queue,agent_guid(GUID))->
                cleanup_wait(GUID)
                %handler(platform,(Platform_Ip,Platform_Port),clean_agent(GUID))
                ;true
        ),
        assert(agent_payload(GUID,PayloadList)),
        assert(agent_token(GUID,ListOfTokens)),
        assert_code_Tartarus_IITG([Code|CodeList]),
        assert(agent_GUID(GUID,Handler,(Platform_Ip,Platform_Port))),
        get_time(Time),
        agent_post(platform,(Ip_sender,Port_sender),[handler,platform,(Ip_sender,Port_sender),send_ackm(GUID,Time)]),
        agent_execute(GUID,(Platform_Ip, Platform_Port),Handler),
        %nl,write('Agent has been created at this port : '),
        %write(Port2),nl,
        %nl,
        true,!.

handler( platform, (_Ip_sender,_Port_sender), send(_GUID,_Handler,_PayloadList,[_Code|_CodeList],_ListOfTokens,_Bcktime)):-
                print_message(error,'Error in handler. The posted agent not received properly over the Link').

%------------------------server module-------------------------------

handler(server,(Sender_Ip,Sender_Port),agent_message(GUID,Message)):-
        ttyflush,
        get_time(Timestamp),
        nl,write(Timestamp),write(' '),write(GUID),write(' '),write(Sender_Ip),write(' '),write(Sender_Port),write(' '),write(Message),
        ttyflush,
        open('server_log.txt',append,X),
        current_output(Y),
        set_output(X),
        nl,write(Timestamp),write(' '),write(GUID),write(' '),write(Sender_Ip),write(' '),write(Sender_Port),write(' '),write(Message),
        set_output(Y),
        close(X),!;
        print_message(warning,'handler(server,(Sender_Ip,Sender_Port),agent_message(GUID,Message)) failed').

handler(serverr,(Sender_Ip,Sender_Port),agent_message(GUID,Message)):-
        ttyflush,
        get_time(Timestamp),
        nl,write(Timestamp),write(' '),write(GUID),write(' '),write(Sender_Ip),write(' '),write(Sender_Port),write(' '),write(Message),
        ttyflush,
        open('server_log1.txt',append,X),
        current_output(Y),
        set_output(X),
        nl,write(Timestamp),write(' '),write(GUID),write(' '),write(Sender_Ip),write(' '),write(Sender_Port),write(' '),write(Message),
        set_output(Y),
        close(X),!;
        print_message(warning,'handler(server,(Sender_Ip,Sender_Port),agent_message(GUID,Message)) failed').

handler(serverrr,(Sender_Ip,Sender_Port),agent_message(GUID,Message)):-
        ttyflush,
        get_time(Timestamp),
        nl,write(Timestamp),write(' '),write(GUID),write(' '),write(Sender_Ip),write(' '),write(Sender_Port),write(' '),write(Message),
        ttyflush,
        open('server_log2.txt',append,X),
        current_output(Y),
        set_output(X),
        nl,write(Timestamp),write(' '),write(GUID),write(' '),write(Sender_Ip),write(' '),write(Sender_Port),write(' '),write(Message),
        set_output(Y),
        close(X),!;
        print_message(warning,'handler(server,(Sender_Ip,Sender_Port),agent_message(GUID,Message)) failed').



%--------------------------------------------------------------------------------------------------------------------

send_log(GUID,Message):-
        get_platform_details(Agent_Ip,Agent_Port),
        message_server(GUID,(Agent_Ip,Agent_Port),Message).

send_log1(GUID,Message):-
        get_platform_details(Agent_Ip,Agent_Port),
        message_serverr(GUID,(Agent_Ip,Agent_Port),Message).

send_log2(GUID,Message):-
        get_platform_details(Agent_Ip,Agent_Port),
        message_serverrr(GUID,(Agent_Ip,Agent_Port),Message).

message_server(GUID,(Agent_Ip,Agent_Port),Message):-
        server_Ip(Ip),
        server_Port(Port),
        agent_post(platform,(Ip,Port),[handler,server,(Agent_Ip,Agent_Port),agent_message(GUID,Message)]).

message_serverr(GUID,(Agent_Ip,Agent_Port),Message):-
        server_Ip1(Ip),
        server_Port1(Port),
        agent_post(platform,(Ip,Port),[handler,serverr,(Agent_Ip,Agent_Port),agent_message(GUID,Message)]).

message_serverrr(GUID,(Agent_Ip,Agent_Port),Message):-
        server_Ip2(Ip),
        server_Port2(Port),
        agent_post(platform,(Ip,Port),[handler,serverrr,(Agent_Ip,Agent_Port),agent_message(GUID,Message)]).

set_log_server(Ip, Port):-
        retractall(server_Ip(_)),
        retractall(server_Port(_)),
        assert(server_Port(Port)),
        assert(server_Ip(Ip)).

set_log_serverr(Ip, Port):-
        retractall(server_Ip1(_)),
        retractall(server_Port1(_)),
        assert(server_Port1(Port)),
        assert(server_Ip1(Ip)).

set_log_serverrr(Ip, Port):-
        retractall(server_Ip2(_)),
        retractall(server_Port2(_)),
        assert(server_Port2(Port)),
        assert(server_Ip2(Ip)).

%-----------------------------------------------------------------------------------------------------------------------------
%---------------------------------------------------------utilities-----------------------------------------------------------

% assert_code_Tartarus_IITG/1 works on a list of code and asserts it. After asserting, it prints a message in the console. Its an internal predicate
% and therefore kept hidden from the user of this platform{Means that if he creates a assert_code_Tartarus_IITG/1 of his own, my predicate here wont
% affect his predicates working and vica versa }
% It checks for clause(Predicate) when reading from the list and if it sees one, it queries a dynamic/1 via execs/1. This effectively wipes
% all predicates with the name Predicate/arity.

%asserts a list of given code in platform.

assert_code_Tartarus_IITG([]):- true,
        %writeln('Agent/Code recieved and asserted'),
        true.

assert_code_Tartarus_IITG([Code|CodeList]):-
        (
        retractall(Code)->assert(Code);
        assert(Code)
        ),
        assert_code_Tartarus_IITG(CodeList).


%--------------------------------------------------------------------------------------

% retracts_code/1 retracts the list of code and issues an output when done

retract_code_Tartarus_IITG([]):-
        true,
        %writeln('Agent/Code retracted'),
        true.


retract_code_Tartarus_IITG([Code|CodeList]):-
        %writeln(Code),writeln('-----------'),
        (
        retract(Code)->
                true;
                (write('Could not retract: '),writeln(Code),true)
        ),
        retract_code_Tartarus_IITG(CodeList).

%------------------------------------------------------------------------------------

% file_to_list_Tartarus_IITG converts a file(full of prolog code) to a list of code. Because I want to send code as onene entity .
% Helps me keep check on the code. There are many ways of doing the above thing. This is one way of implementing it.
% The inquire_Tartarus_IITG builds a list by looping on the read and recursively calling inquire_Tartarus_IITG. Finally, that is reversed and presented
% in the unbound LIST variable. This is not an interface function. So I have not pushed up all the bounding checks

file_to_list_Tartarus_IITG(FILE,LIST) :-
   open(FILE,read,X),
   inquire_Tartarus_IITG(X,[],R),
   reverse(R,LIST),
   close(X).

inquire_Tartarus_IITG(X,IN,OUT):-
   read_clause(X,Clause,[]),
   (Clause == end_of_file -> OUT = IN ;inquire_Tartarus_IITG(X,[Clause|IN],OUT) ) .

%-----------------------------------------

% predicate_header/4 prepares the head of the predicate. Example : Predicate = platform, Arity = 3, and What = jatin.
% The Result obtained is Result = platform(jatin,_,_).
% Working : its uses functor/3 (CHECK WIN_REF.pdf) to create platform(_,_,_). Then =../2 is used to convert the tuple to a list. we
% get [platform,_,_,_] and we replace the first underscore via replace_underscore/3 by jatin. Finally using =../2 in opposite manner, I get
% platform(jatin,_,_).

% This is used to generate headers for Handler code, and payload code. The user specifies the Handler and payload name. The What is usually 'guid'
% or actual identifier for a agent. Once a predicate_header is prepared, we usually call clauses/2 to get the body(code) of the predicate.
% This is used in lots of predicates below such as generate_unique_predicate, generate_generic_predicate, genhandler etc.

predicate_header(What,Predicate,Arity,Result):-
        functor(R,Predicate,Arity),R=..[H|T],
        replace_underscore(T,What,NT),
        Result=..[H|NT],!.

replace_underscore([_H|T],What,[What|T]).

%------------------------------------------

% generatePayloadCode is used for generating payload code :D. Given a list of predicates which needs to be 'attached' to a agent, we need
% to process those predicates inorder to attach them. Given a predicate : ability(guid,X):-body of ability/2 {THE WAY OF DEFINING A PAYLOAD}
% If we wish to attach ability/2 as payload, then the term 'guid' needs to be replaced by the Agent ID(or GUID). So the above code becomes
% agent(agent) specific when ability(actual ID of agent,X):-body is generated from above code, by text manipulation. This code is asserted

generatePayloadCode(_,[],[]).

generatePayloadCode(GUID,[(Predicate,Arity)|Tail],PayloadCode):-
        generatePayloadCode(GUID,[(Predicate,Arity)|Tail],[],PayloadCode).

generatePayloadCode(GUID,[(Predicate,Arity)|Tail],TempList,PayloadCode):-
        generate_unique_predicate(GUID,Predicate,Arity,Body),
        append(TempList,Body,NTempList),
        generatePayloadCode(GUID,Tail,NTempList,PayloadCode).

generatePayloadCode(_,[],TempList,TempList).

%-------------------------------------------

% genhandler/2 is similar to what generatePayloadCode. The latter does to payload predicates, while the former does the same thing to Handler predicate
% of a agent. It replaces all instances of 'guid' and pushed the actual identifier/name of the agent/agent into that code.Uses generate_unique_predicate
% which is the main thing behind the process.

genhandler(GUID,Handler,Result):-
        generate_unique_predicate(GUID,Handler,3,Result).

%-------------------------------------------

:-dynamic temp_list/1.

temp_list([]).

add_element(H,Old,[H|Old]).

list_code(H):-
        forall(
        clause(H,Code),
        (
        term_to_atom(H,H2),
        term_to_atom(Code,Code2),
        atom_concat(H2,':-',H3),
        atom_concat(H3,Code2,Code3),
        atom_to_term(Code3,Code4,_),
        temp_list(UserHandlerCode),
        add_element(Code4,UserHandlerCode,Result),
        retractall(temp_list(_)),
        assert(temp_list(Result))
        )
        ).

% generate_unique_predicate/4 is the brain behind all the manipulation. The user specifies code of a handler / payload etc . This code is generic in nature
% since its not attached to any agent. However when we attach code (Handler or otherwise) to a agent, we replace the 'guid' present in the code by the
% name/identifier of the agent. This makes the said attachment possible. {Try this by doing on a rough copy}. The example given for generatePayloadCode
% is what happens here. predicate_header/4 is used to make the header predicate(guid,_,_..arity-1 times). The body is then found using clauses.
% Then generate_unique_predicate/5 is called which recurses through the code of the predicate. It replaces 'guid' by actual idenetifier and pushes
% all the code so generated into Result. The convert to tuple actually is used to convert the code(which is in text form) back to the tuple form (Try
% reading code from a .pl file (use file_to_list_Tartarus_IITG/2 ). you will see that the code is in tuples. So regain this back I use covert_to_tuple. Check its code
% later below

generate_unique_predicate(GUID,Predicate,Arity,Result):-
        var(Result),
        predicate_header(guid,Predicate,Arity,H),
        ((list_code(H),temp_list(UserHandlerCode),retractall(temp_list(_)),assert(temp_list([])))->(
                                                generate_unique_predicate(GUID,Predicate,UserHandlerCode,[],Result)
                                                )
                                            ; (
                                                  writeln('Warning :: '),write(Predicate),write(' not found'),nl,
                                                  unify_with_empty_list(Result,[])
                                                )
        ),!.


generate_unique_predicate(GUID,Predicate,[Head|Tail],Incoming,Outgoing):-
        term_to_atom(Head,X),
        name(X,CodeList),
        name(guid,Symbol),
        name(GUID,Glist),
        substitute_all(CodeList,Symbol,Glist,R),
        name(Text,R),
        atom_to_term(Text,Text2,_),
        append([Text2],Incoming,NextIncoming),
        generate_unique_predicate(GUID,Predicate,Tail,NextIncoming,Outgoing).

generate_unique_predicate(_,_,[],I,I).


%-------------------------------------------



add_element2(H,Old,[H|Old]).

list_code2(H):-
        forall(
        clause(H,Code),
        (
        term_to_atom(H,H2),
        term_to_atom(Code,Code2),
        atom_concat(H2,':-',H3),
        atom_concat(H3,Code2,Code3),
        atom_to_term(Code3,Code4,_),
        temp_list2(H,UserHandlerCode),
        add_element2(Code4,UserHandlerCode,Result),
        retractall(temp_list2(H,_)),
        assert(temp_list2(H,Result))
        )
        ).

% generate_generate_predicate performs the reverse process of the above predicate. Given a predicate, we find its specific defination with respect
% to a agent. Here for the above example, we will have ability(absadwf,_,_) and we would convert it to ability(guid,_,_):-blahblah. This predicate
% is used when you want to shed a payload. This payload is therfore converted into a generic format (the way a user writes a payload in file)
% and then pushed into the system. You will see this predicate in use in interface functions such as shed_payload,copy_payload .

generate_generic_predicate(GUID,Predicate,Arity,Result):-
        var(Result),
        predicate_header(GUID,Predicate,Arity,H),
        retractall(temp_list2(H,_)),
        assert(temp_list2(H,[])),
        ((list_code2(H),temp_list2(H,UniqueCode),retractall(temp_list2(H,_)))->
                        (generate_generic_predicate(GUID,Predicate,UniqueCode,[],Result));
                        (writeln('Warning :: Agent Predicate '),write(Predicate),write(' not found'),nl,
                        unify_with_empty_list(Result,[]))
        ),!.


generate_generic_predicate(GUID,Predicate,[Head|Tail],Incoming,Outgoing):-
        term_to_atom(Head,X),
        name(X,CodeList),
        name(GUID,Glist),
        name(guid,SymList),
        substitute_all(CodeList,Glist,SymList,R),
        name(Text,R),
        atom_to_term(Text,Text2,_),
        append(Incoming,[Text2],NextIncoming),
        generate_generic_predicate(GUID,Predicate,Tail,NextIncoming,Outgoing).

generate_generic_predicate(_,_,[],I,I).


%---------------------------------------------------



add_element3(H,Old,Result):-
        append(Old,[H],Result).

list_code3(GUID,H):-
                (forall(
                 clause(H,Code),(
                  term_to_atom(H,H2),
          term_to_atom(Code,Code2),
          atom_concat(H2,':-',H3),
                  atom_concat(H3,Code2,Code3),
          atom_to_term(Code3,Code4,_),
                  temp_list3(GUID,UserHandlerCode),
          add_element3(Code4,UserHandlerCode,Result),
          retractall(temp_list3(GUID,_)),
          assert(temp_list3(GUID,Result))
         )
        )).

% assimilate_code/2 fetches all the Agent code and presents it as code list in Result. It calls assimilate_payload_code/4, and assimilate_handler_code/3
% which do their job and the results are appended and returned. <Its self explantory> Read the code

assimilate_code(GUID,Result):-
        agent_GUID(GUID,Handler,_),
        assimilate_handler_code(GUID,Handler,HandlerCode),
        agent_payload(GUID,PayloadList),
        assimilate_payload_code(GUID,PayloadList,[],PayloadCode),
        append(HandlerCode,PayloadCode,Result),
        true,!.

assimilate_handler_code(GUID,Handler,HandlerCode):-
        predicate_header(GUID,Handler,3,Result),
        (
                retractall(temp_list3(GUID,_)),
                assert(temp_list3(GUID,[])),
                (list_code3(GUID,Result),temp_list3(GUID,HandlerCode),retractall(temp_list3(GUID,_)))->nothing;
                (
                writeln('Warning :: Handler code for agent not found - id '),
                write(GUID),nl,
                unify_with_empty_list(HandlerCode,[])
                )
        ).

assimilate_payload_code(GUID,[(Predicate,Arity)|Tail],Incoming,Outgoing):-
        predicate_header(GUID,Predicate,Arity,Result),
        (
                retractall(temp_list3(GUID,_)),
                assert(temp_list3(GUID,[])), 
                (list_code3(GUID,Result),temp_list3(GUID,Body),retractall(temp_list3(GUID,_)))->append(Body,Incoming,NextIncoming) ;
                (
                unify_with_empty_list(Body,[]),
                append(Body,Incoming,NextIncoming)
                )
        ),
        assimilate_payload_code(GUID,Tail,NextIncoming,Outgoing).

assimilate_payload_code(_,[],I,I):-!.

%-----------------------------------------------------

% replaceGUID/4 is used for cloning purposes. In cloning(see agent_clone/2), a new identifier is generated and the original agent code is
% assmilated. This code is AgentCode. All instances of the original identifier in AgentCode is replaced by the new identifier generated before
% The resulting code is returned as CloneCode. The working of replaceGUID is simple. It parses through the AgentCode, and substitutes using substitute_all/4.
% AgentCode is list of tuples(each tuple being a predicate definiation). Each tuple is converted into string using portray_clause/2. Then the strings
% are converted into a List of ascii numbers (for the characters of the string) . Substitution is now performed. After the substitution, the code is packed
% into text(characters again). And pushed into list again. These are then converted into tuples . Result is the CloneCode
% Example : original agent is qwertyu(GUID1) and its code is given below. Lets say the new clone ID(GUID2) is zxcvbn
% AGENTCODE [(hello(qwertyu,X):-abcd(qwertyu),write('heheh')),(bbye(qwertyu):-write(bye))]
% CLONECODE obtained will be : [(hello(zxcvbn,X):-abcd(zxcvbn),write('heheh')),(bbye(zxcvbn):-write(bye))]

replaceGUID(AgentCode,GUID1,GUID2,CloneCode):-
        replaceGUID(AgentCode,GUID1,GUID2,[],CloneCode),!.

replaceGUID([Head|Tail],GUID1,GUID2,Incoming,Outgoing):-
        term_to_atom(Head,X),
        name(X,CodeList),
        name(GUID1,Remove),
        name(GUID2,Add),
        substitute_all(CodeList,Remove,Add,R),
        name(Text,R),
        atom_to_term(Text,Text2,_),
        append(Incoming,[Text2],NextIncoming),
        replaceGUID(Tail,GUID1,GUID2,NextIncoming,Outgoing).

replaceGUID([],_,_,Incoming,Incoming).

%-----------------------------------------------------

% hoptime_rec/2 is used for recording the times. Its essentially used for calcluating the averages. Given the previous no of agents
% going out/in and the avg out/in time, a new average is calculated each time an agent moves out/in. The time to go out/in is represented by
% T. hoptime_rec with second paramter as outgoing computes the new average outgoing time and updates the statistics. The hoptime-rec with incoming as
% second parameter calculates the same for incoming agents.

hoptime_rec(T,outgoing):-
        hoptime_stat(Out,OutAvg,Inc,IncAvg),
        NewAvg is (((Out *OutAvg)+T)/(Out+1)),
        NewOut is Out +1,
        retractall(hoptime_stat(Out,OutAvg,Inc,IncAvg)),
        assert(hoptime_stat(NewOut,NewAvg,Inc,IncAvg)),!.

hoptime_rec(T,incoming):-
        hoptime_stat(Out,OutAvg,Inc,IncAvg),
        NewAvg is (((Inc *IncAvg)+T)/(Inc+1)),
        NewInc is Inc +1,
        retractall(hoptime_stat(Out,OutAvg,Inc,IncAvg)),
        assert(hoptime_stat(Out,OutAvg,NewInc,NewAvg)),!.

hoptime_rec1(T1):-  
       minimum_hop_out(T1), maximum_hop_out(T1),
       outgoing_hoptime_stat(Out1,OutAvg1),  
       NewAvg1 is (((Out1 *OutAvg1)+T1)/(Out1+1)),
       NewOut1 is Out1 +1, 
       retractall(outgoing_hoptime_stat(Out1,OutAvg1)),
       assert(outgoing_hoptime_stat(NewOut1,NewAvg1)),
       min_val_out(Mvo), max_val_out(Mavo),
       retractall(out_hop_time_stat(_,_,_)),
       assert(out_hop_time_stat(Mavo,Mvo,NewAvg1)), overall_avg.

hoptime_rec2(T2):- 
       minimum_hop_in(T2), maximum_hop_in(T2),
       incoming_hoptime_stat(Inc1,IncAvg1),  
       NewAvg1 is (((Inc1 *IncAvg1)+T2)/(Inc1+1)),
       NewInc1 is Inc1 +1, 
       retractall(incoming_hoptime_stat(Inc1,IncAvg1)),
       assert(incoming_hoptime_stat(NewInc1,NewAvg1)),
       min_val_in(Mvo), max_val_in(Mavo),
       retractall(in_hop_time_stat(_,_,_)),
       assert(in_hop_time_stat(Mavo,Mvo,NewAvg1)), overall_avg.

overall_avg:-
      minimum1(OverallMin), maximum1(OverallMax),
      outgoing_hoptime_stat(NumofAg_out,Aver_out), incoming_hoptime_stat(NumofAg_in,Aver_in), 
      Ans1 is  (NumofAg_out * Aver_out)+(NumofAg_in * Aver_in), 
      OverallAverageHopTime is (Ans1/(NumofAg_out+NumofAg_in)),
      retractall(overall_hop_time_stat(_,_,_)), assert(overall_hop_time_stat(OverallMax,OverallMin,OverallAverageHopTime)).
 
%----------------------------------------------------
hoptime(Out,OutAvg,Inc,IncAvg):-
        var(Out),var(OutAvg),var(Inc),var(IncAvg),
        hoptime_stat(Out,OutAvg,Inc,IncAvg).

%----------------------------------------------------------

% list_agents/0 prints all the agents and their ports onto the console. list_agents/2 lets you do the same, accept that
% it lets you unify that with variables.
/* Depracated --- tushar 01 March 2017
list_agents:-
        forall(
                        (
                                agent_GUID(G,_H,P)
                        ),
                        (
                                writeln('Identifier : '),write(G),write('||| Port : '),write(P),nl,nl
                        )
                 ).
list_agents(G,P):-
        agent_GUID(G,_,P),!.
*/
%------------------------------------------------------

% hide all inbuilt predicates from user to avoid interference with user defined predicates in case of concurrency . test its working....

hide_mycode:-
        noprofile(setup_Tartarus_IITG/0),
        noprofile(platform_port/1),
        noprofile(execs/1),
        noprofile(agent_GUID/3),
        noprofile(agent_payload/2),
        noprofile(outgoing_agent/3),
        noprofile(hoptime_stat/4),
        noprofile(agent_token/2),
        noprofile(transit_GUID/3),
        noprofile(transit_payload/2),
        noprofile(transit_token/2),
        noprofile(transit_code/2),
        noprofile(platform_token/1),
        noprofile(code_base/2),
        noprofile(do/1),
        noprofile(start_db/1),
        noprofile(handler/3),
        noprofile(assert_code_Tartarus_IITG/1),
        noprofile(retract_code_Tartarus_IITG/1),
        noprofile(file_to_list_Tartarus_IITG/2),
        noprofile(inquire_Tartarus_IITG/1),
        noprofile(predicate_header/4),
        noprofile(replace_underscore/3),
        noprofile(insert_underscores/1),
        noprofile(generatePayloadCode/1),
        noprofile(genhandler/2),
        noprofile(generate_unique_predicate/4),
        noprofile(generate_generic_predicate/4),
        noprofile(write_unique_code/1),
        noprofile(assimilate_code/2),
        noprofile(assimilate_handler_code/3),
        noprofile(assimilate_payload_code/4),
        noprofile(replaceGUID/4),
        noprofile(hoptime_rec/2),
        noprofile(substitute/4),
        noprofile(substitute_all/4),
        noprofile(unify_with_empty_list/1),
        noprofile(redo/1),
        noprofile(delete_x/1),
        noprofile(delete_list/1),
        noprofile(replace/1).

hide_mycode:- true.

%---------------------------------------------------------

timeout(Name,Status):- writeln(Name),write(' : gave a timeout'),write(Status),nl.

%---------------------------------------------------------

substitute(Source, Tag, Content, X) :-
        append(Tag, After, TagAndAfter),
        append(Before, TagAndAfter, Source),
        append(Before, Content, BeforeAndContent),
        append(BeforeAndContent, After, X).

substitute_all(Source, Tag, Content, X) :-
        append(Tag, After, TagAndAfter),
        append(Before, TagAndAfter, Source),
        append(Before, Content, BeforeAndContent),
        append(BeforeAndContent, FinalAfter, X),
        !,
        substitute_all(After, Tag, Content, FinalAfter).

substitute_all(Source, _, _, Source).


%----------------------------------------------------------

aborted :-
        nl,
        write('! ----------------------------------------'),nl,
        write('! ERROR (CHECK PARAMETERS)----------------'),nl,
        write('! ----------------------------------------'),nl,
        abort.

%----------------------------------------------------------

unify_with_empty_list([],[]).
redo.
redo:-redo.
nothing.

%----------------------------------------------------------

delete_x(X,[X|T],T).
delete_x(X,[Y|T],[Y|NT]):-delete_x(X,T,NT).

%----------------------------------------------------------

delete_list([H|T],[H|T],[]).
delete_list([X1|Y1],Incoming,Outgoing):-
        delete_x(X1,Incoming,NextIncoming),
        delete_list(Y1,NextIncoming,Outgoing).
delete_list([],Incoming,Incoming).

%-----------------------------------------------------------
% add_tail(+List,+Element,-List)
% Add the given element to the end of the list, without using the "append" predicate.
add_tail([],X,[X]).
add_tail([H|T],X,[H|L]):-add_tail(T,X,L).
%-----------------------------------------------------------------------
replace([],_A,_B,[]).
replace([H|T],A,B,[B|Result]) :-
    H=A,
    replace(T,A,B,Result),!.
replace([H|T],A,B,[H|Result]) :-
    replace(T,A,B,Result).

%-----------------------------------------------------------------

insert_underscores(0).
insert_underscores(No):-
        write(',_'),
        UnderscoresLeft is No - 1,
        insert_underscores(UnderscoresLeft).

%-----------------------------------------------------------------

write_unique_code([],File):-
        open(File,write,X),
        close(X).

write_unique_code(Code,File):-
        open(File,write,X),
        current_output(Y),
        set_output(X),
        write_unique_code2(Code),
        set_output(Y),
        close(X).

write_unique_code2([Head|Tail]):-
        with_output_to(atom(X),portray_clause(Head)),
        write(X),nl,
        write_unique_code2(Tail).

write_unique_code2([]):-true.

%-----------------------------------------------------------------

get_ip(Ip):-
        gethostname(Hostname),
        tcp_host_to_address(Hostname,RawIP),
        ip(I1,I2,I3,I4)=RawIP,
        atom_concat(I1,.,R1),atom_concat(R1,I2,R2),
        atom_concat(R2,.,R3),atom_concat(R3,I3,R4),
        atom_concat(R4,.,R5),atom_concat(R5,I4,Ip).



%===========================================debugging utilities==================================================
print_tartarus_status:-
        print_platform_status,!.
		
		
print_tartarus_status:-
        print_message(warning,'print_tartarus_status/0 failed!').
        

print_platform_status:-
        ttyflush,
        platform_Ip(Y),write('platform_Ip : '),write(Y),write('    '),
        platform_port(X),write('platform_port : '),write(X),nl,
        (agent_GUID(_,_,_)->
        writeln('-------------------------------------------'),
        forall(agent_GUID(X1,Y1,Z1),(write('Agent_GUID: '),write(X1),write('|'),write(Y1),write('|'),write(Z1),nl))
        ;nothing),
        (agent_payload(_,_)->
        writeln('-------------------------------------------'),
        forall(agent_payload(X2,Y2),(write('Agent_payload: '),write(X2),write('|'),write(Y2),nl))
        ;nothing),
        (outgoing_agent(_,_,_)->
        writeln('-------------------------------------------'),
        forall(outgoing_agent(X3,Y3,Z3),(write('outgoing_agent: '),write(X3),write('|'),write(Y3),write('|'),write(Z3),nl))
        ;nothing),
        (agent_token(_,_)->
        writeln('-------------------------------------------'),
        forall(agent_token(X4,Y4),(write('Agent_token: '),write(X4),write('|'),write(Y4),nl))
        ;nothing),
        (platform_token(_)->
        writeln('-------------------------------------------'),
        forall(platform_token(X5),(write('platform_token: '),write(X5),nl))
        ;nothing),
        (transit_GUID(_,_,_)->
        writeln('-------------------------------------------'),
        forall(transit_GUID(X6,Y6,Z6),(write('transit_GUID: '),write(X6),write('|'),write(Y6),write('|'),write(Z6),nl))
        ;nothing),
        (transit_payload(_,_)->
        writeln('-------------------------------------------'),
        forall(transit_payload(X7,Y7),(write('transit_payload: '),write(X7),write('|'),write(Y7),nl))
        ;nothing),
        (transit_token(_,_)->
        writeln('-------------------------------------------'),
        forall(transit_token(X8,Y8),(write('transit_token: '),write(X8),write('|'),write(Y8),nl))
        ;nothing),
        (transit_code(_,_)->
        writeln('-------------------------------------------'),
        forall(transit_code(X9,Y9),(write('transit_code: '),write(X9),write('|'),write(Y9),nl))
        ;nothing),
        (transit_agent(_,_)->
        writeln('-------------------------------------------'),
        forall(transit_agent(X10,Y10),(write('transit_agent: '),write(X10),write('|'),write(Y10),nl))
        ;nothing),
        (code_base(_,_)->
        writeln('-------------------------------------------'),
        forall(code_base(X11,Y11),(write('code_base: '),write(X11),write('|'),write(Y11),nl))
        ;nothing),
        %writeln('-------------------------------------------'),
        %forall(guid_threadid(X12,Y12),(write('guid_threadid: '),write(X12),write('|'),write(Y12),nl)),
        writeln('-------------------------------------------'),ttyflush.
		

%%Set the Tartarus GUI server IP and Port
set_gui_server(Ip,Port):-
        retractall(gui_server_Ip(_)),
        retractall(gui_server_Port(_)),
        assert(gui_server_Port(Port)),
        assert(gui_server_Ip(Ip)).

%% Add agent position on GUI
set_position(Agent_name, Source):-
        gui_server_Port(Port),
        gui_server_Ip(Ip),
        tcp_socket(Socket),
        tcp_connect(Socket,Ip:Port),
        tcp_open_socket(Socket,_In,Out),
        format(Out,'~q-~q-~q\n',[0,Agent_name, Source]),
        flush_output(Out),
        close(Out).     

        
%% Remove agent from GUI        
remove_position(Agent_name, Source):-
        gui_server_Port(Port),
        gui_server_Ip(Ip),
        tcp_socket(Socket),
        tcp_connect(Socket,Ip:Port),
        tcp_open_socket(Socket,_In,Out),
        format(Out,'~q-~q-~q\n',[1,Agent_name, Source]),
        flush_output(Out),
        close(Out).

%----------------------------Predicate to check whether the handler is present------------------------%		
check_handler(H,3):-                                                                                      %added by Menaxi J Bagchi%
   (current_predicate(H/3),true),!;writeln('Handler not present'),abort.	
%-----------------------------------------------------------------------------------------------------%
   
%======================================================================================================%
%                                       Tartarus Text-to-Speech tool                                   %
%                                                                                                      %
% This predicate is used to utter a specific text (word or sentence), as shall be provided in place of %
% the variable in single quotes.								       %
%												       %
% Prior to the usage, kindly install the espeak library as follows:				       %
% Windows:											       %
% Please execute the windows installer provisioned at "http://espeak.sourceforge.net/download.html"    %
% to install espeak in C drive.									       %
%												       %						
% Ubuntu:											       %
% use the command "sudo apt-get install espeak"							       %
% 												       %
% added by Suraj Kumar Pandey									       %	
%======================================================================================================%



go_to_path:-
		sysbit(32)->
					cd('C:\\Program Files\\eSpeak\\command_line');
					cd('C:\\Program Files (x86)\\eSpeak\\command_line').
tts(X):-
	tt(1)->						
				
				(go_to_path,
					subt(0)-> 
						string_concat('espeak \"',X,Y),string_concat(Y,'\"',Z),shell(Z);
						string_concat('espeak \"',X,Y),string_concat(Y,'\"',Z),shell(Z),writeln(X));
				subt(0)->
					true;writeln(X).


subt_on:-retractall(subt(_)),asserta(subt(1)).


subt_off:-retractall(subt(_)),asserta(subt(0)).

tts_on:-retractall(tt(_)),asserta(tt(1)).


tts_off:-retractall(tt(_)),asserta(tt(0)).





stt(Result) :-
	atomic_list_concat(['from ', 'stt', ' import *;print(', 'say_hello', '())'], Command),
    	process_create(path('python'), ['-c', Command], [stdout(pipe(Out))]),
    	read_lines(Out, Lines), last(Lines, Result),writeln(Result).
	
verbose(Il10):- (Il10=:=0,retractall(verbose1(_)), assert(verbose1(Il10)),!;
                 Il10=:=1,retractall(verbose1(_)), assert(verbose1(Il10)),!;
                 writeln('entered value should either be 0 for disabling the messages or 1 for enabling them'),!).
		
%%%INSERTED BY S.B.NAIR BEGINS HERE
/***** BILLBOARDS AND DIALOGS ********/

/****************** BILLBOARDS *****************/
%%%INSERTED BY S.B.NAIR BEGINS HERE              19th Nov. 2016


clear_billboard(Bill_Name):- retract(plt_bboard(Bill_Name,P)), free(P),
        /*get_platform_details(Ip,Port),
        string_concat('TARTARUS@',Ip, Txt1),
        string_concat(Txt1, '::', Txt2),
        string_concat(Txt2, Port, Title_Txt), */
        create_billboard(Bill_Name,'Ready4Action',10,10,0).

clear_billboard(_,_):- writeln('WARNING:No such Billboard to Clear!').


create_billboard(Bill_Title_Txt,Disp_Txt,X,Y,Time):-
           not(plt_bboard(Bill_Title_Txt,P)),new(P,picture(Bill_Title_Txt)),assert(plt_bboard(Bill_Title_Txt,P)),
send(P, display, text(Disp_Txt), point(X,Y)), send(P, open),
sleep_time(Time,P).

create_billboard(_,_,_,_,_):-  writeln('Error: Billboard with this name already exists!').

write2billboard(Bill_Name,Disp_Txt,X,Y):- plt_bboard(Bill_Name,P),
                         send(P, display, text(Disp_Txt), point(X,Y)), send(P, open).
write2billboard(_,_,_,_):-        writeln('Error: Window with this name does not exist!').

close_billboard(Bill_Name):- retract(plt_bboard(Bill_Name,P)), free(P).
close_billboard(_):- writeln('Error: Window with this name does not exist!').

close_billboards:-retract(plt_bboard(_A,B)), free(B), fail.
close_billboards.


sleep_time(0,_).
sleep_time(Time,P):- sleep(Time), free(P).

:-dynamic plt_bboard/2.



%%%INSERTED BY S.B.NAIR on 23rd Nov. 2016

/*************** DIALOGS **************/

%:- use_module(library(pce)).
/*USAGE::
create_dlg/3
create_dlg(DialogID, DialogTitle, ComponentList)
DialogID: It is an unbound variable that will form the ID of the dialog.
DialogTitle: This needs to be provided by the user and will be what appears at the top of the dialog window.
ComponentList: This is a list of components that the dialog holds. The manner in which
a member of the list is represented depends on whether it is an text item, a menu, a slider or a button.
The format for each is given below:
Text item:
(text_item,Text_item_Class_ID,Text_Label,DefaultTextWithinBox)
The first argument needs to be typed as it is while the second has to be an unbound variable.
The second forms the ID of the text item and is used to get the data provided using NameClass?selection explained later in the button format.
The third argument is a string which forms the default text that needs to be displayed in the text box which can then can be edited in run-time.
E.g. (text_item,NameClass,name,'TypeHere')
Slider:
(slider,Slider_Class_ID,slider(SliderLabel, X,Y,Z)
The first argument needs to be typed as it is while the second has to be an unbound variable.
The second forms the ID of the slider and is used to get the data provided using SliderClass?selection explained later in the button format.
In the third argument SliderLabel has to be bound to the label describing it. This label appears alongside the slider.
X,Y and Z, all numeberical values, refer to the starting, ending and current position of the slider. X,Y and Z need to be bound.
Menu:
(menu,Menu_Class_ID,mymenu,MenuList)
The first argument needs to be typed as it is while the second has to be an unbound variable.
The second forms the ID of the menu and is used to get the data provided using MenuClass?selection explained later in the button format.
The third is the name of the menu or the text that is indicative of the menu. This has to be bound to a string.
The fourth, MenuList, is a list takes the form [class(menu_option_1),class(menu_option_2),...,class(menu_option_n)]
where the menu_option_i indicates the string that has to appear as an item name in the menu. MenuList should be bound.
E.g.: (menu,MenuClass,mymenu,[class(dosa),class(idli),class(sandwich),class(paratha)])
Button: (button,ButtonLabel,message(@prolog,dialog_handler,NameClass?selection,SliderClass?selection,MenuClass?selection)
The first argument needs to be typed as it is as it indicates a button.
The second has to be label/text that needs to appear on the button and thus needs to be bound.
The third is a bit complex and changes based on the requirement.
The message predicate actually invokes the event handler viz. dialog_handler written by the user in Prolog, when the button is pressed/activated.
The remaining arguments form the arity of the dialog_handler. Note the form in which they need to be written:
Dialog_Class_ID?selection where Dialog_Class_ID could be any of the second arguments of the text, slider or menu items in the dialog.
Note:
A couple of message formats are given below for ready reference:
message(DialogID,destroy)    % The third argument is missing since the handler here does not need it.
message(@prolog, format, ’Hi There˜n’))
E.g.
my_dialog:-
create_dlg(DialogID,'MyWindow',
           [(text_item,NameClass,name,'TypeHere'),(slider,SliderClass,slider(mywidth, 100, 1000, 400)),
            (menu,MenuClass,mymenu,[class(dosa),class(idli),class(sandwich),class(paratha)]),
            (button,enter,message(@prolog,dialog_handler,NameClass?selection,SliderClass?selection,MenuClass?selection)),
            (button,abort, message(DialogID,destroy))
           ]
          ).
dialog_handler(Class, Label, Width) :- writeln(Class), writeln(Label), writeln(Width).
%%%% CODE TO HANDLE DIALOGS - TEXT INPUT, MENU, SLIDER and BUTTON %%%%%                */


create_dlg(DialogID,DName, [Head|Tail]):- new(DialogID, dialog(DName)),  process_dlg(DialogID,[Head|Tail]).

process_dlg(D,[]):-send(D,open).

process_dlg(D,[(text_item,LabelClass,TxtLabel,Untitled)|T]):-
            send(D, append, new(LabelClass, text_item(TxtLabel, Untitled))),!,            %NOTE: The first arg of send is an output. Thus should be unbound for use by other predicates
            process_dlg(D,T).

process_dlg(D,[(menu,MenuClass,MenuLabel,MenuList)|T]):-
            send(D, append, new(MenuClass, menu(MenuLabel, choice))),
            send_list(MenuClass, append, MenuList), !, process_dlg(D,T).

process_dlg(D,[(slider,SliderClass,SliderParameters)|T]):-
            send(D, append,new(SliderClass,SliderParameters)),!, process_dlg(D,T).

process_dlg(D,[(button,ButtonLabel,ButtonMessage)|T]):-
            send(D, append,button(ButtonLabel,ButtonMessage)),!,process_dlg(D,T).

process_dlg(_,_):-writelb('Problem with dialog!').



%%%INSERTED BY S.B.NAIR ENDS HERE

%-------------------Different Network-----------------------------------------------------------------------------------------%
inter_network(GUID,Queue,N) :-                              % added by Menaxi J Bagchi
                        thread_get_message(Queue,L),
                        thread_get_message(Queue,Len),testloop(L,Len,GUID,Queue,N).  % L is list,Len is the length of list
						 
testloop(L,L1,GUID,Queue,N):-						 
						 (
						 L1>0, nth1(L1,L, Elem), writeln(Elem), thread_send_message(Queue,Elem),   %start checking from the last element
                         thread_get_message(Queue,Elem)->writeln('got'),
						(member(N,Elem)->true,last(Elem,Last),nth1(2,Elem,P),move_agent(GUID,(Last,P)); writeln('not found'),
						 M is L1-1, testloop(L,M,GUID,Queue,N));
						 writeln('Destination not found.')
						 ).   

predicate_tar(_Receive,(localhost,50),GUID,Source_ip,Source_port,Dest_ip,Dest_port,Ya2,Start_time,Time_taken):-
         hoptime_rec2(Time_taken),
         retractall(inc_agent(_)),retractall(inc_sip(_)),retractall(inc_sp(_)),retractall(inc_dip(_)),
         retractall(inc_dp(_)),retractall(inc_date(_)),retractall(inc_stime(_)),retractall(inc_htime(_)),
         assert(inc_agent(GUID)),assert(inc_sip(Source_ip)),assert(inc_sp(Source_port)),assert(inc_dip(Dest_ip)),
         assert(inc_dp(Dest_port)),assert(inc_date(Ya2)),assert(inc_stime(Start_time)),assert(inc_htime(Time_taken)),
         inc_agent(Agent), inc_sip(Soip),inc_sp(Sop),inc_dip(Dip),inc_dp(Dop),inc_date(Sod),inc_stime(St),inc_htime(Stit),
         K8 = (Agent,(Soip,Sop),(Dip,Dop),(Sod,St),Stit), 
         %K = (GUID,(Soip,_),(_,_),(_,_),_), K3 = [(GUID,(Soip,_),(_,_),(_,_),_)], 
         in_hop_times_details(K1),      
         %(member(K,K1)->delete_list(K3,K1,NewList1),add_tail(NewList1,K8,New1),retractall(in_hop_times_details(_)),assert(in_hop_times_details(New1));add_tail(K1,K8,New1),retractall(in_hop_times_details(_)),assert(in_hop_times_details(New1))),
         add_tail(K1,K8,New1),retractall(in_hop_times_details(_)),assert(in_hop_times_details(New1)),
         length(New1,Leng1), retractall(length2(_)),assert(length2(Leng1)), retractall(in_hop_time(_,(_,_),(_,_),(_,_),_)),hoptime_info1.

predicate_tar1(_Receive1,(localhost,60),GUID,Time_taken):- 
         retractall(inc_agent1(_)), retractall(inc_htime1(_)), 
         assert(inc_agent1(GUID)), assert(inc_htime1(Time_taken)), 
         inc_agent1(Agent1), inc_htime1(Stit1), 
         K88 = (Agent1,Stit1), 
         %K82 = (GUID,_), K83 = [(GUID,_)], 
         in_hop_times_info(K81),      
         %(member(K82,K81)->delete_list(K83,K81,NewList200),add_tail(NewList200,K88,New200),retractall(in_hop_times_info(_)),assert(in_hop_times_info(New200));add_tail(K81,K88,New200),retractall(in_hop_times_info(_)),assert(in_hop_times_info(New200))),
         add_tail(K81,K88,New200),retractall(in_hop_times_info(_)),assert(in_hop_times_info(New200)),
         length(New200,Leng5), retractall(length5(_)),assert(length5(Leng5)), retractall(in_hop_time(_,_)),hoptime_info2.						 
						 
hoptime_info:- 
      out_hop_times_details(New),write_list(New).
      
      
write_list([]).
write_list([(A,(B,C),(D,E),(F,G),H)|Tail]) :-
  assert(out_hop_time(A,(B,C),(D,E),(F,G),H)), 
  write_list(Tail).


hoptime_info1:- 
      in_hop_times_details(New1),write_list1(New1).
      
      
write_list1([]).
write_list1([(Aa,(Bb,Cc),(Dd,Ee),(Ff,Gg),Hh)|Tail]) :-
  assert(in_hop_time(Aa,(Bb,Cc),(Dd,Ee),(Ff,Gg),Hh)), 
  write_list1(Tail).

hoptime_info3:- 
      out_hop_times_info(New100),write_list3(New100).
      
      
write_list3([]).
write_list3([(A3,B3)|Tail]) :-
  assert(out_hop_time(A3,B3)), 
  write_list3(Tail).

hoptime_info2:- 
      in_hop_times_info(New200),write_list2(New200).

write_list2([]).
write_list2([(A2,B2)|Tail]) :-
  assert(in_hop_time(A2,B2)), 
  write_list2(Tail).

minimum_hop_out(Time_taken1):- 
      min_val_out(Min),
      ( Time_taken1=<Min, retractall(min_val_out(_)), assert(min_val_out(Time_taken1)); nothing), overall_min.

maximum_hop_out(Time_taken2):- 
       max_val_out(Max), 
      ( Time_taken2>=Max, retractall(max_val_out(_)), assert(max_val_out(Time_taken2));nothing), overall_max.

minimum_hop_in(Time_taken1):- 
      min_val_in(Min),
      ( Time_taken1=<Min, retractall(min_val_in(_)), assert(min_val_in(Time_taken1)); nothing), overall_min.

maximum_hop_in(Time_taken2):- 
       max_val_in(Max), 
      ( Time_taken2>=Max, retractall(max_val_in(_)), assert(max_val_in(Time_taken2));nothing),overall_max. 
      
overall_min:- 
      min_val_in(Minvalin), min_val_out(Minvalout),
      ( Minvalin=<Minvalout, retractall(minimum1(_)), assert(minimum1(Minvalin)); retractall(minimum1(_)),assert(minimum1(Minvalout))).

overall_max:- 
      max_val_in(Maxvalin), max_val_out(Maxvalout),
      ( Maxvalin>=Maxvalout, retractall(maximum1(_)), assert(maximum1(Maxvalin)); retractall(maximum1(_)),assert(maximum1(Maxvalout))).

%--------------------------------------------------error correction-----------------------------------------------------------%
%-----------added by Menaxi J Bagchi-----------------------------------------------------------------------------------------%

:-dynamic error/0.
:-dynamic ecnt/1.

ecnt(0).

check_syntax(assert_file_to_tartarus,FILE):-
            catch((atom(FILE)->true;assert(error),retract(ecnt(X)),X1 is X+1,assert(ecnt(X1)),throw(notAtom)),_Error2000,(write('Error# tf1:'),writeln(' The argument should be the file name which is an atom.'))), chk_err.        

check_syntax(set_token,Token):-
            catch((integer(Token)->true;assert(error),retract(ecnt(X)),X1 is X+1,assert(ecnt(X1)),throw(notInt)),_Error2050,(write('Error# st:'),writeln(' The argument should be the token number which is an integer.'))), chk_err.        

check_syntax(purge_agent,Guid):-
            catch((atom(Guid)->true;assert(error),retract(ecnt(X)),X1 is X+1,assert(ecnt(X1)),throw(notAtom)),_Error2550,(write('Error# pa:'),writeln(' The argument should be the agent name which is an atom.'))), chk_err.

check_syntax(get_new_name_alpha,Agent_name):-
            catch((var(Agent_name)->true;assert(error),retract(ecnt(Y)),Y1 is Y+1,assert(ecnt(Y1)),throw(notVar)),_Error400,(write('Error# gnma1:'),writeln(' The argument should be a variable representing the name of an agent.'))),chk_err. 
             
check_syntax(add_payload,GUID,AddList):-
            catch((atom(GUID)->true;assert(error),retract(ecnt(X)),X1 is X+1,assert(ecnt(X1)),throw(notAtom)),_Error1,(write('Error# ap1:'),writeln(' First argument which is the name of the agent should be an atom.'))),      
            catch((is_list(AddList)->true;assert(error),retract(ecnt(Y)),Y1 is Y+1,assert(ecnt(Y1)),throw(notList)),_Error2,(write('Error# ap2:'),writeln(' Last argument which is the payload list should be a list.'))),chk_err.

check_syntax(remove_payload,GUID,ToRemove):-
            catch((atom(GUID)->true;assert(error),retract(ecnt(X)),X1 is X+1,assert(ecnt(X1)),throw(notAtom)),_Error300,(write('Error# rp1:'),writeln(' First argument which is the name of the agent should be an atom.'))),      
            catch((is_list(ToRemove)->true;assert(error),retract(ecnt(Y)),Y1 is Y+1,assert(ecnt(Y1)),throw(notList)),_Error301,(write('Error# rp2:'),writeln(' Last argument which is the payload list should be in the form of a list.'))),chk_err.

check_syntax(save_agent,GUID,FileName):-
            catch((atom(GUID)->true;assert(error),retract(ecnt(X)),X1 is X+1,assert(ecnt(X1)),throw(notAtom)),_Error1000,(write('Error# sav1:'),writeln(' First argument which is the name of the agent should be an atom.'))),      
            catch((atom(FileName)->true;assert(error),retract(ecnt(Y)),Y1 is Y+1,assert(ecnt(Y1)),throw(notList)),_Error1001,(write('Error# sav2:'),writeln(' Last argument which is a file name should be an atom.'))),chk_err.

check_syntax(get_tartarus_details,Ip,Port):-
            catch((var(Ip)->true;assert(error),retract(ecnt(X)),X1 is X+1,assert(ecnt(X1)),throw(notVar)),_Error3,(write('Error# td1:'),writeln(' First argument should be the IP which should be a variable here.'))), 
            catch((var(Port)->true;assert(error),retract(ecnt(Y)),Y1 is Y+1,assert(ecnt(Y1)),throw(notVar)),_Error4,(write('Error# td2:'),writeln(' Last argument should be the Port number which should be a variable here.'))),
			%catch((var(OS)->true;assert(error),retract(ecnt(Z)),Z1 is Z+1,assert(ecnt(Z1)),throw(notVar)),_Error9876,(write('Error# td3:'),writeln(' Last argument is not a variable.'))),
			chk_err. 
						
check_syntax(start_tartarus,_Ip,Port,Token):-
			 catch((integer(Port)->true;assert(error),retract(ecnt(X)),X1 is X+1,assert(ecnt(X1)),throw(notInt)),_Error5,(write('Error# t1:'),writeln(' Second argument should be the port number which is an integer.'))),
			 catch((integer(Token)->true;assert(error),retract(ecnt(Y)),Y1 is Y+1,assert(ecnt(Y1)),throw(notInt)),_Error6,(write('Error# t2:'),writeln('  Last argument should be the platform token which is an integer.'))),
			 ((integer(Port)->(catch((pre1(Port)),_Error900,(write('Error# t3:'),writeln(' Second argument should be the port number which is a positive integer.')))));chk_err),
			 chk_err.	
			 
check_syntax(move_agent,_GUID,_Ip_other_end,Port_other_end):-
             %catch((atom(GUID)->true;assert(error),retract(ecnt(Y)),Y1 is Y+1,assert(ecnt(Y1)),throw(notAtom)),_Err,(write('Error# mov1:'),writeln(' First argument is not an atom.'))),
             catch((integer(Port_other_end)->true;assert(error),retract(ecnt(X)),X1 is X+1,assert(ecnt(X1)),throw(notInt)),_Err1,(write('Error# mov2:'),writeln(' Last argument should be the port number which is an integer.'))),	
             ((integer(Port_other_end)->(catch((pre1(Port_other_end)),_Err2,(write('Error# mov3:'),writeln(' Last argument should be the port number which is a positive integer.')))));chk_err),
			 chk_err.			 

check_syntax(create_mobile_agent,_GUID,Handler,List):-
			 catch((atom(Handler)->true;assert(error),retract(ecnt(Y)),Y1 is Y+1,assert(ecnt(Y1)),throw(notAtom)),_Er,(write('Error# ma21:'),writeln(' Second argument should be the name of the handler which is an atom.'))),
			 catch((is_list(List)->true;assert(error),retract(ecnt(Z)),Z1 is Z+1,assert(ecnt(Z1)),throw(notList)),_Er1,(write('Error # ma22:'),writeln(' Last argument which is a list of tokens should be in the form of a list.'))),
             ((is_list(List)->(catch((pre(List)),_Er2,(write('Error# ma23:'),writeln(' Last argument should be a list of tokens which is an integer.')))));chk_err),chk_err.			 
			 
check_syntax(execute_agent3,_Agent_name,_Ip,Port,Handler):-
            %catch((atom(Agent_name)->true;assert(error),retract(ecnt(X)),X1 is X+1,assert(ecnt(X1)),throw(notAtom)),_Error7,(write('Error# ea1:'),writeln(' First argument should be the agent name which is an atom.'))),      		
			catch((integer(Port)->true;assert(error),retract(ecnt(Y)),Y1 is Y+1,assert(ecnt(Y1)),throw(notInt)),_Error8,(write('Error# ea2:'),writeln(' Second argument should be the port number which is an integer.'))),
            catch((atom(Handler)->true;assert(error),retract(ecnt(Z)),Z1 is Z+1,assert(ecnt(Z1)),throw(notAtom)),_Error9,(write('Error# ea3:'),writeln(' Last argument should be the handler name which is an atom.'))),
			((integer(Port)->(catch((pre1(Port)),_Error50,(write('Error# ea4:'),writeln(' Second argument should be the port number which is a positive number.')))));chk_err),
			chk_err.			 			 

check_syntax(clone_agent,_GUID1,_Send_Ip,Send_Port,GUID2):-
            %catch((atom(GUID1)->true;assert(error),retract(ecnt(X)),X1 is X+1,assert(ecnt(X1)),throw(notAtom)),_Error10,(write('Error# ca1:'),writeln(' First argument should be the name of the agent to be cloned which is an atom.'))),      		
			%catch((integer(Send_Port)->true;assert(error),retract(ecnt(Y)),Y1 is Y+1,assert(ecnt(Y1)),throw(notInt)),_Error11,(write('Error# ca2:'),writeln(' Second argument should be the port number which is an integer.'))),
			catch((var(GUID2)->true;assert(error),retract(ecnt(Z)),Z1 is Z+1,assert(ecnt(Z1)),throw(notVar)),_Error12,(write('Error# ca3:'),writeln(' Last argument should be the new name of the agent which is a variable.'))),
			((integer(Send_Port)->(catch((pre1(Send_Port)),_Error51,(write('Error# ca4:'),writeln(' Second argument should be the port number which is a positive number.')))));chk_err),
			chk_err.
			
check_syntax(create_mobile_agent,_GUID,_Ip,Port,Handler,List):-
             catch((integer(Port)->true;assert(error),retract(ecnt(X)),X1 is X+1,assert(ecnt(X1)),throw(notInt)),_Error1600,(write('Error# ma1:'),writeln(' Second argument should be the port number which is an integer.'))),
			 catch((atom(Handler)->true;assert(error),retract(ecnt(Y)),Y1 is Y+1,assert(ecnt(Y1)),throw(notAtom)),_Error14,(write('Error# ma2:'),writeln(' Third argument should be the handler name which is an atom.'))), 
			 catch((is_list(List)->true;assert(error),retract(ecnt(Z)),Z1 is Z+1,assert(ecnt(Z1)),throw(notList)),_Error15,(write('Error # ma3:'),writeln(' Last argument should be a list of tokens.'))),
             ((is_list(List)->(catch((pre(List)),_Error56,(write('Error# ma4:'),writeln(' Last argument should be a list of tokens which is an integer.')))));true),
			 ((integer(Port)->(catch((pre1(Port)),_Error5300,(write('Error# ma5:'),writeln(' Second argument should be the port number which is a positive integer.')))));chk_err),chk_err.  
			 
			
check_syntax(create_static_agent,_Agent_name,_Platform_Ip,Port,Handler,List):-
            catch((integer(Port)->true;assert(error),retract(ecnt(X)),X1 is X+1,assert(ecnt(X1)),throw(notInt)),_Error16,(write('Error# sa1:'),writeln(' Second argument should be the port number which is an integer.'))),
			((integer(Port)->(catch((pre1(Port)),_Error53,(write('Error# sa2:'),writeln(' Second argument should be the port number which is a positive integer.')))));true),
			catch((atom(Handler)->true;assert(error),retract(ecnt(Y)),Y1 is Y+1,assert(ecnt(Y1)),throw(notAtom)),_Error17,(write('Error# sa3:'),writeln(' Third argument should be the handler name which is an atom.'))),		
			catch((is_list(List)->true;assert(error),retract(ecnt(Z)),Z1 is Z+1,assert(ecnt(Z1)),throw(notList)),_Error18,(write('Error# sa4:'),writeln(' Last argument should be a list of tokens.'))),
            ((is_list(List)->(catch((pre(List)),_Error55,(write('Error# sa5:'),writeln(' Last argument should be a list of tokens which is an integer.')))));chk_err),
			chk_err.  
			
check_syntax(execute_agent4,Agent_name,_Ip,Port,Handler,Start_function):-
            catch((atom(Agent_name)->true;assert(error),retract(ecnt(X)),X1 is X+1,assert(ecnt(X1)),throw(notAtom)),_Error20,(write('Error# ea5:'),writeln(' First argument should be the agent name which is an atom.'))),      		
			catch((integer(Port)->true;assert(error),retract(ecnt(Y)),Y1 is Y+1,assert(ecnt(Y1)),throw(notInt)),_Error21,(write('Error# ea6:'),writeln(' Second argument should be the port number which is an integer.'))),
            catch((atom(Handler)->true;assert(error),retract(ecnt(Z)),Z1 is Z+1,assert(ecnt(Z1)),throw(notAtom)),_Error22,(write('Error# ea7:'),writeln(' Third argument should be the handler name which is an atom.'))),
            catch((atom(Start_function)->true;assert(error),retract(ecnt(A)),A1 is A+1,assert(ecnt(A1)),throw(notAtom)),_Error23,(write('Error# ea8:'),writeln(' Last argument is the name of the parameter in the handler instead of "main" which is an atom.'))),
			((integer(Port)->(catch((pre1(Port)),_Error54,(write('Error# ea9:'),writeln(' Second argument should be the port number which is a positive integer.')))));chk_err),
			chk_err.	
%-----------------------------------------------------------------------------------------------------------------%
	
chk_err:- 
	   error,retractall(error),
	   chk_ecnt.	   

chk_err.	

chk_ecnt:-
           write('errors found: '),ecnt(Num_err),write(Num_err),nl,retract(ecnt(Num_err)), assert(ecnt(0)),abort.

chk_ecnt.	
%-----------------------------------------------------------------------------------------------------------------%

%-------- Program to access the elements of a list one by one and check whether it is an integer.--------------%
pre(List):-
      length(List,L), 
	  testloop(List,L).	
	  
testloop(_Token_list,0).
testloop(Token_list,N) :- N>0, nth1(N, Token_list, Elem),(integer(Elem)->true;assert(error),retract(ecnt(B)),B1 is B+1,assert(ecnt(B1)),throw(notInt)), M is N-1, testloop(Token_list,M).	  
%---------------------------------------------------------------------------------------------------------------%
% ----------------------To check whether the given port number is positive---------------------------------------%  
 pre1(Port):-
     ((Port>0)->true;assert(error),retract(ecnt(C)),C1 is C+1,assert(ecnt(C1)),throw(notPositive)).
%----------------------------------------------------------------------------------------------------------------%


