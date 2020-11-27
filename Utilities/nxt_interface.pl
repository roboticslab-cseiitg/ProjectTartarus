/*
Copyright (C) 2016  Robotics Lab., IIT Guwahati

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

%============================================================================================================================================
% Authors :: Vivek Singh 10010172
%			 Manoj Bode   
% Date : 08022015
%
% Project Supervisor : Dr. S.B. Nair.
%
% This is a SWI-Prolog interface for Lego MindStrom NXT Brick running on Lejos Firmware.
%
% (c) Robotics Lab, CSE Department, IIT-Guwahti.
%=============================================================================================================================================

%To download the initial files to the nxt brick
:- dynamic name_socket/4. %% used for storing name and socket of connection

my_number_string(Number,String):-
	atom_number(String,Number).
	

setup_nxt(NXT):-
	writeln('Copying nxtbin file to NXT...'),
	join_str('C:\\Tartarus\\copy_files.bat ',NXT,' ',R1),
	join_str(R1,'nxtbin',' ',R2),
	atom_string(Cmd1,R2),
	win_exec(Cmd1,normal),
	write('Check nxtbin.nxj file on NXT'),nl,!.


%Start the nxt, java server and create the connection stream Link is Ip address
initialize_nxt(Name,Link,Port,Res):-
	my_number_string(Port,P),
	join_str('C:\\Tartarus\\run_server.bat ',P,' ',R3),												%send Robot Name too as a parameter - Nikhil 30/4/2016
	atom_string(Cmd,R3),
	win_exec(Cmd,normal),
	write('Initiating connection with nxt...'),nl,
	%atom_string(Link,Link2),
	connect2_nxt(Name,Link,Port,Res).


%To read the response from the java server the response comes as => 123XMessage
%read_nxt(_Soc,_Text).
read_nxt(ResText,Output):-
	format(codes(A),'~q.',[ResText]),
	string_codes(Text,A),
	nth0(Index,A,45),
	sub_atom(Text,0,Index,_,Output),!.

%Connecting with the java server with error handling
connect2_nxt(Name,Link,Port,T):-
	atom(Name)->
		(atom(Link)->                                 %%%%%%%%%%%%%%%%%%%%%% Atom to string
			(number(Port)->
				(atomic(T)->write('Error: 4th Agrument must be Unbound Variable'),nl,abort;
					connect_nxt(Name,Link,Port),sleep(0.1)->write('Waiting for reply...'),nl,
					read_res_nxt(Name,T),
					(T='failed'->
						print_message(error,'Please check the NXT'),
						fail;
						nl),
					write('==================== SWI-PROLOG-NXT INTERFACE ==================='),nl,
					write('====  Developed by Robotics Lab, Department of CSE, IIT Guwahati, INDIA  ===='),nl,
					write('================================================================'),nl,!;
					write('Error: Connection Failed, Check if port is available'),nl,abort);
				write('Error: 3rd Argument must be number'),nl,abort);
			write('Error: 2nd Argument must be atom'),nl,abort);
		write('Error: 1st Argument must be atom'),nl,abort.

%Creating a socket and connecting with the java server
connect_nxt(Name,Link,Port):-
	atom_string(Link2,Link),
	tcp_socket(Socket),
	tcp_connect(Socket,Link2:Port),
	retractall(name_socket(Name,_,_,_)),
	tcp_open_socket(Socket,In,Out),
	asserta(name_socket(Name,Socket,In,Out)),!.

%To write to the java server, i.e., on the socket
written_nxt(Name,Msg):-
	string_codes(Msg,List),
	append(List,[36],Nlist),
	string_codes(M,Nlist),
	atom_string(M2,M),
	name_socket(Name,_,In,Out),
	%tcp_open_socket(Socket,In,Out),
	format(Out,'~q',[M2]),
	flush_output(Out),
	read(In,Rep),
	read_nxt(Rep,_Output).

socket_recv_code(StreamPair, String, Length) :-
	(   at_end_of_stream(StreamPair)
	->  String = "",
	    Length = -1
	;   read_pending_input(StreamPair, String, []),
	    length(String, Length)
	).


%Write with error handling
write_nxt(Name,Msg):-
	%catch(Error,
	written_nxt(Name,Msg),!.
	%throw(Error,start_nxt(Name,_Link,_Port,_Response)).

%To close the complete connection or session
close_nxt(Name):- write_nxt(Name,'@'),
	name_socket(Name,Socket,In,Out),
	retractall(name_socket(Name,Socket,_,_)),
	close(Out,[force(true)]),
	close(In,[force(true)]),
	tcp_close_socket(Socket),!.

%To join two string str1 and str2 with a seperator Sep and gives the final string Fstr.
join_str(Str1,Str2,Sep,Fstr):-
	string_codes(Str1,S1),
	string_codes(Str2,S2),
	string_codes(Sep,S3),
	append(S1,S3,Slist),
	append(Slist,S2,Flist),
	string_codes(Fstr,Flist).

%To move the nxt in forward direction
move_forward_nxt(Name,Port,Speed):-
	atom(Name)->
		(atom(Port)->
			(number(Speed)->
				my_number_string(Speed,S),
				join_str('0',Port,'-',R1),
				join_str(R1,S,'-',R2),
				write_nxt(Name,R2),!;
				write('Error: in Speed, must be number'),nl,abort);
			write('Error: in Port,must be atom A,B,C'),nl,abort);   %%%%%%%%%% Atom to string
		write('Error: in connection specifier '),nl,abort.

%To Move forward with two ports at a time
move_forward_nxt(Name,Port1,Port2,Speed):-
	move_forward_nxt(Name,Port1,Speed),
	move_forward_nxt(Name,Port2,Speed),!.

%To move the nxt in backward direction
move_backward_nxt(Name,Port,Speed):-
	atom(Name)->
		(atom(Port)->
			(number(Speed)->
				my_number_string(Speed,S),
				join_str('1',Port,'-',R1),
				join_str(R1,S,'-',R2),
				write_nxt(Name,R2),!;
				write('Error: in Speed, must be number'),nl,abort);
			write('Error: in Port,must be atom A,B,C'),nl,abort);
		write('Error: in connection specifier '),nl,abort.

%Move backward with two ports at a time
move_backward_nxt(Name,Port1,Port2,Speed):-move_backward_nxt(Name,Port1,Speed),
							 move_backward_nxt(Name,Port2,Speed),!.

%To float the nxt to stop
flt_nxt(Name,Port):-atom(Name)->
						(
								atom(Port)->join_str('2',Port,'-',R1),write_nxt(Name,R1),!;
								write('Error: in Port,must be atom A,B,C'),nl,abort
						);
						write('Error: in connection specifier '),nl,abort.

%with two ports
flt_nxt(Name,Port1,Port2):-
	flt_nxt(Name,Port1),
	flt_nxt(Name,Port2),!.

%To Stop the nxt motor
stop_nxt(Name,Port):-
	atom(Name)->
		(atom(Port)->
			join_str('3',Port,'-',R1),
			write_nxt(Name,R1),!;
			write('Error: in Port,must be atom A,B,C'),nl,abort);
		write('Error: in connection specifier '),nl,abort.


%Two motors at a time
stop_nxt(Name,Port1,Port2):-
	stop_nxt(Name,Port1),
	stop_nxt(Name,Port2),!.


%To Rotate the nxt motor by some degrees in clockwise or anticlockwise direction
rotate_nxt(Name,Port,Angle,Direction):-
	atom(Name)->
		(atom(Port)->
			(number(Angle)->
				(atom(Direction)->
					my_number_string(Angle,A),
					join_str('4',Port,'-',R0),
					join_str(R0,A,'-',R1),
					join_str(R1,Direction,'-',R2),
					write_nxt(Name,R2),!;
					write('Error: in Direction,must be atom A for anticlockwise and C for clockwise'),nl,abort);
				write('Error: in Angle,must be a number'),nl,abort);
			write('Error: in Port,must be atom A,B,C'),nl,abort);
		write('Error: in connection specifier '),nl,abort.

%To rotate a Motor (not the robot itself) upto a certain angle
rotateto_nxt(Name,Port,Angle,Sign):-
	atom(Name)->
		(atom(Port)->
			(number(Angle)->
				(atom(Sign)->
					my_number_string(Angle,A),
					join_str('61',Port,'-',R0),
					join_str(R0,A,'-',R1),
					join_str(R1,Sign,'-',R2),
					write_nxt(Name,R2),!;
					write('Error: in Direction,must be atom A for anticlockwise and C for clockwise'),nl,abort);
				write('Error: in Angle,must be a number'),nl,abort);
			write('Error: in Port, must be atom A,B,C'),nl,abort);
		write('Error: in connection specifier '),nl,abort.


%To set the speed of a motor to a certain value
set_motorspeed_nxt(Name,Port,Speed):-
	atom(Name)->
		(atom(Port)->
			(number(Speed)->
				my_number_string(Speed,S),
				join_str('64',Port,'-',R1),
				join_str(R1,S,'-',R2),
				write_nxt(Name,R2),!;
				write('Error: in Speed, must be number'),nl,abort);
			write('Error: in Port, must be atom A,B,C'),nl,abort);
		write('Error: in connection specifier '),nl,abort.


%To set the speed of two motors at a time
set_motorspeed_nxt(Name,Port1,Port2,Speed1,Speed2):-
	set_motorspeed_nxt(Name,Port1,Speed1),
	set_motorspeed_nxt(Name,Port2,Speed2),!.

%To set the acceleration of a motor to a certain value
set_acceleration_nxt(Name,Port,Accel):-
	atom(Name)->
		(atom(Port)->
			(number(Accel)->
				my_number_string(Accel,A),
				join_str('65',Port,'-',R1),
				join_str(R1,A,'-',R2),
				write_nxt(Name,R2),!;
				write('Error: in Acceleration, must be number'),nl,abort);
			write('Error: in Port, must be a atom A,B,C'),nl,abort);
		write('Error: in connection specifier '),nl,abort.

%To set the acceleration of two motors at a time
set_acceleration_nxt(Name,Port1,Port2,Accel1,Accel2):-
	set_acceleration_nxt(Name,Port1,Accel1),
	set_acceleration_nxt(Name,Port2,Accel2),!.

%To reverse the direction of the motor movement
reverse_nxt(Name,Port):-
	atom(Name)->
		(atom(Port)->
			join_str('5',Port,'-',R1),
			write_nxt(Name,R1),!;
			write('Error: in Port,must be atom A,B,C'),nl,abort);
		write('Error: in connection specifier '),nl,abort.

%reversing two motors at a time
reverse_nxt(Name,Port1,Port2):-
	reverse_nxt(Name,Port1),
	reverse_nxt(Name,Port2),!.

%write on the LCD screen of the NXT
write_lcd_nxt(Name,Corx,Cory,String):-
	atom(Name)->
		(number(Corx)->
			(number(Cory)->
				(atom(String)->
					my_number_string(Corx,X),
					my_number_string(Cory,Y),
					join_str('7',X,'-',R1),
					join_str(R1,Y,'-',R2),
					join_str(R2,String,'-',R3),
					write_nxt(Name,R3),!;
					write('Error: in Last argument, must be a atom'),nl,abort);
				write('Error: in Co-ordinate, must be a number'),nl,abort);
			write('Error: in Co-ordinate, must be a number'),nl,abort);
		write('Error: in connection specifier '),nl,abort.



%Clear the LCD screen of the NXT
clear_lcd_nxt(Name):-
	atom(Name)->
		join_str('8','','-',R1),
		write_nxt(Name,R1),!;
		write('Error: in connection specifier '),
		nl,abort.

%Make the NXT Wait for the time specified
wait_nxt(Name,Time):-
	atom(Name)->
		(number(Time)->
			my_number_string(Time,T),
			join_str('10',T,'-',R1),
			write_nxt(Name,R1),!;
			write('Error: in Time, must be a number'),nl,abort);
		write('Error: in connection specifier '),nl,abort.

%Execute a program on the NXT
exec_nxt(Name,Program):-
	atom(Name)->
		(atom(Program)->
			join_str('11',Program,'-',R1),
			write_nxt(Name,R1),!;
			write('Error: in program name,must be a atom'),nl,abort);
		write('Error: in connection specifier '),nl,abort.

%Delete a file (Program) present on the NXT
del_nxt(Name,Program):-
	atom(Name)->
		(atom(Program)->
			join_str('13',Program,'-',R1),
			write_nxt(Name,R1),!;
			write('Error: in program name,must be a atom'),nl,abort);
		write('Error: in connection specifier '),nl,abort.

%To check if a Program(File) is present on nxt or NOT
exists_nxt(Name,Program,Res):-
	atom(Name)->
		(atom(Program)->
			join_str('12',Program,'-',R0),
			join_str('#',R0,'-',R1),
			write_nxt(Name,R1),nl,
			read_res_nxt(Name,Res),!;
			write('Error: in program name,must be a atom'),nl,abort);
		write('Error: in connection specifier '),nl,abort.

%Download a program on to the NXT, File is filename and path if the location of the file
download_nxt(Name,File,Path):-
	atom(Name)->
		(atom(File)->
			(atom(Path)->
				join_str('14',File,'-',R0),
				join_str(R0,Path,'-',R1),
				write_nxt(Name,R1),!;
				write('Error: in Path'),nl,abort);
			write('Error: in file name'),nl,abort);
		write('Error: in connection specifier '),nl,abort.

%To check if any response is coming from nxt or not
check_res_nxt(Name,T):-
	read_nxt(Name,T)->
		write('response');
		write('no response'),!.

%This Predicates waits until it reads a message from the Java server
read_res_nxt(Name,T):-
	name_socket(Name,_,In,_Out),
	read(In,Rep),
	read_nxt(Rep,T)-> !;
	read_res_nxt(Name,T).

%To check if a motor is moving or not
ismoving_nxt(Name,Port,Res):-
	atom(Name)->
		(atom(Port)->
			join_str('6',Port,'-',R0),
			join_str('#',R0,'-',R1),
			write_nxt(Name,R1),nl,
			read_res_nxt(Name,R),
			my_number_string(Res,R),!;
			write('Error: in port, must be a atom A/B/C'),nl,abort);
		write('Error: in connection specifier '),nl,abort.

%To check two motors at a time for movement
ismoving_nxt(Name,Port1,Port2,Res1,Res2):-
	ismoving_nxt(Name,Port1,Res1),
	ismoving_nxt(Name,Port2,Res2),!.

%To get the mode of the motor movement on a port, The response is a numeric code
getmode_nxt(Name,Port,Res):-
	atom(Name)->
		(atom(Port)->
			join_str('9',Port,'-',R0),
			join_str('#',R0,'-',R1),
			write_nxt(Name,R1),nl,
			read_res_nxt(Name,R),
			my_number_string(Res,R),!;
			write('Error: in port, must be a atom A/B/C'),nl,abort);
		write('Error: in connection specifier '),nl,abort.

%To get the mode of movement of two motors at a time
getmode_nxt(Name,Port1,Port2,Res1,Res2):-
	getmode_nxt(Name,Port1,Res1),
	getmode_nxt(Name,Port2,Res2),!.


%To initialize a sensor in a sensor port
init_light_nxt(Name,Port):-
	atom(Name)->
		(number(Port)->
			my_number_string(Port,P),
			join_str('20',P,'-',R1),
			write_nxt(Name,R1),!;
			write('Error: in port number,must be a number between 1 to 4'),nl,abort);
		write('Error: in connection specifier '),nl,abort.

init_sound_nxt(Name,Port):-
	atom(Name)->
		(number(Port)->
			my_number_string(Port,P),join_str('21',P,'-',R1),write_nxt(Name,R1),!;
			write('Error: in port number,must be a number between 1 to 4'),nl,abort);
		write('Error: in connection specifier '),nl,abort.

init_ultrasonic_nxt(Name,Port):-
	atom(Name)->
		(number(Port)->
			my_number_string(Port,P),join_str('22',P,'-',R1),write_nxt(Name,R1),!;
			write('Error: in port number,must be a number between 1 to 4'),nl,abort);
		write('Error: in connection specifier '),nl,abort.

init_touch_nxt(Name,Port):-
	atom(Name)->
		(number(Port)->
			my_number_string(Port,P),join_str('23',P,'-',R1),write_nxt(Name,R1),!;
			write('Error: in port number,must be a number between 1 to 4'),nl,abort);
		write('Error: in connection specifier '),nl,abort.

init_color_nxt(Name,Port):-
	atom(Name)->
		(number(Port)->
			my_number_string(Port,P),join_str('48',P,'-',R1),write_nxt(Name,R1),!;
			write('Error: in port number,must be a number between 1 to 4'),nl,abort);
		write('Error: in connection specifier '),nl,abort.

init_compass_nxt(Name,Port):-
	atom(Name)->
		(number(Port)->
			my_number_string(Port,P),join_str('49',P,'-',R1),write_nxt(Name,R1),!;
			write('Error: in port number,must be a number between 1 to 4'),nl,abort);
		write('Error: in connection specifier '),nl,abort.

init_gyro_nxt(Name,Port):-
	atom(Name)->
		(number(Port)->
			my_number_string(Port,P),join_str('50',P,'-',R1),write_nxt(Name,R1),!;
			write('Error: in port number,must be a number between 1 to 4'),nl,abort);
		write('Error: in connection specifier '),nl,abort.

init_irseeker_nxt(Name,Port):-
	atom(Name)->
		(number(Port)->
			my_number_string(Port,P),join_str('54',P,'-',R1),write_nxt(Name,R1),!;
			write('Error: in port number,must be a number between 1 to 4'),nl,abort);
		write('Error: in connection specifier '),nl,abort.

init_accel_nxt(Name,Port):-
	atom(Name)->
		(number(Port)->
			my_number_string(Port,P),
			join_str('59',P,'-',R1),
			write_nxt(Name,R1),!;
			write('Error: in port number,must be a number between 1 to 4'),nl,abort);
		write('Error: in connection specifier '),nl,abort.



%To initialize a multiple light sensor
init_light_nxt(Name,Port1,Port2):-
	init_light_nxt(Name,Port1),
	init_light_nxt(Name,Port2),!.

init_light_nxt(Name,Port1,Port2,Port3):-
	init_light_nxt(Name,Port1),
	init_light_nxt(Name,Port2),
	init_light_nxt(Name,Port3),!.

init_light_nxt(Name,Port1,Port2,Port3,Port4):-
	init_light_nxt(Name,Port1),
	init_light_nxt(Name,Port2),
	init_light_nxt(Name,Port3),
	init_light_nxt(Name,Port4),!.

%To initialize a multiple sound sensor
init_sound_nxt(Name,Port1,Port2):-
	init_sound_nxt(Name,Port1),
	init_sound_nxt(Name,Port2),!.

init_sound_nxt(Name,Port1,Port2,Port3):-
	init_sound_nxt(Name,Port1),
	init_sound_nxt(Name,Port2),
	init_sound_nxt(Name,Port3),!.

init_sound_nxt(Name,Port1,Port2,Port3,Port4):-
	init_sound_nxt(Name,Port1),
	init_sound_nxt(Name,Port2),
	init_sound_nxt(Name,Port3),
	init_sound_nxt(Name,Port4),!.

%To initialize a multiple ultrasonic sensor
init_ultrasonic_nxt(Name,Port1,Port2):-
	init_ultrasonic_nxt(Name,Port1),
	init_ultrasonic_nxt(Name,Port2),!.

init_ultrasonic_nxt(Name,Port1,Port2,Port3):-
	init_ultrasonic_nxt(Name,Port1),
	init_ultrasonic_nxt(Name,Port2),
	init_ultrasonic_nxt(Name,Port3),!.

init_ultrasonic_nxt(Name,Port1,Port2,Port3,Port4):-
	init_ultrasonic_nxt(Name,Port1),
	init_ultrasonic_nxt(Name,Port2),
	init_ultrasonic_nxt(Name,Port3),
	init_ultrasonic_nxt(Name,Port4),!.

%To initialize a multiple touch sensor
init_touch_nxt(Name,Port1,Port2):-
	init_touch_nxt(Name,Port1),
	init_touch_nxt(Name,Port2),!.

init_touch_nxt(Name,Port1,Port2,Port3):-
	init_touch_nxt(Name,Port1),
	init_touch_nxt(Name,Port2),
	init_touch_nxt(Name,Port3),!.

init_touch_nxt(Name,Port1,Port2,Port3,Port4):-
	init_touch_nxt(Name,Port1),
	init_touch_nxt(Name,Port2),
	init_touch_nxt(Name,Port3),
	init_touch_nxt(Name,Port4),!.

%To set the flood light of light sensor On or Off; State is On or Off
floodlight_nxt(Name,Port,State):-
	atom(Name)->
		(number(Port)->
			my_number_string(Port,P),
			join_str('29',P,'-',R0),
			join_str(R0,State,'-',R1),
			write_nxt(Name,R1),!;
			write('Error: in port number,must be a number between 1 to 4'),nl,abort);
		write('Error: in connection specifier '),nl,abort.

%To read sensors at specified ports
read_light_nxt(Name,Port,Res):-
	atom(Name)->
		(number(Port)->
			my_number_string(Port,P),join_str('24',P,'-',R0),join_str('#',R0,'-',R1),
			write_nxt(Name,R1),read_res_nxt(Name,R),my_number_string(Res,R),!;
			write('Error: in port number,must be a number between 1 to 4'),nl,abort);
		write('Error: in connection specifier '),nl,abort.


read_ultrasonic_nxt(Name,Port,Res):-
	atom(Name)->
		(number(Port)->
			my_number_string(Port,P),join_str('26',P,'-',R0),join_str('#',R0,'-',R1),
			write_nxt(Name,R1),read_res_nxt(Name,R),my_number_string(Res,R),!;
			write('Error: in port number,must be a number between 1 to 4'),nl,abort);
		write('Error: in connection specifier '),nl,abort.


read_sound_nxt(Name,Port,Res):-
	atom(Name)->
		(number(Port)->
			my_number_string(Port,P),join_str('25',P,'-',R0),join_str('#',R0,'-',R1),
			write_nxt(Name,R1),read_res_nxt(Name,R),my_number_string(Res,R),!;
			write('Error: in port number,must be a number between 1 to 4'),nl,abort);
		write('Error: in connection specifier '),nl,abort.


read_touch_nxt(Name,Port,Res):-
	atom(Name)->
		(number(Port)->
			my_number_string(Port,P),join_str('27',P,'-',R0),join_str('#',R0,'-',R1),
			write_nxt(Name,R1),read_res_nxt(Name,R),my_number_string(Res,R),!;
			write('Error: in port number,must be a number between 1 to 4'),nl,abort);
		write('Error: in connection specifier '),nl,abort.


read_color_nxt(Name,Port,Res):-
	atom(Name)->
		(number(Port)->
			my_number_string(Port,P),join_str('51',P,'-',R0),join_str('#',R0,'-',R1),
			write_nxt(Name,R1),read_res_nxt(Name,R),my_number_string(Res,R),!;
			write('Error: in port number,must be a number between 1 to 4'),nl,abort);
		write('Error: in connection specifier '),nl,abort.


read_compass_nxt(Name,Port,Res):-
	atom(Name)->
		(number(Port)->
			my_number_string(Port,P),join_str('52',P,'-',R0),join_str('#',R0,'-',R1),
			write_nxt(Name,R1),read_res_nxt(Name,R),my_number_string(Res,R),!;
			write('Error: in port number,must be a number between 1 to 4'),nl,abort);
		write('Error: in connection specifier '),nl,abort.


read_gyro_nxt(Name,Port,Res):-
	atom(Name)->
		(number(Port)->
			my_number_string(Port,P),join_str('53',P,'-',R0),join_str('#',R0,'-',R1),
			write_nxt(Name,R1),read_res_nxt(Name,R),my_number_string(Res,R),!;
			write('Error: in port number,must be a number between 1 to 4'),nl,abort);
		write('Error: in connection specifier '),nl,abort.


read_irseeker_nxt(Name,Port,Res):-
	atom(Name)->
		(number(Port)->
			my_number_string(Port,P),join_str('55',P,'-',R0),join_str('#',R0,'-',R1),
			write_nxt(Name,R1),read_res_nxt(Name,R),my_number_string(Res,R),!;
			write('Error: in port number,must be a number between 1 to 4'),nl,abort);
		write('Error: in connection specifier '),nl,abort.


read_accel_nxt(Name,Port,Res1,Res2,Res3):-
	atom(Name)->
		(number(Port)->
			my_number_string(Port,P),join_str('60',P,'-',R0),join_str('#',R0,'-',R1),
			write_nxt(Name,R1),read_res_nxt(Name,R),my_number_string(Res1,R),
			read_res_nxt(Name,R),my_number_string(Res2,R),
			read_res_nxt(Name,R),my_number_string(Res3,R),!;
			write('Error: in port number,must be a number between 1 to 4'),nl,abort);
		write('Error: in connection specifier '),nl,abort.



read_sensorport_nxt(Name,Port,Res):-
	atom(Name)->
		(number(Port)->
			my_number_string(Port,P),join_str('28',P,'-',R0),join_str('#',R0,'-',R1),
			write_nxt(Name,R1),read_res_nxt(Name,R),my_number_string(Res,R),!;
			write('Error: in port number,must be a number between 1 to 4'),nl,abort);
		write('Error: in connection specifier '),nl,abort.


%To Calibrate Compass Sensor
start_compasscalibration_nxt(Name,Port):-
	atom(Name)->
		(number(Port)->
			my_number_string(Port,P),join_str('56',P,'-',R1),write_nxt(Name,R1),!;
			write('Error: in port number,must be a number between 1 to 4'),nl,abort);
		write('Error: in connection specifier '),nl,abort.

stop_compasscalibration_nxt(Name,Port):-
	atom(Name)->
		(number(Port)->
			my_number_string(Port,P),join_str('57',P,'-',R1),write_nxt(Name,R1),!;
			write('Error: in port number,must be a number between 1 to 4'),nl,abort);
		write('Error: in connection specifier '),nl,abort.

%To set Offset for Gyro sensor
set_gyroffset_nxt(Name,Port):-
	atom(Name)->
		(number(Port)->
			my_number_string(Port,P),join_str('58',P,'-',R1),write_nxt(Name,R1),!;
			write('Error: in port number,must be a number between 1 to 4'),nl,abort);
		write('Error: in connection specifier '),nl,abort.


%To produce the beep sound in the NXT; Type can be 1, 2, 3 & 4.
beep_nxt(Name,Type):-
	atom(Name)->
		(number(Type)->
			my_number_string(Type,T),join_str('30',T,'-',R1),write_nxt(Name,R1),!;
			write('Error: in beep type,must be a number between 1 to 4'),nl,abort);
		write('Error: in connection specifier '),nl,abort.



%To read multiple sensors at a time
read_light_nxt(Name,Port1,Port2,R1,R2):-
	read_light_nxt(Name,Port1,R1),
	read_light_nxt(Name,Port2,R2),!.

read_sound_nxt(Name,Port1,Port2,R1,R2):-
	read_sound_nxt(Name,Port1,R1),
	read_sound_nxt(Name,Port2,R2),!.

read_ultrasonic_nxt(Name,Port1,Port2,R1,R2):-
	read_ultrasonic_nxt(Name,Port1,R1),
	read_ultrasonic_nxt(Name,Port2,R2),!.

read_touch_nxt(Name,Port1,Port2,R1,R2):-
	read_touch_nxt(Name,Port1,R1),
	read_touch_nxt(Name,Port2,R2),!.

%get the files currently present on the NXT
get_files_nxt(Name,Res):-
	atom(Name)->
		join_str('47','Files','-',R0),
		join_str('#',R0,'-',R1),
		write_nxt(Name,R1),nl,
		read_res_nxt(Name,Res),
		write('Files on NXT : '),
		nl,write(Res),nl,
		write('**********************************************'),nl,!;
		write('Error: in connection specifier '),nl,abort.



%for creating the .pc file.

create_pc:- save_predicates( [read_nxt,setup_nxt,initialize_nxt,connect2_nxt,written_nxt,connect_nxt,write_nxt,close_nxt,join_str,move_forward_nxt,move_backward_nxt,flt_nxt,stop_nxt,rotate_nxt,reverse_nxt,write_lcd_nxt,clear_lcd_nxt,wait_nxt,exec_nxt,del_nxt,exists_nxt,download_nxt,check_res_nxt,read_res_nxt,ismoving_nxt,getmode_nxt,init_light_nxt,init_sound_nxt,init_ultrasonic_nxt,init_touch_nxt,floodlight_nxt,read_light_nxt,read_ultrasonic_nxt,read_sound_nxt,read_touch_nxt,read_sensorport_nxt,beep_nxt,get_files_nxt,set_motorspeed_nxt,set_acceleration_nxt,rotateto_nxt,init_color_nxt,read_color_nxt,init_compass_nxt,read_compass_nxt,init_gyro_nxt,read_gyro_nxt,init_irseeker_nxt,read_irseeker_nxt,init_accel_nxt,start_compasscalibration_nxt,stop_compasscalibration_nxt],nxt_interface ).
