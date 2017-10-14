:-style_check(-singleton).
:-assert(slave(no)).

:-dynamic master_handler/3.
master_handler(guid,(I,P),main):-
writeln('=================Human_heat Agent======================'),
writeln('ALERT!!! Human activity reported at the Server room!!!'),
writeln('=================================================='),!.

master_handler(guid,(I,P),gyro):-
writeln('===%%****Vibration Agent reporting*********======='),
writeln('ALERT!!! Activity reported at the Server room!!!'),
writeln('=================================================='),!.

:-dynamic pir_handler/3.
pir_handler(guid,(Ip,P),main):-
			add_token(guid,[1212]),
			add_payload(guid,[(init_pir,2),(read_motion,2)]),
			platform_port(Port),slave(X),

			(X=no->
				agent_move(guid,('192.168.1.7',8888))
			;
				init_pir(guid,7),
				writeln('System Initiated..'),
				writeln('Reading human motion--->>'),
				read_motion(guid,7)
			),!.

			
:-dynamic init_pir/2.
init_pir(guid,Inpin):-
		pinMode(Inpin,0),
		writeln('PIR connected to input pin: ':Inpin),!.

:-dynamic read_motion/2.
read_motion(guid,Pin):-
	digitalRead(Pin,Data),
	(Data=1->
		writeln('Human Motion detected! Sending alert to the master..'),
		agent_post(platform,('192.168.1.3',9999),[master_handler,master,('192.168.1.7',8888),main]),
		sleep(1.5),read_motion(guid,Pin)
	;

	sleep(1.5),read_motion(guid,Pin)
	),!.
	

:-dynamic vibration_handler/3.
vibration_handler(guid,(Ip,P),main):-
		add_token(guid,[1212]),
		add_payload(guid,[(init_mpu,3),(read_gyro,2),(poll_gyro,1)]),
		platform_port(Port),slave(X),

			(X=no->
				agent_move(guid,('192.168.1.7',8888))
			;
				writeln('Vibration_agent initiated!!'),
				init_mpu(guid,0x68,F),
				poll_gyro(guid)
				
			),!.	
            
:-dynamic init_mpu/3.
init_mpu(guid,Addr,Fd):-
		wiringPiI2CSetup(Addr,Fd),
		wiringPiI2CWriteReg8(Fd,0x6b,0x00),
		wiringPiI2CWriteReg8(Fd,0x6c,0x00),
		wiringPiI2CWriteReg8(Fd,0x74,0x00),
		retractall(fd(_)),assert(fd(Fd)),
		writeln('Fd is ':Fd),!.

:-dynamic read_gyro/2.
read_gyro(guid,Data):-
		fd(Fd),wiringPiI2CReadReg8(Fd,0x43,D1),D2 is D1<<8,
		wiringPiI2CReadReg8(Fd,0x44,D3),D4 is D2\/D3,
		Data is round(D4/16384),!.

:-dynamic poll_gyro/1.
poll_gyro(guid):-
		sleep(0.2),read_gyro(guid,D),
		(D<1->writeln('alert'),
		agent_post(platform,('192.168.1.3',9999),[master_handler,master,('192.168.1.7',8888),gyro])
		;
		writeln(D)
		),
		poll_gyro(guid),!.