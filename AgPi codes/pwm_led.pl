loop2(K):-
	( K > -1 ->
		writeln(K),
		pwmWrite(1,K),
		delay(7),
		K1 is K-1,
		loop2(K1),
		!
	;
		writeln('Stop')
	).

loop(C):-
	( C < 1025 ->
		writeln(C),
		pwmWrite(1,C),
		delay(7),
		C1 is C+1,
		loop(C1),
		!
	;
		loop2(1024)
	).


start:-
	wiringPiSetup,
	pinMode(1,2),
	pinMode(0,1),
	digitalWrite(0,0),
	pwmSetMode(0),
	pwmSetRange(1024),
	pwmSetClock(32),
	writeln('b'),
	pwmWrite(1,1024),
	loop(1),
	writeln('done').
