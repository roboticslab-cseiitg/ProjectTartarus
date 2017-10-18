loop(C):-
	(C<20 ->
			(0 is mod(C,2)->
				digitalWrite(1,0),
				delayMicroseconds(1000000),
				writeln('second'),
				millis(T),
				write(T),
				C1 is C+1,
				loop(C1)
			;
				digitalWrite(1,1),
				delayMicroseconds(1000000),
				writeln('first'),
				C1 is C+1,
				loop(C1)
			)
	).


start:-
	pinMode(1, 1),
	pinMode(0, 1),
	digitalWrite(0,0),
	digitalWrite(1,1),
	C is 1,
	loop(C),
	writeln('done').
