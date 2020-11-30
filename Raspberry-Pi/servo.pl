:- module(servo, [python_call/6, python_call/3, servo/4]).




python_call(File, Function, Arg1,Arg2,Arg3,Result) :-
    atomic_list_concat(['from ', File, ' import *; print ', Function, '(''',Arg1,''',''',Arg2,''',''',Arg3,''')'], Command),
    process_create(path('python'), ['-c', Command], [stdout(pipe(Out))]),
    read_lines(Out, Lines), last(Lines, Result).

python_call(File, Function, Arg) :-
    atomic_list_concat(['from ', File, ' import *; print(', Function, '(''', Arg, '''))'], Command),
    process_create(path('python'), ['-c', Command], [stdout(pipe(Out))]).


read_lines(Out, Lines) :-
    read_line_to_codes(Out, Line1),
    read_lines(Line1, Out, Lines).

read_lines(end_of_file, _, []) :- !.

read_lines(Codes, Out, [Line|Lines]) :-
    atom_codes(Line, Codes),
    read_line_to_codes(Out, Line2),
    read_lines(Line2, Out, Lines).

term_string(Term, String) :-
    term_to_atom(Term, Atom),
    atom_string(Atom, S),
    remove_quote(S, String).

remove_quote(S1, S2) :-
    atom_chars(S1, Chars),
    delete(Chars, '\'', RChars),
    atomic_list_concat(RChars, S2).

servo(Pin,Fr,Start,Result):-python_call('servo','init',Pin,Fr,Start,Result).
