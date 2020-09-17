%this is a library to connect prolog and python
:-module(python_lib, [python_call/4, python_call/3]).

python_call(File, Function, Args, Result) :-
    term_string(Args, SArgs),
    atomic_list_concat(['from ', File, ' import *;print(', Function, '(''', SArgs, '''))'], Command),
    process_create(path('python'), ['-c', Command], [stdout(pipe(Out)),process(PID)]),
    %writeln('PID is ':PID),
    read_lines(Out, Lines), last(Lines, Result),process_release(PID),close(Out).

python_call(File, Function, Result) :-
    atomic_list_concat(['from ', File, ' import *;print(', Function, '())'], Command),
    process_create(path('python'), ['-c', Command], [stdout(pipe(Out))]),
    read_lines(Out, Lines), last(Lines, Result).

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
