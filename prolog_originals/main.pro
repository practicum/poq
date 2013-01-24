
main2 :-
        write('running main\n'),
        member(X,[a,b,c]),
        write(X),nl,
        write('hey'),nl,
        fail.


main :-
        get_char(user_input,C),
        doech(C).

doech(end_of_file) :- !.

doech(C) :-
        %char_code(C,E),
%        current_stream(2, _, StdErr),
        write(user_error,C),write(user_error,'xxxabc'),
%        current_stream(2, _, StdErr),
        nl(user_error),
        %put_char(StdErr,C),
        %put_char(StdErr,C),
        %put_char(StdErr,C),
        %nl(StdErr),
        %current_stream(1, _, StdErr),
%        ttyflush,
        skip('\n'),
        get_char(user_input,D),!,doech(D).


main3 :-
        writeln('wtf'),
        read(C),
        writeln('hey'),
        doech2(C).

doech2(end_of_file) :- !.

doech2(C) :-
        %char_code(C,E),
        writeln(C),writeln('xxxabc').
%,read(D),!,doech2(D).