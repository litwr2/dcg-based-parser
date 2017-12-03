:- dynamic(eoi/0).
:- initialization(hoc1).

hoc1 :- 
    retractall(eoi),
       write('Enter an expression (Ctrl-D to exit)'), nl, lex(L), (eoi, !; input(L,[]), hoc1);
    write('*** Error'), nl, hoc1.

input --> 
   expr(N), !, {write(N), nl};
   [].

expr(N) --> term(N1), expr_rest(N1,N).

expr_rest(N1,N) --> 
   [+], !, term(N2), {N3 is N1+N2}, expr_rest(N3,N);
   [-], !, term(N2), {N3 is N1-N2}, expr_rest(N3,N).
expr_rest(N,N) --> [].

term(N) --> factor(N1), term_rest(N1,N).

term_rest(N1,N) --> 
   [*], !, factor(N2), {N3 is N1*N2}, term_rest(N3,N);
   [/], !, factor(N2), {N3 is N1/N2}, term_rest(N3,N).
term_rest(N,N) --> [].

factor(N) --> 
   element(N1), [^], !, factor(N2), {N is N1**N2};
   [-], !, factor(N1), {N is -N1};
   [+], !, factor(N);
   element(N).

element(N) --> 
   ['('], !, expr(N), [')'];
   num(N).

num(N) --> [N], {number(N)}.

lex(L) :- 
   get_char(X),
   ((X=' '; X='\t'), !, lex(L);
   X=end_of_file, !, L=[], assertz(eoi);
   digit(X), unget_char(X), input_num(N),
      set_prolog_flag(syntax_error,fail), number_chars(N1,N), set_prolog_flag(syntax_error,warning),
      !, lex(E), L=[N1|E];
   X='\n', !, L=[];
   lex(E), L=[X|E]).

digit(D) :- char_code(D,C), C > 47, C < 58.

input_num(L) :-
   get_char(X), ((digit(X), !; X='.'; X=e), !, input_num(N), L=[X|N]; unget_char(X), L=[]).

