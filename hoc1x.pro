:- dynamic(eoi/0).
:- include(gtree).
:- initialization(hoc1x).

hoc1x :-
    retractall(eoi),
       write('Enter an expression (Ctrl-D to exit)'), nl,
       lex(L), writeq(L), nl, (eoi, !; input(T,L,[]), write(T), nl, hoc1x);
    write('*** Error'), nl, hoc1x.

%Run 
%     hoc1g.
%to produce 'data.dot' file.
%Then use Graphvis by 'dot -Tpng -O data.dot'.
hoc1g :- lex(L), input(T,L,[]), gtree(T), !.

input(input(T)) --> expr(T,N), !, {write(N), nl}.
input(input(em)) --> [].

expr(expr(T1,T2),N) --> term(T1,N1), expr_rest(T2,N1,N).
expr_rest(expr_rest(+,T1,T2),N1,N) --> [+], !, term(T1,N2), {N3 is N1+N2}, expr_rest(T2,N3,N).
expr_rest(expr_rest(-,T1,T2),N1,N) --> [-], !, term(T1,N2), {N3 is N1-N2}, expr_rest(T2,N3,N).
expr_rest(expr_rest(em),N,N) --> [].

term(term(T1,T2),N) --> factor(T1,N1), term_rest(T2,N1,N).
term_rest(term_rest(*,T1,T2),N1,N) --> [*], !, factor(T1,N2), {N3 is N1*N2}, term_rest(T2,N3,N).
term_rest(term_rest(/,T1,T2),N1,N) --> [/], !, factor(T1,N2), {N3 is N1/N2}, term_rest(T2,N3,N).
term_rest(term_rest(em),N,N) --> [].

factor(factor(T,^,F),N) --> element(T,N1), [^], !, factor(F,N2), {N is N1**N2}.
factor(factor(-,T),N) --> [-], !, factor(T,N1), {N is -N1}.
factor(factor(+,T),N) --> [+], !, factor(T,N).
factor(factor(T),N) --> element(T,N).

element(element('(',E,')'),N) --> ['('], !, expr(E,N), [')'].
element(element(N1),N) --> num(N1,N).

num(number(N),N) --> [N], {number(N)}.

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

