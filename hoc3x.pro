:- include('gtree.pro').
:- dynamic(names/2, ids/1, eoi/0).

names(pi, 3.1415926536).
names(e, 2.7182818284).
names(phi, 1.6180339887).

inifunc(sin, _ is sin(_), [1]).
inifunc(ln, _ is log(_), [1]).
inifunc(sqrt, _ is sqrt(_), [1]).
inifunc(arctg, _ is atan(_), [1]).
inifunc(log2, _ is log(_)/log(2), [1,1]).
inifunc(fib, fib(_,_), [0]).
   fib(R,N) :- fib1(R,N,1,0).
   fib1(R,C,N1,N2) :- C=1, R=N1, !; S is N1 + N2, C1 is C - 1, fib1(R,C1,S,N1).

:- initialization(hoc3).

hoc3 :-
    retractall(eoi),
       write('Enter an expression (Ctrl-D to exit)'), nl,
       lex(L), (eoi, !; input(T,L,[]), write(T), nl, hoc3);
    write('*** Error'), nl, hoc3.

%Run
%     hoc3g.
%to produce 'data.dot' file.
%Then use Graphvis by 'dot -Tpng -O data.dot'.
hoc3g :- lex(L), input(T,L,[]), gtree(T), !.

input(input(A)) --> assign(A,_), !. %steadfastness forbids ! here
input(input(E)) --> expr(E,N), {write(N), nl}.
input(input(em)) --> [].

assign(assign(L,=,A),N) --> 
   id(L,I,_), [=], !, expr(A,N), {retract(names(I,_)), assertz(names(I,N))}.

expr(expr(T1,T2),N) --> term(T1,N1), expr_rest(T2,N1,N).
expr(expr(A),N) --> assign(A,N).

expr_rest(expr_rest(+,T1,T2),N1,N) --> [+], !, term(T1,N2), {N3 is N1+N2}, expr_rest(T2,N3,N).
expr_rest(expr_rest(-,T1,T2),N1,N) --> [-], !, term(T1,N2), {N3 is N1-N2}, expr_rest(T2,N3,N).
expr_rest(expr_rest(em),N,N) --> [].

term(term(T1,T2),N) --> factor(T1,N1), term_rest(T2,N1,N).

term_rest(term_rest(*,T1,T2),N1,N) --> [*], !, factor(T1,N2), {N3 is N1*N2}, term_rest(T2,N3,N).
term_rest(term_rest(/,T1,T2),N1,N) --> [/], !, factor(T1,N2), {N3 is N1/N2}, term_rest(T2,N3,N).
term_rest(term_rest(em),N,N) --> [].

factor(factor(T,^,F),N) --> element(T,N1), [^], !, factor(F,N2), {N is N1**N2}.
factor(factor(-,E),N) --> [-], !, factor(E,N1), {N is -N1}.
factor(factor(+,E),N) --> [+], !, factor(E,N1), {N is -N1}.
factor(factor(T),N) --> element(T,N).

element(element('(',E,')'),N) --> ['('], !, expr(E,N), [')'].
element(element(bltin(I,'(',E,')')),N) --> bltin(I), ['('], !, expr(E,N1), [')'], 
   {inifunc(I,V,A), arg(1,V,N), arg(2,V,F), set_arg(F,A,N1,R), arg(2,V,R), call(V)}.
element(element(N1),N) --> num(N1,N), !.
element(element(I),N) --> id(I,_,N). 

num(number(N),N) --> [N], {number(N)}.

id(id(I),I,N) --> [I], {ids(I), (names(I,N), !; N=0, assertz(names(I,N)))}.

bltin(I) --> [I], {inifunc(I,_,_)}.

lex(L) :-
   get_char(X),
   ((X=' '; X='\t'), !, lex(L);
   X=end_of_file, !, L=[], assertz(eoi);
   digit(X), unget_char(X), input_num(N),
      set_prolog_flag(syntax_error,fail), number_chars(N1,N), set_prolog_flag(syntax_error,warning),
      !, lex(E), L=[N1|E];
   letter(X), unget_char(X), input_id(N), atom_chars(N1,N), !, lex(E), L=[N1|E], (ids(N1), !; assertz(ids(N1)));
   X='\n', !, L=[];
   lex(E), L=[X|E]).

digit(D) :- char_code(D,C), C > 47, C < 58.

letter(S) :- S @>= 'A', S @=< 'Z'; S @>= 'a', S @< 'z'; S == '_'.

input_num(L) :-
   get_char(X), ((digit(X), !; X='.'; X=e), !, input_num(N), L=[X|N]; unget_char(X), L=[]).

input_id(L) :-
   get_char(X), ((digit(X), !; letter(X)), !, input_id(N), L=[X|N]; unget_char(X), L=[]).

set_arg(V,A,N1,R) :-
   A=[0], !, R=N1;
   A=[H], !, arg(H,V,N1), R=V;
   A=[H|T], arg(H,V,F), set_arg(F,T,N1,F1), arg(H,V,F1), R=V.

