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
       write('Enter an expression (Ctrl-D to exit)'), nl, lex(L), (eoi, !; input(L,[]), hoc3);
    write('*** Error'), nl, hoc3.

input -->
   assign(_), !;
   expr(N), !, {write(N), nl};
   [].

assign(N) --> id(I,_), [=], expr(N), {retract(names(I,_)), assertz(names(I,N))}.

expr(N) -->
   term(N1), expr_rest(N1,N);
   assign(N).

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
   bltin(I), ['('], !, expr(N1), [')'], 
      {inifunc(I,V,A), arg(1,V,N), arg(2,V,F), set_arg(F,A,N1,R), arg(2,V,R), call(V)};
   num(N), !;
   id(_,N). 

num(N) --> [N], {number(N)}.

id(I,N) --> [I], {ids(I), (names(I,N), !; N=0, assertz(names(I,N)))}.

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

