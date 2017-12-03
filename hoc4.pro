:-dynamic(names/2, ids/1).

names(pi, 3.1415926536).
names(e, 2.7182818284).
names(phi, 1.6180339887).

inifunc(sin, is(_,sin(_)), [1]).
inifunc(ln, is(_,log(_)), [1]).
inifunc(sqrt, is(_,sqrt(_)), [1]).
inifunc(arctg, is(_,atan(_)), [1]).
inifunc(log2, is(_,log(_)/log(2)), [1,1]).
inifunc(fib, fib(_,_), [0]).
   fib(R,N) :- fib1(R,N,1,0).
   fib1(R,C,N1,N2) :- C=1, R=N1, !; S is N1 + N2, C1 is C - 1, fib1(R,C1,S,N1).

:- initialization(hoc4).

hoc4 :- 
    write('Enter an expression (Ctrl-D to exit)'), nl, lex(L), (L=[], !; input(L,[]), hoc4);
    write('*** Error'), nl, hoc4.

exec(R,S,P) :- P=[], [R]=S, !;
   P=[H|T], 
   (
      (
         H=uminus, !, S=[E|SR], V is -E;
         H=bltin, !, S=[I,N|SR], inifunc(I,Z,A), arg(1,Z,V), arg(2,Z,F), 
            set_arg(F,A,N,R1), arg(2,Z,R1), call(Z);
         H=id, !, S=[I|SR], names(I,V);
         =(H,=), !, S=[V,I|SR], retractall(names(I,_)), assertz(names(I,V));
         =(H,+), !, S=[E1,E2|SR], V is E1 + E2;
         =(H,-), !, S=[E1,E2|SR], V is E2 - E1;
         =(H,*), !, S=[E1,E2|SR], V is E2*E1;
         =(H,/), !, S=[E1,E2|SR], V is E2/E1;
         =(H,^), !, S=[E1,E2|SR], V is E2**E1
      ), [V|SR]=NS;
      [H|S]=NS
   ), exec(R,NS,T).

input -->
   assign(T), !, {exec(_,[],T)};
   expr(N), !, {exec(R,[],N), write(R), nl};
   [].

assign(T) --> lval(T1), [=], expr(T2), {append(T1,T2,T3), append(T3,[=],T)}.

expr(T) --> term(T1), expr_rest(T2), {append(T1,T2,T)}.
expr(T) --> assign(T).

expr_rest(T) --> [+], !, term(T1), expr_rest(T2), {append(T1,T2,T3), append(T3,[+],T)}.
expr_rest(T) --> [-], !, term(T1), expr_rest(T2), {append(T1,T2,T3), append(T3,[-],T)}.
expr_rest([]) --> [].

term(T) --> factor(T1), term_rest(T2), {append(T1,T2,T)}.

term_rest(T) --> [*], !, factor(T1), term_rest(T2), {append(T1,T2,T3), append(T3,[*],T)}.
term_rest(T) --> [/], !, factor(T1), term_rest(T2), {append(T1,T2,T3), append(T3,[/],T)}.
term_rest([]) --> [].

factor(T) --> element(T1), [^], !, factor(T2), {append(T1,T2,T3), append(T3,[^],T)}.
factor(T) --> [-], !, factor(T1), {append(T1,[uminus],T)}.
factor(T) --> [+], !, factor(T).
factor(T) --> element(T).

cond(T) --> ['('], expr(T), [')'].

element(T) --> cond(T), !.
element(T) --> bltin(I), ['('], !, expr(T1), [')'], 
   {append(T1,[I],T2), append(T2,[bltin],T)}.
element([T]) --> num(T), !.
element(T) --> id(T). 

num(N) --> [N], {number(N)}.

id(T) --> [I], {ids(I), T=[I|[id]]}.

lval(T) --> [I], {ids(I), T=[I]}.

bltin(I) --> [I], {inifunc(I,_,_)}.

lex(L) :- 
   get_char(X),
   ((X=' '; X='\t'), !, lex(L);
   X=end_of_file, !, L=[];
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

