:- include('gtree.pro').
:- dynamic(names/2, ids/1, plevel/1, eoi/0).

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

resword(if).
resword(else).
resword(print).
resword(while).

:- initialization(hoc5x).

hoc5x :- 
    retractall(plevel(_)), assertz(plevel(0)), retractall(eoi),
       write('Enter an expression (Ctrl-D to exit)'), nl,
       lex(L), (eoi, !; input(T,L,[]), write(T), nl, hoc5x);
    write('*** Error'), nl, hoc5x.

%Run
%     hoc5g.
%to produce 'data.dot' file.
%Then use Graphvis by 'dot -Tpng -O data.dot'.
hoc5g :- lex(L), input(T,L,[]), gtree(T), !.

exec(R,S,P) :- P=[], ([R]=S, !; !);
   P=[H|T], 
   (
      H=while, !, S=[E1,E2|SR], exec(V,[],E1), 
         (V=0, NS=SR, NP=T, !; exec(_,[],E2), NS=S, NP=P), exec(_,NS,NP);
      (
         (
            H=uminus, !, S=[E|SR], V is -E;
            H=bltin, !, S=[I,N|SR], inifunc(I,Z,A), arg(1,Z,V), arg(2,Z,F), 
               set_arg(F,A,N,R1), arg(2,Z,R1), call(Z);
            H=id, !, S=[I|SR], names(I,V);
            =(H,=), !, S=[V,I|SR], (retract(names(I,_)), !; true), 
               assertz(names(I,V));
            =(H,&&), !, S=[E1,E2|SR], (E1=0, V=0, !; E2=0, V=0, !; V=1);
            =(H,'||'), !, S=[E1,E2|SR], (E1=0, E2=0, V=0, !; V=1);
            =(H,==), !, S=[E1,E2|SR], (E2=E1, V=1, !; V=0);
            H='!=', !, S=[E1,E2|SR], (E2=E1, V=0, !; V=1);
            =(H,>=), !, S=[E1,E2|SR], (E2>=E1, V=1, !; V=0);
            H='<=', !, S=[E1,E2|SR], (E2=<E1, V=1, !; V=0);
            =(H,>), !, S=[E1,E2|SR], (E2>E1, V=1, !; V=0);
            =(H,<), !, S=[E1,E2|SR], (E2<E1, V=1, !; V=0);
            =(H,+), !, S=[E1,E2|SR], V is E1 + E2;
            =(H,-), !, S=[E1,E2|SR], V is E2 - E1;
            =(H,*), !, S=[E1,E2|SR], V is E2*E1;
            =(H,/), !, S=[E1,E2|SR], V is E2/E1;
            =(H,!), !, S=[E|SR], (E=0, V=1, !; V=0);
            =(H,^), !, S=[E1,E2|SR], V is E2**E1
         ), [V|SR]=NS;
         H=if, !, S=[E1,E2|NS], exec(V,[],E1), (V=0, !; exec(_,[],E2));
         H=ifelse, !, S=[E1,E2,E3|NS], exec(V,[],E1), 
            (V=0, exec(_,[],E3), !; exec(_,[],E2));
         H=print, !, S=[E|NS], exec(V,[],E), write(V), nl;
         [H|S]=NS
      ), 
      exec(R,NS,T)
   ).

input(input(A)) -->  assign(A,T), {exec(_,[],T)}. %steadfastness forbids ! here :(
input(input(A)) -->  formula(A,T), !, {exec(R,[],T), write(R), nl}.
input(input(A)) -->  oper(A,T), {exec(_,[],T)}.
input(input(em)) --> [].

oper(oper(while,A1,A2),T) --> [while], !, cond(A1,T1), oper(A2,T2), {T=[T2,T1|[while]]}.
oper(oper(print,A),T) --> [print], !, formula(A,T1), {T=[T1|[print]]}.
oper(oper(if,A1,A2,else,A3),T) --> [if], cond(A1,T1), oper(A2,T2), 
   [else], !, oper(A3,T3), {T=[T3,T2,T1|[ifelse]]}.
oper(oper(if,A1,A2),T) --> [if], !, cond(A1,T1), oper(A2,T2), {T=[T2,T1|[if]]}.
oper(oper('{',A,'}'),T) -->   ['{'], !, operlist(A,T), ['}'].
oper(oper(A),T) --> formula(A,T).

operlist(operlist(A1,A2),T) --> oper(A1,T1), operlist(A2,T2), {append(T1,T2,T)}.
operlist(operlist(em),[]) --> [].

assign(assign(A1,=,A2),T) --> lval(A1,T1), [=], formula(A2,T2), {append(T1,T2,T3), append(T3,[=],T)}.

formula(formula(A1,A2),T) --> rel(A1,T1), formula_rest(A2,T2), {append(T1,T2,T)}.
formula(formula(A),T) --> assign(A,T).

formula_rest(formula_rest(&&,A1,A2),T) --> [&, &], !, rel(A1,T1), formula_rest(A2,T2), {append(T1,T2,T3), append(T3,[&&],T)}.
formula_rest(formula_rest('||',A1,A2),T) --> ['|', '|'], !, rel(A1,T1), formula_rest(A2,T2), {append(T1,T2,T3), append(T3,['||'],T)}.
formula_rest(formula_rest(em),[]) --> [].

rel(rel(A1,==,A2),T) --> expr(A1,T1), [=, =], !, expr(A2,T2), {append(T1,T2,T3), append(T3,[==],T)}.
rel(rel(A1,'!=',A2),T) --> expr(A1,T1), [!, =], !, expr(A2,T2), {append(T1,T2,T3), append(T3,['!='],T)}.
rel(rel(A1,'<=',A2),T) --> expr(A1,T1), [<, =], !, expr(A2,T2), {append(T1,T2,T3), append(T3,['<='],T)}.
rel(rel(A1,>=,A2),T) --> expr(A1,T1), [>, =], !, expr(A2,T2), {append(T1,T2,T3), append(T3,[>=],T)}.
rel(rel(A1,<,A2),T) --> expr(A1,T1), [<], !, expr(A2,T2), {append(T1,T2,T3), append(T3,[<],T)}.
rel(rel(A1,>,A2),T) --> expr(A1,T1), [>], !, expr(A2,T2), {append(T1,T2,T3), append(T3,[>],T)}.
rel(rel(A),T) --> expr(A,T).

expr(expr(A1,A2),T) --> term(A1,T1), expr_rest(A2,T2), {append(T1,T2,T)}.

expr_rest(expr_rest(+,A1,A2),T) --> 
   [+], !, term(A1,T1), expr_rest(A2,T2), {append(T1,T2,T3), append(T3,[+],T)}.
expr_rest(expr_rest(-,A1,A2),T) --> 
   [-], !, term(A1,T1), expr_rest(A2,T2), {append(T1,T2,T3), append(T3,[-],T)}.
expr_rest(expr_rest(em),[]) --> [].

term(term(A1,A2),T) --> factor(A1,T1), term_rest(A2,T2), {append(T1,T2,T)}.

term_rest(term_rest(*,A1,A2),T) --> 
   [*], !, factor(A1,T1), term_rest(A2,T2), {append(T1,T2,T3), append(T3,[*],T)}.
term_rest(term_rest(/,A1,A2),T) --> 
   [/], !, factor(A1,T1), term_rest(A2,T2), {append(T1,T2,T3), append(T3,[/],T)}.
term_rest(term_rest(em),[]) --> [].

factor(factor(A1,^,A2),T) --> element(A1,T1), [^], !, factor(A2,T2), {append(T1,T2,T3), append(T3,[^],T)}.
factor(factor(-,A),T) --> [-], !, factor(A,T1), {append(T1,[uminus],T)}.
factor(factor(+,A),T) --> [+], !, factor(A,T).
factor(factor(A),T) --> element(A,T).

cond(cond('(',A,')'),T) --> ['('], formula(A,T), [')'].

element(element(A),T) --> cond(A,T), !.
element(element(!,A),T) --> [!], !, element(A,T1), {append(T1,[!],T)}.
element(element(bltin(I,'(',A,')')),T) --> 
   bltin(I), ['('], !, expr(A,T1), [')'], {append(T1,[I],T2), append(T2,[bltin],T)}.
element(element(A),[T]) --> num(A,T), !.
element(element(A),T) --> id(A,T). 

num(num(N),N) --> [N], {number(N)}.

id(id(I),T) --> [I], {ids(I), T=[I|[id]]}.

lval(lval(I),T) --> [I], {ids(I), T=[I]}.

bltin(I) --> [I], {inifunc(I,_,_)}.

lex(L) :- 
   get_char(X),
   ((X=' '; X='\t'), !, lex(L);
   X=end_of_file, !, L=[], assertz(eoi);
   digit(X), unget_char(X), input_num(N),
      set_prolog_flag(syntax_error,fail), number_chars(N1,N), set_prolog_flag(syntax_error,warning),
      !, lex(E), L=[N1|E];
   letter(X), unget_char(X), input_id(N), atom_chars(N1,N), !, lex(E), L=[N1|E], (ids(N1), !; resword(N1), !; assertz(ids(N1)));
   X='{', retract(plevel(N)), N1 is N+1, assertz(plevel(N1)), !, lex(E), L=[X|E];
   X='}', retract(plevel(N)), N1 is N-1, assertz(plevel(N1)), !, lex(E), L=[X|E];
   X='\n', !, (plevel(0), L=[], !; lex(L));
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

