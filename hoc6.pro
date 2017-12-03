:- dynamic(names/2, ids/1, subrlist/2, procmode/0, returning/1, params/1, 
   flex/1, plex/1, eoi/0).

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
resword(proc).
resword(func).
resword(return).

params([]).

:- initialization(hoc6).

hoc6 :- 
    retractall(plevel(_)), assertz(plevel(0)), retractall(eoi),
       write('Enter an expression (Ctrl-D to exit)'), nl, lex(L),
       writeq(L), nl,
       (eoi, !; input(L,[]), hoc6);
    write('*** Error'), nl, hoc6.

calcparams(X,P) :-
   P=[], X=[], !;
   [H|T]=P, exec(R,[],H), calcparams(X1,T), X=[R|X1].

setparam(Res,C,L,P,V) :-
   C=P, L=[_|LR], [V|LR]=Res, !;
   C1 is C+1, L=[H|T], setparam(Res1,C1,T,P,V), Res=[H|Res1].

printparams(N,C) :-
   nth(C,N,E), !, exec(V,[],E), write(V), C1 is C + 1, printparams(N,C1);
   true.

commonsubr(I,N) :- subrlist(I,CODE), assertz(procmode),
   params(PL), calcparams(X,N), retract(params(_)), 
   assertz(params([X|PL])), exec(_,[],CODE), retract(params(_)), 
   assertz(params(PL)), retract(procmode).

exec(R,S,P) :- 
   P=[], ([R]=S, !; !);
   returning(R), !;
   P=[H|T], 
   (
      H=while, !, S=[E1,E2|SR], exec(V,[],E1), 
         (V=0, NS=SR, NP=T, !; exec(_,[],E2), NS=S, NP=P), exec(_,NS,NP);
      H=retproc, !, procmode, asserta(returning(_));
      H=retfunc, !, procmode, S=[V|SR], exec(E,[],V), asserta(returning(E));
      H=str, !, exec(R,S,T); 
      (
         (
            H=uminus, !, S=[E|SR], V is -E;
            H=bltin, !, S=[I,N|SR], inifunc(I,Z,A), arg(1,Z,V), arg(2,Z,F), 
               set_arg(F,A,N,R1), arg(2,Z,R1), call(Z);
            H=id, !, S=[I|SR], names(I,V);
            H=procid, !, procmode, [E|SR]=S, params([N|_]), nth(E,N,V);
            =(H,=), !, S=[V,I|SR], retractall(names(I,_)), 
               assertz(names(I,V));
            H='$=', !, procmode, S=[V,E|SR], retract(params(PL)), PL=[N|PR], 
               setparam(Res,1,N,E,V), assertz(params([Res|PR]));
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
            =(H,^), !, S=[E1,E2|SR], V is E2**E1;
            H=funccall, !, S=[I,N|SR], commonsubr(I,N), retract(returning(V))
         ), [V|SR]=NS;
         H=if, !, S=[E1,E2|NS], exec(V,[],E1), (V=0, !; exec(_,[],E2));
         H=ifelse, !, S=[E1,E2,E3|NS], exec(V,[],E1), 
            (V=0, exec(_,[],E3), !; exec(_,[],E2));
         H=print, !, S=[N|NS], printparams(N,1);
         H=proccall, !, S=[I,N|NS], commonsubr(I,N), retractall(returning(_));
         H=subr, !, S=[[ID],CODE|NS], retractall(subrlist(ID,_)), 
            assertz(subrlist(ID,CODE));
         [H|S]=NS
      ),
      exec(R,NS,T)
   ).

input -->
   assign(T), !, {exec(_,[],T)}; %steadfastness forbids ! here
   oper(T), !, {retractall(longoper), exec(_,[],T)};
   formula(T), !, {exec(R,[],T), write(R), nl};
   deffn(T), !, {exec(_,[],T)};
   [].

deffn(T) -->
   [func], !, lval(T1), ['('], [')'], 
      {(plex(T1); inifunc(T1,_,_)), !, fail; asserta(flex(T1))}, %retractall(flex(_))
      oper(T2), {T=[T2,T1|[subr]]};
   [proc], lval(T1), ['('], [')'], 
      {flex(T1), !, fail; asserta(plex(T1))}, 
      oper(T2), {T=[T2,T1|[subr]]}.

oper(T) --> 
   [while], !, cond(T1), oper(T2), {T=[T2,T1|[while]]};
   [print], prlist(T1), {T=[T1|[print]]}, !;
   [if], cond(T1), oper(T2), [else], !, oper(T3), {T=[T3,T2,T1|[ifelse]]};
   [if], !, cond(T1), oper(T2), {T=[T2,T1|[if]]};
   ['{'], !, {asserta(longoper)}, operlist(T), ['}'];
   proccall(T1), !, ['('], arglist(T2), [')'], {T=[T2,T1|[proccall]]};
   [return], formula(T1), !, {T=[T1|[retfunc]]};
   [return], !, {T=[retproc]};
   formula(T).

arglist(T) --> formula(T1), [','], !, arglist(T2), {[T1|T2]=T}.
arglist([T]) --> formula(T), !.
arglist([]) --> [].

operlist(T) --> oper(T1), operlist(T2), {append(T1,T2,T)}.
operlist([]) --> [].

assign(T) --> 
   lval(T1), [=], formula(T2), !, {append(T1,T2,T3), append(T3,[=],T)};
   procid(T1), [=], formula(T2), {T1=[N|_], [N|T2]=T3, append(T3,[$=],T)}.

formula(T) --> rel(T1), formula_rest(T2), {append(T1,T2,T)}.
formula(T) --> assign(T).

formula_rest(T) --> [&, &], !, rel(T1), formula_rest(T2), {append(T1,T2,T3), append(T3,[&&],T)}.
formula_rest(T) --> ['|', '|'], !, rel(T1), formula_rest(T2), {append(T1,T2,T3), append(T3,['||'],T)}.
formula_rest([]) --> [].

rel(T) --> expr(T1), [=, =], !, expr(T2), {append(T1,T2,T3), append(T3,[==],T)}.
rel(T) --> expr(T1), [!, =], !, expr(T2), {append(T1,T2,T3), append(T3,['!='],T)}.
rel(T) --> expr(T1), [<, =], !, expr(T2), {append(T1,T2,T3), append(T3,['<='],T)}.
rel(T) --> expr(T1), [>, =], !, expr(T2), {append(T1,T2,T3), append(T3,[>=],T)}.
rel(T) --> expr(T1), [<], !, expr(T2), {append(T1,T2,T3), append(T3,[<],T)}.
rel(T) --> expr(T1), [>], !, expr(T2), {append(T1,T2,T3), append(T3,[>],T)}.
rel(T) --> expr(T).

expr(T) --> term(T1), expr_rest(T2), {append(T1,T2,T)}.

expr_rest(T) --> [+], !, term(T1), expr_rest(T2), {append(T1,T2,T3), append(T3,[+],T)}.
expr_rest(T) --> [-], !, term(T1), expr_rest(T2), {append(T1,T2,T3), append(T3,[-],T)}.
expr_rest([]) --> [].

term(T) --> factor(T1), term_rest(T2), {append(T1,T2,T)}.

term_rest(T) --> 
   [*], !, factor(T1), term_rest(T2), {append(T1,T2,T3), append(T3,[*],T)}.
term_rest(T) --> 
   [/], !, factor(T1), term_rest(T2), {append(T1,T2,T3), append(T3,[/],T)}.
term_rest([]) --> [].

factor(T) --> element(T1), [^], !, factor(T2), {append(T1,T2,T3), append(T3,[^],T)}.
factor(T) --> [-], !, factor(T1), {append(T1,[uminus],T)}.
factor(T) --> [+], !, factor(T).
factor(T) --> element(T).

cond(T) --> ['('], formula(T), [')'].

element(T) --> cond(T), !.
element(T) --> [!], !, element(T1), {append(T1,[!],T)}.
element(T) --> 
   bltin(I), ['('], !, expr(T1), [')'], {append(T1,[I],T2), append(T2,[bltin],T)}.
element(T) --> funccall(T1), !, ['('], arglist(T2), [')'], {T=[T2,T1|[funccall]]}.
element([T]) --> num(T), !.
element(T) --> procid(T), !.
element(T) --> id(T). 

prlist(T) --> 
   formula(T1), [','], !, prlist(T2), {T=[T1|T2]};
   formula(T1), {T=[T1]}, !;
   string(T1), [','], !, prlist(T2), {T=[T1|T2]};
   string(T1), {T=[T1]}.

num(N) --> [N], {number(N)}.

id(T) --> [I], {ids(I), T=[I|[id]]}.

procid(T) --> ['$'], num(T1), {T=[T1|[procid]]}.

lval(T) --> [I], {ids(I), T=[I]}.

bltin(I) --> [I], {inifunc(I,_,_)}.

funccall(I) --> [I], {flex([I])}.

proccall(I) --> [I], {plex([I])}.

string(S) --> ['"'], [S1], {S=[S1|[str]]}.

lex(L) :- 
   get_char(X),
   ((X=' '; X='\t'), !, lex(L);
   X=end_of_file, !, L=[], assertz(eoi);
   digit(X), unget_char(X), input_num(N),
      set_prolog_flag(syntax_error,fail), number_chars(N1,N), set_prolog_flag(syntax_error,warning),
      !, lex(E), L=[N1|E];
   letter(X), unget_char(X), input_id(N), atom_chars(N1,N), !, lex(E), L=[N1|E], (ids(N1), !; resword(N1), !; assertz(ids(N1)));
   X='"', !, input_string(S), lex(E), atom_chars(A,S), append(['"'|[A]],E,L);
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

input_string(L) :-
   get_char(X), (X=('\\'), get_escaped(Y), !, input_string(N), L=[Y|N]; X='"', L=[], !; input_string(N), L=[X|N]).

get_escaped(Z) :-
   get_char(Y),(
      Y=n, Z='\n', !;
      Y=t, Z='\t', !;
      Y=f, Z='\f', !;
      Y=r, Z='\r', !;
      Y=b, Z='\b', !;
      Y='"', Z=Y, !;
      Y=('\\'), Z=Y, !;
      Y=end_of_file, Z=('\\'), !;
      unget_char(Y), Z=('\\')).

set_arg(V,A,N1,R) :-
   A=[0], !, R=N1;
   A=[H], !, arg(H,V,N1), R=V;
   A=[H|T], arg(H,V,F), set_arg(F,T,N1,F1), arg(H,V,F1), R=V.

