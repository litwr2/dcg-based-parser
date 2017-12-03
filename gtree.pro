parse_functor(P,T,C) :-
   functor(T,N,L),
   number_chars(C,TL), atom_chars(N4,TL), atom_concat(P,'x',Q), atom_concat(Q,N4,N1),
   write(N1), write(' [label="'),
   (N='em',
       !;
       write(N)),
   (L > 0,
       write('"];\n'), parse_arg(N1,T,L,1), !;
       write('",shape="plaintext"];\n')),
   (P='',
       !;
       write(P), write(' -> '), write(N1), write(';\n')).

parse_arg(P,T,L,C) :- arg(C,T,X),
   parse_functor(P,X,C), C1 is C + 1, C < L,
   parse_arg(P,T,L,C1), !;
   true.

gtree(S) :-
   open('data.dot',write,Z), set_output(Z),
   write('digraph Parse {\nnode [shape="box"];\n'),
   parse_functor('',S,1),
   write('}\n'), close(Z), set_output(user_output),
   write('Graph data saved\n').

