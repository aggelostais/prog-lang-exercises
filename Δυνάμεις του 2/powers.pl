%Metraei ta 1 se lista. Tha klithei os once(count_ones(lista,X))
%kanontas bound to apotelesma sti X
count_ones([],0).
count_ones([1|T],Count):- count_ones(T,N),Count is 1+N.
count_ones([_|T],Count):- count_ones(T,Count).

%Pairnei enan akeraio kai epistrefei ti dyadiki tou anaparastasi se pinaka.
%Kaleitai os dec_bin(number,X). opou tha ginei bound sto X.
dec_bin(0,[0]).
dec_bin(1,[1]).
dec_bin(N,B):-N>1,X is N mod 2,Y is N//2,dec_bin(Y,B1),append(B1,[X], B).

%Pairnei lista kai epistrefei ti thesi sth lista tou protou mh 0 stoixeiou
%H arithmisi ton theseon arxizei apo to 1
%Se periptosi pou bgalei size+1 tote ola einai 0 sti lista
%klisi first_non_zero(lista,S)
first_non_zero([0|T],S):-first_non_zero(T,X),S is X + 1.
first_non_zero(_,1).

%help_break(index,lista,teliki lista(unbound))
help_break([],_,[]).
help_break(List,Index,List):-Index =< 0.
help_break([Head|Tail],1,[Result|Tail]):- Result is Head - 1.
help_break([Head|Tail],2,[Result|B1]):-
    Result is Head + 2, help_break(Tail,1,B1).
help_break([Head|Tail],Index,[Head|B1]):-
    New_index is Index -1,
    help_break(Tail,New_index,B1).

%breakin(dyadiki anaparstasi,k(dynameis tou 2)-arithmos_1_sti_dyadiki,X)
%Epistrefei ton pinaka apotelesma anapoda
breaking(Result,Times,Result):-Times =<0.
breaking([Head|Tail],Times,Result):-
  New_Times is Times-1,
  first_non_zero(Tail,Y),
  Index is 1 + Y,
  help_break([Head|Tail],Index,X),
  breaking(X,New_Times,Result).

%Epistrefei ti lista apotelesmatos xoris ta midenika sto telos.
%cut_out_zeros(lista,X(unbound),k)
cut_out_zeros(_,[],0).
cut_out_zeros([Head|Tail],Result,K):-
  New_K is K - Head,
  cut_out_zeros(Tail,New_Result,New_K),
  append([Head],New_Result,Result).

%Painrei tous 2 arithmous kai epistrefei ti lista sto teleutaio orisma
powers(N,K,[]):-K>N.
powers(N,K,[]):-
  dec_bin(N,Binary),
  count_ones(Binary,Ones),
  K<Ones.
powers(N,K,Result):-
  dec_bin(N,Binary2),
  reverse(Binary2,Binary), %kakos kodikas(gia na doulepsei thelei to binary anapoda)
  count_ones(Binary,Ones),
  breaking(Binary,K-Ones,Dirty_Result),
  cut_out_zeros(Dirty_Result,Result,K).


powers2(File, Answers) :-
    once(read_file(File, Answers)).

    % code for reading

    read_file(File, Answers) :-
        open(File, read, Stream),
        % writeln("file opened"),
        read_line(Stream, [Size]),
        %size=arithmos gramon kai arithmon pou tha diaxeiristei
        % write("size = "),
        % writeln(Size),
        while(File, Stream, Size, [], Answers).
        %H keni lista tha gemisei kai tha antigrafei sto answers

    while(_,_,I, CurrAns, CurrAns) :- %Termatiki synthiki while Size<1, antigrafei th lista
        I < 1.
    while(File, Stream, Size, CurrAns, Answers) :-
        Size > 0,
        % write("on while "), writeln(Size),
        read_input(File, Stream, N, K),
        powers(N,K,Answer),
        %writeln(Answer),
        append(CurrAns, [Answer], NextAnswers),
        !,
        NextSize is Size - 1,
        while(File, Stream, NextSize, NextAnswers, Answers).

    read_input(_File, Stream, N, M) :-
        % open(File, read, Stream),
        read_line(Stream, [N, M]),
        !.

    read_line(Stream, L) :-
        read_line_to_codes(Stream, Line),
        atom_codes(Atom, Line),
        atomic_list_concat(Atoms, ' ', Atom),
        maplist(atom_number, Atoms, L).
    % end of reading code
