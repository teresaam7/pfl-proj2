display_game(state(Board, Player)) :-
    nl,
    write('           '),
    (Player = white -> write('Player O Playing') ; write('Player X Playing')), nl,
    write('                                    '), nl,
    write('   | 1 | 2 | 3 | 4 | 5 | 6 | 7 |'), nl,
    write('---|---|---|---|---|---|---|---|'), nl,
    print_board(Board, 1).

print_board([], _).
print_board([Row|Rows], N) :-
    header_line(Letter, N),
    write(' '), write(Letter), write(' | '),
    print_row(Row), nl,
    write('___|___|___|___|___|___|___|___|'), nl,
    N1 is N + 1,
    print_board(Rows, N1).

print_row([]).
print_row([Cell|Cells]) :-
    (Cell = empty -> write('.') ;
     Cell = white -> write('o') ;
     Cell = black -> write('v') ;
     Cell = 8 -> write('8') ;  % Stack branca
     Cell = x -> write('X')), % Stack preta
    write(' | '),
    print_row(Cells).


header_line('1', 1).
header_line('2', 2).
header_line('3', 3).
header_line('4', 4).
header_line('5', 5).
header_line('6', 6).
header_line('7', 7).