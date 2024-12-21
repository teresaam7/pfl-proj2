display_game(state(Board, Player)) :-
    nl,
    write('           '),
    (Player = white -> write('Player O Playing') ; write('Player X Playing')), nl,
    write('                                    '), nl,
    write('    |  1 |  2 |  3 |  4 |  5 |  6 |  7 |'), nl,
    write('----|----|----|----|----|----|----|----|'), nl,
    reverse(Board, DisplayBoard),
    print_board(DisplayBoard, 1).

print_board([], _).
print_board([Row|Rows], N) :-
    header_line(Letter, N),
    write('  '), write(Letter), write(' | '),
    print_row(Row), nl,
    write('____|____|____|____|____|____|____|____|'), nl,
    N1 is N + 1,
    print_board(Rows, N1).

print_text([]) :- !.
print_text([H|T]) :-
    char_code(Char, H),
    write(Char),
    print_text(T).

print_row([]).
print_row([Cell|Cells]) :-
    (Cell = empty -> print_text(" .") ;
     Cell = white -> print_text(" ") , print_text([0x25CB]) ;    % ○
     Cell = black -> print_text(" ") , print_text([0x25CF]) ;    % ●
     Cell = 8 -> print_text([0x25CB, 0x25CB]) ; % ○○
     Cell = x -> print_text([0x25CF, 0x25CF])), % ●●
    write(' | '),
    print_row(Cells).



header_line('7', 1).
header_line('6', 2).
header_line('5', 3).
header_line('4', 4).
header_line('3', 5).
header_line('2', 6).
header_line('1', 7).