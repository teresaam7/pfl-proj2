% Displays the current game state including the board and current player
display_game(state(Board, Player)) :-
    nl,
    board_width(Board, Width),
    center_text_spaces(Width, Player, Spaces),
    write_spaces(Spaces),
    display_player(Player),
    nl,
    write(' '),
    nl,
    print_column_headers(Board),
    print_horizontal_line(Board),
    reverse(Board, DisplayBoard),
    print_board(DisplayBoard, 1).

% Display text for players turn
display_player(white) :-
    write('Player *white* Playing').
display_player(black) :-
    write('Player *black* Playing').

board_width(Board, Width) :-
    length(Board, Size),
    Width is Size * 4 + 1.

center_text_spaces(BoardWidth, Player, Spaces) :-
    player_text_length(Player, TextLength),
    Spaces is (BoardWidth - TextLength) // 2.

player_text_length(white, 23). % "Player *white* Playing"
player_text_length(black, 23). % "Player *black* Playing"

write_spaces(0).
write_spaces(N) :-
    N > 0,
    write(' '),
    N1 is N - 1,
    write_spaces(N1).

% Print column numbers at the top of the board
print_column_headers(Board) :-
    length(Board, Size),
    write('    |'),
    print_column_numbers(1, Size),
    nl.

print_column_numbers(N, Size) :-
    N =< Size,
    format(' ~|~t~d~2+ |', [N]),
    N1 is N + 1,
    print_column_numbers(N1, Size).
print_column_numbers(_, _).

% Print horizontal separator line
print_horizontal_line(Board) :-
    length(Board, Size),
    write('____'),
    print_horizontal_segments(Size),
    nl.

print_horizontal_segments(0):-  write('|').
print_horizontal_segments(Size) :-
    write('|____'),
    Size1 is Size - 1,
    print_horizontal_segments(Size1).


print_board([], _).
print_board([Row|Rows], N) :-
    length(Rows, RemainingRows),
    TotalRows is RemainingRows + 1,
    header_line(TotalRows),
    write(' | '),
    print_row(Row), nl,
    print_horizontal_line(Row),
    N1 is N + 1,
    print_board(Rows, N1).

% Print different cell types in a row
print_row([]).
print_row([empty|Cells]) :-
    print_text(" ."),
    write(' | '),
    print_row(Cells).
print_row([white|Cells]) :-
    print_text(" "),
    print_text([0x25CB]), % ○
    write(' | '),
    print_row(Cells).
print_row([black|Cells]) :-
    print_text(" "),
    print_text([0x25CF]), % ●
    write(' | '),
    print_row(Cells).
print_row([8|Cells]) :-
    print_text([0x25CB, 0x25CB]), % ○○
    write(' | '),
    print_row(Cells).
print_row([x|Cells]) :-
    print_text([0x25CF, 0x25CF]), % ●●
    write(' | '),
    print_row(Cells).

% Print row number at start of line
header_line(Index) :-
    format('~|~t~d~3+', [Index]).

% Print Unicode characters
print_text([]) :- !.
print_text([H|T]) :-
    char_code(Char, H),
    write(Char),
    print_text(T).
