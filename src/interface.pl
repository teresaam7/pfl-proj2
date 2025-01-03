% Mostra o tabuleiro com tamanho dinâmico
display_game(state(Board, Player)) :-
    nl,
    write('           '),
    (Player = white -> write('Player *white* Playing') ; write('Player *black* Playing')), nl,
    write('                                    '), nl,
    print_column_headers(Board),
    print_horizontal_line(Board),
    reverse(Board, DisplayBoard),
    print_board(DisplayBoard, 1).

% Imprime os números no topo do tabuleiro (colunas)
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

% Imprime linha horizontal dinâmica
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

% Imprime cada linha do tabuleiro
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

% Imprime células de uma linha
print_row([]).
print_row([Cell|Cells]) :-
    (Cell = empty -> print_text(" .") ;
     Cell = white -> print_text(" "), print_text([0x25CB]) ;    % ○
     Cell = black -> print_text(" "), print_text([0x25CF]) ;    % ●
     Cell = 8 -> print_text([0x25CB, 0x25CB]) ; % ○○
     Cell = x -> print_text([0x25CF, 0x25CF])), % ●●
     write(' | '),
    print_row(Cells).

% Gera cabeçalhos das linhas numericamente (de baixo para cima)
header_line(Index) :-
    format('~|~t~d~3+', [Index]).

% Função auxiliar para exibir texto
print_text([]) :- !.
print_text([H|T]) :-
    char_code(Char, H),
    write(Char),
    print_text(T).
