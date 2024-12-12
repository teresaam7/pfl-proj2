
% Start the game with given players
start_game(Player1, Player2) :-
    initial_state(GameState),
    game_loop(GameState, Player1, Player2).

% Define initial state (empty board and starting player)
initial_state(state(Board, white)) :-
    empty_board(Board).

% Create an empty 7x7 board
empty_board(Board) :-
    length(Board, 7),
    maplist(empty_row, Board).

empty_row(Row) :-
    length(Row, 7),
    maplist(=(empty), Row).

% Game loop
game_loop(GameState, Player1, Player2) :-
    display_game(GameState),
    game_over(GameState, Winner),
    (Winner \= none ->
        nl, format("Game over! Winner: ~w~n", [Winner])
    ;
        GameState = state(_, CurrentPlayer),
        (CurrentPlayer = white -> CurrentPlayerType = Player1 ; CurrentPlayerType = Player2),
        choose_move(GameState, CurrentPlayerType, Move),
        move(GameState, Move, NewGameState),
        game_loop(NewGameState, Player1, Player2)
    ).

% Display the game state
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
    (Cell = empty -> write('.') ; (Cell = white -> write('O') ; write('X'))),
    write(' | '),
    print_row(Cells).


header_line('1', 1).
header_line('2', 2).
header_line('3', 3).
header_line('4', 4).
header_line('5', 5).
header_line('6', 6).
header_line('7', 7).

% Determine a move (based on player type)
choose_move(GameState, human, Move) :-
    nl, write('Enter your move (Row, Col): '),
    read((Row, Col)),
    Move = move(Row, Col).

choose_move(GameState, computer(1), Move) :-
    valid_moves(GameState, Moves),
    random_member(Move, Moves).

% Custom random_member/2 implementation
random_member(Element, List) :-
    length(List, Length),
    Length > 0,  % Ensure the list is not empty
    random_between(1, Length, Index),
    nth1(Index, List, Element).


% Custom random_between/3 implementation using system time for randomness
random_between(Low, High, Value) :-
    statistics(runtime, [Time|_]),  % Get the runtime in milliseconds
    Range is High - Low + 1,
    Value is Low + Time mod Range.  % Use modulus to generate a value in the range


choose_move(GameState, computer(2), Move) :-
    valid_moves(GameState, Moves),
    best_move(GameState, Moves, Move).

% Validate and execute a move
move(state(Board, Player), move(Row, Col), state(NewBoard, NextPlayer)) :-
    valid_moves(state(Board, Player), Moves),
    member(move(Row, Col), Moves),
    apply_move(Board, Row, Col, Player, NewBoard),
    next_player(Player, NextPlayer).

% Apply the move to the board
apply_move(Board, Row, Col, Player, NewBoard) :-
    nth1(Row, Board, OldRow),
    replace_nth(Col, OldRow, Player, NewRow),
    replace_nth(Row, Board, NewRow, NewBoard).

% Replace the Nth element in a list
replace_nth(1, [_|Rest], Elem, [Elem|Rest]).
replace_nth(N, [X|Xs], Elem, [X|Ys]) :-
    N > 1,
    N1 is N - 1,
    replace_nth(N1, Xs, Elem, Ys).

% Determine valid moves
valid_moves(state(Board, _), Moves) :-
    findall(move(Row, Col), valid_position(Board, Row, Col), Moves).

valid_position(Board, Row, Col) :-
    nth1(Row, Board, BoardRow),
    nth1(Col, BoardRow, Cell),
    Cell = empty.

% Check if the game is over
game_over(state(Board, _), Winner) :-
    (line_of_stacks(Board, Winner) -> true ;
    board_full(Board) -> Winner = draw ;
    Winner = none).

line_of_stacks(Board, Winner) :-
    (check_lines(Board, Winner) ;        % Check rows
     check_columns(Board, Winner) ;     % Check columns
     check_diagonals(Board, Winner)).   % Check diagonals

check_lines(Board, Winner) :-
    member(Row, Board),
    check_line(Row, Winner).

check_columns(Board, Winner) :-
    transpose(Board, TransposedBoard),
    check_lines(TransposedBoard, Winner).

check_diagonals(Board, Winner) :-
    diagonal(Board, Diagonals),
    member(Diagonal, Diagonals),
    check_line(Diagonal, Winner).

check_line(Line, Winner) :-
    append(_, [X,X,X|_], Line),
    X \= empty,
    Winner = X.

board_full(Board) :-
    \+ (member(Row, Board), member(empty, Row)).

% Determine the next player
next_player(white, black).
next_player(black, white).

% AI: Find the best move
best_move(GameState, Moves, BestMove) :-
    GameState = state(_, Player),
    findall(Value-Move, (member(Move, Moves), simulate_move(GameState, Move, Value)), MovesWithValues),
    max_member(_-BestMove, MovesWithValues).

simulate_move(GameState, Move, Value) :-
    move(GameState, Move, NewGameState),
    value(NewGameState, _, Value).

% Evaluate the game state (simple greedy heuristic)
value(state(Board, Player), Player, Value) :-
    count_pieces(Board, Player, Value).

count_pieces(Board, Player, Count) :-
    flatten(Board, Pieces),
    include(=(Player), Pieces, PlayerPieces),
    length(PlayerPieces, Count).

% Extract all diagonals from a board
diagonal(Board, Diagonals) :-
    findall(Diag, diagonal_down(Board, Diag), Diagonals1),
    findall(Diag, diagonal_up(Board, Diag), Diagonals2),
    append(Diagonals1, Diagonals2, Diagonals).

% Extract diagonals sloping downwards (top-left to bottom-right)
diagonal_down(Board, Diagonal) :-
    length(Board, N),
    between(1, N, StartRow),
    diagonal_down_from(Board, StartRow, 1, Diagonal).
diagonal_down(Board, Diagonal) :-
    length(Board, N),
    between(2, N, StartCol),
    diagonal_down_from(Board, 1, StartCol, Diagonal).

diagonal_down_from(Board, Row, Col, []) :-
    length(Board, N),
    (Row > N ; Col > N).
diagonal_down_from(Board, Row, Col, [Elem|Rest]) :-
    nth1(Row, Board, RowList),
    nth1(Col, RowList, Elem),
    NextRow is Row + 1,
    NextCol is Col + 1,
    diagonal_down_from(Board, NextRow, NextCol, Rest).

% Extract diagonals sloping upwards (bottom-left to top-right)
diagonal_up(Board, Diagonal) :-
    length(Board, N),
    between(1, N, StartRow),
    diagonal_up_from(Board, StartRow, 1, Diagonal).
diagonal_up(Board, Diagonal) :-
    length(Board, N),
    between(2, N, StartCol),
    diagonal_up_from(Board, N, StartCol, Diagonal).

diagonal_up_from(Board, Row, Col, []) :-
    (Row < 1 ; Col > 7). % Board size is fixed at 7x7.
diagonal_up_from(Board, Row, Col, [Elem|Rest]) :-
    nth1(Row, Board, RowList),
    nth1(Col, RowList, Elem),
    NextRow is Row - 1,
    NextCol is Col + 1,
    diagonal_up_from(Board, NextRow, NextCol, Rest).

between(Low, High, Low) :-
    Low =< High.
between(Low, High, Value) :-
    Low < High,
    Next is Low + 1,
    between(Next, High, Value).