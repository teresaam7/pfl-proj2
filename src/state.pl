
initial_state(state(Board, white)) :-
    empty_board(Board).

empty_board(Board) :-
    length(Board, 7),
    maplist(empty_row, Board).

empty_row(Row) :-
    length(Row, 7),
    maplist(=(empty), Row).

game_loop(GameState, Player1, Player2) :-
    display_game(GameState),
    game_over(GameState, Winner),
    (Winner \= none ->
        nl, format("Game over! Winner: ~w~n", [Winner])
    ;
        GameState = state(_, CurrentPlayer),
        (CurrentPlayer = white -> CurrentPlayerType = Player1 ; CurrentPlayerType = Player2),
        choose_move(GameState, CurrentPlayerType, Move),
        move(GameState, Move, TempGameState),
        handle_line_of_three(TempGameState, CurrentPlayerType, NewGameState),
        game_loop(NewGameState, Player1, Player2)
    ).


start_game(Player1, Player2) :-
    initial_state(GameState),
    game_loop(GameState, Player1, Player2).

choose_move(GameState, computer(1), Move) :-
    valid_moves(GameState, Moves),
    random_member(Move, Moves).

choose_move(GameState, computer(2), Move) :-
    valid_moves(GameState, Moves),
    best_move(GameState, Moves, Move).

choose_move(GameState, human, Move) :-
    nl, write('Enter your move (Row, Col): '),
    read((Row, Col)),
    Move = move(Row, Col).

move(state(Board, Player), move(Row, Col), state(NewBoard, NextPlayer)) :-
    valid_moves(state(Board, Player), Moves),
    member(move(Row, Col), Moves),
    apply_move(Board, Row, Col, Player, NewBoard),
    next_player(Player, NextPlayer).

apply_move(Board, Row, Col, Player, NewBoard) :-
    nth1(Row, Board, OldRow),
    replace_nth(Col, OldRow, Player, NewRow),
    replace_nth(Row, Board, NewRow, NewBoard).

replace_nth(1, [_|Rest], Elem, [Elem|Rest]).
replace_nth(N, [X|Xs], Elem, [X|Ys]) :-
    N > 1,
    N1 is N - 1,
    replace_nth(N1, Xs, Elem, Ys).

valid_moves(state(Board, _), Moves) :-
    findall(move(Row, Col), valid_position(Board, Row, Col), Moves).

valid_position(Board, Row, Col) :-
    nth1(Row, Board, BoardRow),
    nth1(Col, BoardRow, Cell),
    Cell = empty.

handle_line_of_three(state(Board, Player), PlayerType, state(NewBoard, Player)) :-
    find_lines_of_three(Board, Player, Lines),
    Lines \= [],
    (
        PlayerType = human -> 
            choose_two_to_remove(Lines, ToRemove, StackPos)
        ;
        PlayerType = computer(_) -> 
            choose_best_removal(Lines, ToRemove, StackPos)
    ),
    update_board(Board, ToRemove, StackPos, Player, NewBoard).
handle_line_of_three(GameState, _, GameState).


choose_best_removal([Line|_], ToRemove, StackPos) :-
    Line = [(R1, C1), (R2, C2), (R3, C3)],
    AllPositions = [(R1, C1), (R2, C2), (R3, C3)],
    random(0, 3, StackIndex),  
    nth0(StackIndex, AllPositions, StackPos),
    select(StackPos, AllPositions, RemainingPositions),
    ToRemove = RemainingPositions,
    
    write('To Remove: '), write(ToRemove), nl,
    write('Stack Position: '), write(StackPos), nl.

find_lines_of_three(Board, Player, Lines) :-
    setof(Line, 
          (check_lines(Board, Player, Line); 
           check_columns(Board, Player, Line); 
           check_diagonals(Board, Player, Line)), 
          Lines),
    print_lines(Lines).

print_lines([]).
print_lines([Line | Rest]) :-
    write('Found line: '), 
    write(Line), nl,  % Escreve a linha encontrada
    print_lines(Rest).  % Processa o restante das linhas

check_lines(Board, Player, Line) :-
    nth1(RowIdx, Board, Row),
    check_line(Row, RowIdx, Player, Line).

check_columns(Board, Player, Line) :-
    transpose(Board, TransposedBoard),
    nth1(ColIdx, TransposedBoard, Col),
    check_line(Col, ColIdx, Player, TempLine),
    swap_coordinates(TempLine, Line).

swap_coordinates([], []).
swap_coordinates([(Col, Row) | Rest], [(Row, Col) | SwappedRest]) :-
    swap_coordinates(Rest, SwappedRest).


check_line(Line, Index, Player, Result) :-
    append(_, [Player, Player, Player|_], Line),
    findall((Index, Col), 
            (nth1(Col, Line, Player)), 
            Result).

choose_two_to_remove(_, ToRemove, StackPos) :-
    write('Enter two positions to remove (e.g., [(R1, C1), (R2, C2)]): '),
    read(ToRemove),
    write('Enter position to stack (e.g., (Rs, Cs)): '),
    read(StackPos).

update_board(Board, ToRemove, StackPos, Player, NewBoard) :-
    remove_pieces(Board, ToRemove, TempBoard),
    add_stack(TempBoard, StackPos, Player, NewBoard).

remove_pieces(Board, [], Board).
remove_pieces(Board, [(Row, Col)|Rest], NewBoard) :-
    nth1(Row, Board, OldRow),
    replace_nth(Col, OldRow, empty, NewRow),
    replace_nth(Row, Board, NewRow, TempBoard),
    remove_pieces(TempBoard, Rest, NewBoard).

add_stack(Board, (Row, Col), white, NewBoard) :-
    nth1(Row, Board, OldRow),
    replace_nth(Col, OldRow, 8, NewRow), 
    replace_nth(Row, Board, NewRow, NewBoard).
add_stack(Board, (Row, Col), black, NewBoard) :-
    nth1(Row, Board, OldRow),
    replace_nth(Col, OldRow, x, NewRow),
    replace_nth(Row, Board, NewRow, NewBoard).

game_over(state(Board, _), Winner) :-
    (line_of_stacks(Board, Winner) -> true ;
    board_full(Board) -> Winner = draw ;
    Winner = none).

line_of_stacks(Board, Winner) :-
    stack_symbol(Winner, Stack),
    (check_lines_stacks(Board, Stack);
     check_columns_stacks(Board, Stack);
     check_diagonals_stacks(Board, Stack)).

check_lines_stacks(Board, Stack) :-
    member(Row, Board),
    append(_, [Stack, Stack, Stack|_], Row).

check_columns_stacks(Board, Stack) :-
    transpose(Board, TransposedBoard),
    member(Column, TransposedBoard),
    append(_, [Stack, Stack, Stack|_], Column).

check_diagonals_stacks(Board, Stack) :-
    diagonal(Board, Diagonals),
    member(Diagonal, Diagonals),
    append(_, [Stack, Stack, Stack|_], Diagonal).


stack_symbol(white, 8).
stack_symbol(black, x).

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

% Extract all diagonals from a board with coordinates
diagonal(Board, Diagonals) :-
    findall(Diag, diagonal_down(Board, Diag), DownDiags),
    findall(Diag, diagonal_up(Board, Diag), UpDiags),
    append(DownDiags, UpDiags, Diagonals).

% Extract downward diagonals with coordinates
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
diagonal_down_from(Board, Row, Col, [(Value,(Row,Col))|Rest]) :-
    length(Board, N),
    Row =< N,
    Col =< N,
    nth1(Row, Board, RowList),
    nth1(Col, RowList, Value),
    NextRow is Row + 1,
    NextCol is Col + 1,
    diagonal_down_from(Board, NextRow, NextCol, Rest).

% Extract upward diagonals with coordinates
diagonal_up(Board, Diagonal) :-
    length(Board, N),
    between(1, N, StartRow),
    diagonal_up_from(Board, StartRow, 1, Diagonal).
diagonal_up(Board, Diagonal) :-
    length(Board, N),
    between(2, N, StartCol),
    diagonal_up_from(Board, N, StartCol, Diagonal).

diagonal_up_from(Board, Row, Col, []) :-
    (Row < 1 ; Col > 7).
diagonal_up_from(Board, Row, Col, [(Value,(Row,Col))|Rest]) :-
    Row >= 1,
    Col =< 7,
    nth1(Row, Board, RowList),
    nth1(Col, RowList, Value),
    NextRow is Row - 1,
    NextCol is Col + 1,
    diagonal_up_from(Board, NextRow, NextCol, Rest).

% Check diagonals for three consecutive pieces
check_diagonals(Board, Player, Line) :-
    diagonal(Board, Diagonals),
    member(Diagonal, Diagonals),
    find_three_consecutive(Diagonal, Player, Line).

% Find three consecutive pieces with their coordinates
find_three_consecutive([(Player,(R1,C1)), (Player,(R2,C2)), (Player,(R3,C3))|_], Player, [(R1,C1), (R2,C2), (R3,C3)]).
find_three_consecutive([_|Rest], Player, Line) :-
    find_three_consecutive(Rest, Player, Line).

between(Low, High, Low) :-
    Low =< High.
between(Low, High, Value) :-
    Low < High,
    Next is Low + 1,
    between(Next, High, Value).