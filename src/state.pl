
initial_state(state(Board, white)) :-
    empty_board(Board).

empty_board(Board) :-
    length(Board, 7),
    maplist(empty_row, Board).

empty_row(Row) :-
    length(Row, 7),
    maplist(=(empty), Row).

game_loop(GameState, Player1, Player2, TurnCount, PieRule) :-
    display_game(GameState),
    game_over(GameState, Winner),
    (Winner \= none ->
        nl, format("Game over! Winner: ~w~n", [Winner])
    ;
        GameState = state(_, CurrentPlayer),
                      % Check if TurnCount is 2 and perform an action
        (TurnCount =:= 2 ->
            nl, write('Would you like to change colors with your opponent? y/n '),
                read(Answer),
                NewPieRule = Answer,
                (Answer = 'y') -> game_loop(GameState, Player1, Player2, TurnCount +1, NewPieRule)
        ;
            NewPieRule = PieRule % Continue if TurnCount is not 2
        ),
        % Determine CurrentPlayerType based on PieRule
        ((PieRule = 'y', CurrentPlayer = black ; PieRule \= 'y', CurrentPlayer = white) ->
            CurrentPlayerType = Player1
        ;
            CurrentPlayerType = Player2
        ),
        choose_move(GameState, CurrentPlayerType, Move),
        move(GameState, Move, TempGameState),
        handle_line_of_three(TempGameState, CurrentPlayerType, NewGameState),
         NewTurnCount is TurnCount + 1, 
        game_loop(NewGameState, Player1, Player2, NewTurnCount, NewPieRule)
    ).


start_game(Player1, Player2) :-
    initial_state(GameState),
    game_loop(GameState, Player1, Player2, 0, 'n').

choose_move(GameState, computer(1), Move) :-
    valid_moves(GameState, Moves),
    random_member(Move, Moves).

choose_move(GameState, computer(2), Move) :-
    valid_moves(GameState, Moves),
    best_move(GameState, Moves, Move).

choose_move(GameState, human, Move) :-
    repeat,  % This creates a loop until `valid_input` succeeds.
    nl, write('Enter your move (Row, Col): '),
    read((Row, Col)),
    (valid_move(GameState, move(Row, Col)) ->
        Move = move(Row, Col),
        !
    ;
        write('Invalid move! Please try again.'), nl, fail).

valid_move(state(Board, _), move(Row, Col)) :-
    valid_position(Board, Row, Col).

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
    next_player(Player, NextPlayer),
    find_lines_of_three(Board, NextPlayer, Lines),
    Lines \= [],
    (
        PlayerType = human -> 
            write('Lines of three found: '), write(Lines), nl,
            choose_two_to_remove(Lines, StackPos),
            select_removal(StackPos, Lines, ToRemove),
            write(ToRemove)
        ;
        PlayerType = computer(1) -> 
            choose_best_removal(Lines, ToRemove, StackPos)
    ),
    update_board(Board, ToRemove, StackPos, NextPlayer, NewBoard).
handle_line_of_three(GameState, _, GameState).

select_removal(StackPos, [Line|_], ToRemove) :-
    exclude(==(StackPos), Line, ToRemove).


choose_best_removal([Line|_], ToRemove, StackPos) :-
    Line = [(R1, C1), (R2, C2), (R3, C3)],
    ToRemove = [(R1, C1), (R2, C2)],
    StackPos = (R3, C3),
    write('To Remove: '), write(ToRemove), nl,
    write('Stack Position: '), write(StackPos), nl.

find_lines_of_three(Board, Player, Lines) :-
    setof(Line, (check_lines(Board, Player, Line); check_columns(Board, Player, Line); check_diagonals(Board, Player, Line)), Lines).

check_lines(Board, Player, Line) :-
    nth1(RowIdx, Board, Row),
    check_line(Row, RowIdx, Player, Line).

check_columns(Board, Player, Line) :-
    transpose(Board, TransposedBoard),
    nth1(ColIdx, TransposedBoard, Col),
    check_line(Col, ColIdx, Player, TempLine),
    maplist(swap_coords, TempLine, Line).

swap_coords((X, Y), (Y, X)).

check_diagonals(Board, Player, Line) :-
    diagonal(Board, Diagonals),
    member(Diagonal, Diagonals),
    check_diagonal_line(Diagonal, Player, Line).

check_diagonal_line(Line, Player, Result) :-
    append(_, [Player, Player, Player|_], Line),
    findall((Row, Col), 
            (nth1(Index, Line, Player), 
             Row is 8 - Index, 
             Col is Index), 
            Result).

check_line(Line, RowIdx, Player, Result) :-
    append(_, [Player, Player, Player|_], Line),
    findall((Row, Col),
        (
            nth1(Index, Line, Player),
            Row = RowIdx,  % Row is fixed for rows
            Col = Index    % Index determines the column
        ),
        Result).

choose_two_to_remove(_, StackPos) :-
    write('Enter position tostack (e.g., (Rs, Cs)): '),
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

% Extract all diagonals from a board
diagonal(Board, Diagonals) :-
    findall(Diag, (diagonal_down(Board, Diag); diagonal_up(Board, Diag)), Diagonals).

diagonal_down(Board, Diagonal) :-
    length(Board, N),
    between(1, N, StartRow),
    diagonal_down_from(Board, StartRow, 1, Diagonal).
diagonal_down(Board, Diagonal) :-
    length(Board, N),
    between(1, N, StartCol),
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

diagonal_up(Board, Diagonal) :-
    length(Board, N),
    between(1, N, StartRow),
    diagonal_up_from(Board, StartRow, 1, Diagonal).
diagonal_up(Board, Diagonal) :-
    length(Board, N),
    between(2, N, StartCol),
    diagonal_up_from(Board, N, StartCol, Diagonal).

diagonal_up_from(Board, Row, Col, []) :-
    (Row < 1 ; Col > 7 ; Row > 7).
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