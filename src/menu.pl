
main_menu :-
    nl,nl,
    write('+------------------------------------------------------------------------+'),nl,
    write('|                                                                        |'),nl,
    write('|   LLLLLLLLLLL                  OOOOOOOOO     TTTTTTTTTTTTTTTTTTTTTTT   |'),nl,
    write('|   L:::::::::L                OO:::::::::OO   T:::::::::::::::::::::T   |'),nl,
    write('|   L:::::::::L              OO:::::::::::::OO T:::::::::::::::::::::T   |'),nl,
    write('|   LL:::::::LL             O:::::::OOO:::::::OT:::::TT:::::::TT:::::T   |'),nl,
    write('|     L:::::L               O::::::O   O::::::OTTTTTT  T:::::T  TTTTTT   |'),nl,
    write('|     L:::::L               O:::::O     O:::::O        T:::::T           |'),nl,
    write('|     L:::::L               O:::::O     O:::::O        T:::::T           |'),nl,
    write('|     L:::::L               O:::::O     O:::::O        T:::::T           |'),nl,
    write('|     L:::::L               O:::::O     O:::::O        T:::::T           |'),nl,
    write('|     L:::::L               O:::::O     O:::::O        T:::::T           |'),nl,
    write('|     L:::::L               O:::::O     O:::::O        T:::::T           |'),nl,
    write('|     L:::::L         LLLLLLO::::::O   O::::::O        T:::::T           |'),nl,
    write('|   LL:::::::LLLLLLLLL:::::LO:::::::OOO:::::::O      TT:::::::TT         |'),nl,
    write('|   L::::::::::::::::::::::L OO:::::::::::::OO       T:::::::::T         |'),nl,
    write('|   L::::::::::::::::::::::L   OO:::::::::OO         T:::::::::T         |'),nl,
    write('|   LLLLLLLLLLLLLLLLLLLLLLLL     OOOOOOOOO           TTTTTTTTTTT         |'),nl,
    write('|                                                                        |'),nl,
    write('|                      1) Human vs Human                                 |'),nl,
    write('|                      2) Human vs Computer                              |'),nl,
    write('|                      3) Computer vs Computer                           |'),nl,
    write('|                      4) Exit                                           |'),nl,
    write('|                                                                        |'),nl,
    write('|                      --INSERT OPTION--                                 |'),nl,
    write('|                                                                        |'),nl,
    write('+------------------------------------------------------------------------+'),nl,nl,
    write('Enter your choice: '),
    read(Choice),
    handle_menu_choice(Choice).


handle_menu_choice(1) :- start_game(human, human).
handle_menu_choice(2) :- select_difficulty(Difficulty), start_game(human, computer(Difficulty)).
handle_menu_choice(3) :- select_difficulty(D1), select_difficulty(D2), start_game(computer(D1), computer(D2)).
handle_menu_choice(4) :- nl, write('Exiting game. Goodbye!'), nl.
handle_menu_choice(_) :- nl, write('Invalid choice, try again.'), nl, main_menu.


% Difficulty selection
select_difficulty(1) :-
    nl, write('Select difficulty level:'), nl,
    write('1. Easy (Random moves)'), nl,
    write('2. Hard (Greedy moves)'), nl,
    write('Enter your choice: '),
    read(1),
    nl, write('You selected Easy (Random moves).'), nl.

select_difficulty(2) :-
    nl, write('Select difficulty level:'), nl,
    write('1. Easy (Random moves)'), nl,
    write('2. Hard (Greedy moves)'), nl,
    write('Enter your choice: '),
    read(2),
    nl, write('You selected Hard (Greedy moves).'), nl.

select_difficulty(Difficulty) :-
    nl, write('Invalid choice. Please select 1 (Easy) or 2 (Hard).'), nl,
    select_difficulty(Difficulty).


