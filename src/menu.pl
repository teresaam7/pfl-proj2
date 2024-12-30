
main_menu :-
    nl,nl,
    write('\e[33m+------------------------------------------------------------------------+\e[0m'),nl,
    write('\e[33m|                                                                        |\e[0m'),nl,
    write('\e[33m|   \e[34mLLLLLLLLLLL                  OOOOOOOOO     TTTTTTTTTTTTTTTTTTTTTTT   \e[33m|\e[0m'),nl,
    write('\e[33m|   \e[34mL:::::::::L                OO:::::::::OO   T:::::::::::::::::::::T   \e[33m|\e[0m'),nl,
    write('\e[33m|   \e[34mL:::::::::L              OO:::::::::::::OO T:::::::::::::::::::::T   \e[33m|\e[0m'),nl,
    write('\e[33m|   \e[34mLL:::::::LL             O:::::::OOO:::::::OT:::::TT:::::::TT:::::T   \e[33m|\e[0m'),nl,
    write('\e[33m|     \e[34mL:::::L               O::::::O   O::::::OTTTTTT  T:::::T  TTTTTT   \e[33m|\e[0m'),nl,
    write('\e[33m|     \e[32mL:::::L               O:::::O     O:::::O        T:::::T           \e[33m|\e[0m'),nl,
    write('\e[33m|     \e[32mL:::::L               O:::::O     O:::::O        T:::::T           \e[33m|\e[0m'),nl,
    write('\e[33m|     \e[32mL:::::L               O:::::O     O:::::O        T:::::T           \e[33m|\e[0m'),nl,
    write('\e[33m|     \e[32mL:::::L               O:::::O     O:::::O        T:::::T           \e[33m|\e[0m'),nl,
    write('\e[33m|     \e[36mL:::::L               O:::::O     O:::::O        T:::::T           \e[33m|\e[0m'),nl,
    write('\e[33m|     \e[36mL:::::L               O:::::O     O:::::O        T:::::T           \e[33m|\e[0m'),nl,
    write('\e[33m|     \e[36mL:::::L         LLLLLLO::::::O   O::::::O        T:::::T           \e[33m|\e[0m'),nl,
    write('\e[33m|   \e[36mLL:::::::LLLLLLLLL:::::LO:::::::OOO:::::::O      TT:::::::TT         \e[33m|\e[0m'),nl,
    write('\e[33m|   \e[35mL::::::::::::::::::::::L OO:::::::::::::OO       T:::::::::T         \e[33m|\e[0m'),nl,
    write('\e[33m|   \e[35mL::::::::::::::::::::::L   OO:::::::::OO         T:::::::::T         \e[33m|\e[0m'),nl,
    write('\e[33m|   \e[35mLLLLLLLLLLLLLLLLLLLLLLLL     OOOOOOOOO           TTTTTTTTTTT         \e[33m|\e[0m'),nl,
    write('\e[33m|                                                                        |\e[0m'),nl,
    write('\e[33m|                      1) Human vs Human                                 |\e[0m'),nl,
    write('\e[33m|                      2) Human vs Computer                              |\e[0m'),nl,
    write('\e[33m|                      3) Computer vs Computer                           |\e[0m'),nl,
    write('\e[33m|                      4) Exit                                           |\e[0m'),nl,
    write('\e[33m|                                                                        |\e[0m'),nl,
    write('\e[33m|                      --INSERT OPTION--                                 |\e[0m'),nl,
    write('\e[33m|                                                                        |\e[0m'),nl,
    write('\e[33m+------------------------------------------------------------------------+\e[0m'),nl,nl,
    write('\e[37mEnter your choice: \e[0m'),
    read(Choice),
    handle_menu_choice(Choice).



handle_menu_choice(1) :- start_game(human, human).
handle_menu_choice(2) :- select_difficulty(Difficulty), start_game(human, computer(Difficulty)).
handle_menu_choice(3) :- select_difficulty(D1), select_difficulty(D2), start_game(computer(D1), computer(D2)).
handle_menu_choice(4) :- nl, write('Exiting game. Goodbye!'), nl.
handle_menu_choice(_) :- nl, write('Invalid choice, try again.'), nl, main_menu.


% Difficulty selection
select_difficulty(Difficulty) :-
    nl, write('Select difficulty level:'), nl,
    write('1. Easy (Random moves)'), nl,
    write('2. Hard (Greedy moves)'), nl,
    write('Enter your choice: '),
    read(Choice),
    validate_difficulty(Choice, Difficulty).

validate_difficulty(1, 1) :-
    nl, write('You selected Easy (Random moves).'), nl, !.

validate_difficulty(2, 2) :-
    nl, write('You selected Hard (Greedy moves).'), nl, !.

validate_difficulty(_, Difficulty) :-
    nl, write('Invalid choice. Please select 1 (Easy) or 2 (Hard).'), nl,
    select_difficulty(Difficulty).

