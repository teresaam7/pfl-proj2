# LOT Game

## Identification
**Game**: Line Of Three (LOT)  
**Group**: LOT_7
**Members**:

| Student Number | Name        | Contribution (%) | Tasks Performed                        |
|----------------|------------------|------------------|----------------------------------------|
| up202208247      | Diana Nunes          | 50%   | [Task1, Task2, ...]                   |
| up202206828      | Teresa Mascarenhas          | 50%   | [Task1, Task2, ...]                   |

## Installation and Execution
### Prerequisites
- **SICStus Prolog 4.9** installed on your system.
- Operating system: Windows or Linux.

### Installation
1. Clone the repository:
   ```bash
   git clone [repository_url]
   cd [repository_folder]
   ```

2. Ensure SICStus Prolog is installed and added to your PATH.

### Execution on Linux
1. Open a terminal and navigate to the game folder:
2. Start SICStus Prolog:
   ```bash
   sicstus
   ```

3. Load the game:
   ```prolog
   consult('game.pl').  
   ['game.pl'].  % alternative solution
   ```

4. Start the game:
   ```prolog
   play.
   ```

### Execution on Windows
1. Open the SICStus Prolog application.
2. Navigate to the game directory in the Prolog console:
3. Load the game:
   ```prolog
   consult('game.pl').  
   ['game.pl'].  % alternative solution
   ```
4. Start the game:
   ```prolog
   play.
   ```

## Description of the Game
**Line Of Three (LOT)** is a strategic game played on a 7x7 orthogonal grid. The goal is to create a line of three stacks, each consisting of two pieces. The game is designed for Human vs Human, Human vs AI, and AI vs AI interactions.

### **Rules**

#### **General Rules**
1. **Turn-Based Play**: Players alternate turns, taking one action per turn.
2. **Actions Per Turn**: A player may:
   - Place a piece of their respective colour on an empty cell.
   - check for a Stack Formation.

#### **Stack Formation**
When a **line of three or more pieces (not stacks)** of a player’s color is created in any direction, the following sequence must be executed during the same turn:

1. **Choose a Line**: If multiple lines are created simultaneously, the player must choose one line of three or more pieces.
2. **Remove Two Pieces**: From the chosen line, remove any two pieces, leaving one piece in place.
3. **Create a Stack**: Add a new piece of the player’s color to the single piece left in place,  creating a **stack** of two pieces.
A stack may contain **no more than two pieces** at any given time.

#### **Winning Condition**
- A player wins by forming a **line of three stacks** with two pieces each. Lines can be formed:
  - Horizontally
  - Vertically
  - Diagonally

Sources regarding rules and game description: 

https://boardgamegeek.com/boardgame/127989/lot

https://boardgamegeek.com/video/164442/lot/lot-line-of-three-game-overview-and-rules-explanat

https://boardgamegeek.com/filepage/217760/lot-rules-in-english



## Considerations for Game Extensions
- **Variable-sized boards**: The game can be adapted to different board sizes by adjusting initial configurations.
- **Optional rules**: Simplified rules for novice players include restricting moves to certain zones. Advanced rules for experts introduce additional win conditions.
- **AI difficulty**: AI algorithms can be extended with enhanced heuristics and depth analysis for expert-level gameplay.

## Game Logic
### Game Configuration Representation
- Initial configuration is represented using a predicate `initial_state/2`. For example:
  `initial_state(state(Board, Player))`
- The board is represented as a list of cells, with each cell being either `empty`, `piece(Player)`, or `stack(Player, Height)`.

### Internal Game State Representation
- The game state is maintained using:
  - **Board**: A list of 49 cells for the 7x7 grid.
 
    #### Initial state
    
    ```prolog
    [[empty, empty, empty, empty, empty, empty, empty],
     [empty, empty, empty, empty, empty, empty, empty],
     [empty, empty, empty, empty, empty, empty, empty],
     [empty, empty, empty, empty, empty, empty, empty],
     [empty, empty, empty, empty, empty, empty, empty],
     [empty, empty, empty, empty, empty, empty, empty],
     [empty, empty, empty, empty, empty, empty, empty]]
     ```
  
     ```
         |  1 |  2 |  3 |  4 |  5 |  6 |  7 |
     ----|----|----|----|----|----|----|----|
       7 |  . |  . |  . |  . |  . |  . |  . | 
     ____|____|____|____|____|____|____|____|
       6 |  . |  . |  . |  . |  . |  . |  . | 
     ____|____|____|____|____|____|____|____|
       5 |  . |  . |  . |  . |  . |  . |  . | 
     ____|____|____|____|____|____|____|____|
       4 |  . |  . |  . |  . |  . |  . |  . | 
     ____|____|____|____|____|____|____|____|
       3 |  . |  . |  . |  . |  . |  . |  . | 
     ____|____|____|____|____|____|____|____|
       2 |  . |  . |  . |  . |  . |  . |  . | 
     ____|____|____|____|____|____|____|____|
       1 |  . |  . |  . |  . |  . |  . |  . | 
     ____|____|____|____|____|____|____|____|
     ```
      #### Intermediate state

      #### Final state
     
  - **Turn**: Indicates the current player (`player1` or `player2`).

Example:
```prolog
state([empty, stack(player1, 2), ...], player2).
```

*Final state with a possible line of three.*

### Game state visualization

The game visualization includes helper predicates that manage the game's user interface, focusing on the board display and player interaction.

- The **game state** can be displayed using the **displayGame/1** predicate, which outputs the current board to the user in a readable format.

#### Initializing the Game

The game starts with the predicate **initialState/1**, which sets up the board and assigns the first player randomly.

#### Displaying the Game

The board is displayed with **displayGame/1**, which shows the board layout using **boardLine/3** to print each row and **displayColumns/1** to show the column headers.

### Move Execution

The **move/3** predicate handles both placing a piece and shifting a piece:

    move(+GameState, +Move, -NewGameState)

This will validate and apply a move if it's legal.

#### Move Types

There are **two types of moves**:
1. **Place a piece**: Adds a new piece to an empty cell.
2. **Shift a piece**: Moves a piece to an adjacent empty cell.

The **move/3** predicate is split into different validation rules for each type, ensuring that each move is valid.

### Game Over

The game checks for a win condition using the **gameOver/2** predicate:

    gameOver(+GameState, -Winner)

The game will end when a player has a line of three pieces stacked in a row (either horizontally, vertically, or diagonally). The predicate checks all possible win conditions on the board.

### List of Valid Moves

To determine the valid moves for a player, the **validMoves/2** predicate is used:

    validMoves(+GameState, -Moves)

This predicate will return a list of all valid moves based on the current board state.

### Game State Evaluation

The **evaluateBoard/2** predicate evaluates the board position based on how close each player is to forming a line of three:

    evaluateBoard(+GameState, -Value)

The evaluation considers:
- How many pieces the player is missing to complete a line.
- How many pieces the opponent is missing to complete their line.

The evaluation helps guide the AI to make optimal decisions.

### Computer Move

The **chooseMove/3** predicate allows the computer to decide on its next move:

    chooseMove(+GameState, +Level, -Move)

The bot selects a move based on the difficulty level:
- **Easy**: Selects a random move.
- **Hard**: Chooses the best move using a greedy algorithm that evaluates the board.


## Conclusions
### Summary
This project successfully implements the LOT game, including:
- Human and AI interactions.
- Configurable board and rules.

### Limitations
- The AI can be slow for complex heuristics.
- The current implementation does not support undo functionality.

### Future Improvements
1. Optimize AI performance.
2. Add a graphical user interface (GUI).
3. Support for online multiplayer mode.

## Bibliography
- [SICStus Prolog Documentation](https://sicstus.sics.se/documentation.html)
- [LOT Rules Source](https://boardgamegeek.com/boardgame/127989/lot)
- ChatGPT queries for Prolog logic and optimizations.
