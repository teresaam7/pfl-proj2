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
   - Place a piece on an empty cell.
   - Move a stack (of one or two pieces) to an adjacent cell, adhering to movement restrictions.
3. **Stack Formation**: A stack may contain **no more than two pieces** at any given time.

#### **Stack Formation**
When a **line of three or more pieces (not stacks)** of a player’s color is created in any direction, the following sequence must be executed during the same turn:

1. **Choose a Line**: If multiple lines are created simultaneously, the player must choose one line of three or more pieces.
2. **Remove Two Pieces**: From the chosen line, remove any two pieces, leaving one piece in place.
3. **Create a Stack**: Add a new piece of the player’s color to the single piece left in place,  creating a **stack** of two pieces.

#### **Winning Condition**
- A player wins by forming a **line of three stacks** with two pieces each. Lines can be formed:
  - Horizontally
  - Vertically
  - Diagonally


## Considerations for Game Extensions
- **Variable-sized boards**: The game can be adapted to different board sizes by adjusting initial configurations.
- **Optional rules**: Simplified rules for novice players include restricting moves to certain zones. Advanced rules for experts introduce additional win conditions.
- **AI difficulty**: AI algorithms can be extended with enhanced heuristics and depth analysis for expert-level gameplay.

## Game Logic
### Game Configuration Representation
- Initial configuration is represented using a predicate `initial_state/2`. For example:
  ```prolog
  initial_state([empty, empty, ...], player1).
  ```
- The board is represented as a list of cells, with each cell being either `empty`, `piece(Player)`, or `stack(Player, Height)`.

### Internal Game State Representation
- The game state is maintained using:
  - **Board**: A list of 49 cells for the 7x7 grid.
  
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
  - **Turn**: Indicates the current player (`player1` or `player2`).

Example:
```prolog
state([empty, stack(player1, 2), ...], player2).
```

### Move Representation
- Moves are represented as:
  ```prolog
  move(From, To).
  ```
  where `From` and `To` are coordinates (e.g., `(3, 4)` for row 3, column 4).

- Moves are validated using the `valid_move/3` predicate.

### User Interaction
- A menu system allows users to select:
  1. Game mode (e.g., Human vs AI).
  2. Difficulty level.

- Input validation ensures only valid moves are accepted:
  ```prolog
  read_move(From, To), validate_move(From, To).
  ```

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
