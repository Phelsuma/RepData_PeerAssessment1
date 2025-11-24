# Chess Game Simulation in R

This repository now includes a complete chess game simulation implemented in R.

## Overview

The `chess_game.R` file contains a fully functional chess game simulation with:

- Complete chess board representation (8x8 grid)
- Full implementation of all chess pieces and their movement rules:
  - Pawns (forward movement, diagonal capture, two-square initial move)
  - Rooks (horizontal and vertical movement)
  - Knights (L-shaped movement)
  - Bishops (diagonal movement)
  - Queens (combination of rook and bishop movement)
  - Kings (one square in any direction)
- Move validation ensuring legal moves only
- Simple AI that evaluates board positions and chooses moves
- Turn-based gameplay alternating between white and black
- Game state tracking and win condition detection
- Visual board display in the console

## How to Run

### Basic Usage

To run the chess simulation with default settings (AI vs AI, 30 moves):

```r
source("chess_game.R")
```

Or from the command line:

```bash
Rscript chess_game.R
```

### Custom Game

You can also customize the game by calling the `play_chess()` function directly:

```r
# Load the chess functions
source("chess_game.R")

# Run a longer game
final_board <- play_chess(mode = "ai_vs_ai", max_moves = 50)
```

## Features

### Board Representation

The chess board uses a matrix representation where:
- Lowercase letters represent white pieces: `p` (pawn), `r` (rook), `n` (knight), `b` (bishop), `q` (queen), `k` (king)
- Uppercase letters represent black pieces: `P` (pawn), `R` (rook), `N` (knight), `B` (bishop), `Q` (queen), `K` (king)
- Empty squares are represented by spaces

### Move Notation

Moves use algebraic notation:
- Columns are labeled a-h (left to right)
- Rows are numbered 1-8 (bottom to top from white's perspective)
- Example: `e2 -> e4` means moving a piece from square e2 to e4

### AI Strategy

The AI uses a simple evaluation function that:
1. Assigns values to pieces (pawn=1, knight=3, bishop=3, rook=5, queen=9, king=100)
2. Calculates the total board value (white pieces positive, black pieces negative)
3. Chooses moves that maximize the evaluation for the current player
4. Includes random variation (30% chance) to make gameplay less predictable

## Functions

The script provides several reusable functions:

- `initialize_board()` - Creates the starting chess position
- `display_board(board)` - Prints the board to console
- `is_valid_move(board, from_pos, to_pos, is_white_turn)` - Validates if a move is legal
- `make_move(board, from_pos, to_pos)` - Executes a move
- `get_all_valid_moves(board, is_white_turn)` - Returns all legal moves for current player
- `evaluate_board(board)` - Calculates board position value
- `ai_choose_move(board, is_white_turn)` - AI move selection
- `play_chess(mode, max_moves)` - Main game loop

## Example Output

```
=== Chess Game Simulation ===
Mode: ai_vs_ai 
Pieces: lowercase = white, UPPERCASE = black

  a b c d e f g h
8 |R |N |B |Q |K |B |N |R |8 
7 |P |P |P |P |P |P |P |P |7 
6 |   |   |   |   |   |   |   |   |6 
5 |   |   |   |   |   |   |   |   |5 
4 |   |   |   |   |   |   |   |   |4 
3 |   |   |   |   |   |   |   |   |3 
2 |p |p |p |p |p |p |p |p |2 
1 |r |n |b |q |k |b |n |r |1 
  a b c d e f g h

Move 1 : White to move
White moves: e2 -> e4
...
```

## Requirements

- R (version 4.0 or higher recommended)
- No external packages required - uses only base R

## Future Enhancements

Potential improvements that could be made:
- Castling support
- En passant capture
- Pawn promotion
- Check and checkmate detection
- Stalemate detection
- More sophisticated AI (minimax algorithm, alpha-beta pruning)
- Interactive mode for human players
- Move history tracking
- Save/load game state

## License

This chess implementation is part of the RepData_PeerAssessment1 repository and follows the same license.
