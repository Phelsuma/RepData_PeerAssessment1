# Chess Game Simulation in R
# A complete implementation of a chess game with basic AI

# Initialize the chess board
initialize_board <- function() {
  board <- matrix("  ", nrow = 8, ncol = 8)
  
  # White pieces (lowercase)
  board[1, ] <- c("r", "n", "b", "q", "k", "b", "n", "r")
  board[2, ] <- rep("p", 8)
  
  # Black pieces (uppercase)
  board[7, ] <- rep("P", 8)
  board[8, ] <- c("R", "N", "B", "Q", "K", "B", "N", "R")
  
  return(board)
}

# Display the chess board
display_board <- function(board) {
  cat("\n  a b c d e f g h\n")
  for (i in 8:1) {
    cat(i, "|")
    for (j in 1:8) {
      cat(board[i, j], "|")
    }
    cat(i, "\n")
  }
  cat("  a b c d e f g h\n\n")
}

# Convert algebraic notation to matrix indices
algebraic_to_indices <- function(pos) {
  col <- match(substr(pos, 1, 1), letters[1:8])
  row <- as.numeric(substr(pos, 2, 2))
  return(c(row, col))
}

# Convert matrix indices to algebraic notation
indices_to_algebraic <- function(row, col) {
  return(paste0(letters[col], row))
}

# Check if a piece is white
is_white <- function(piece) {
  return(piece %in% c("p", "r", "n", "b", "q", "k"))
}

# Check if a piece is black
is_black <- function(piece) {
  return(piece %in% c("P", "R", "N", "B", "Q", "K"))
}

# Get piece color
get_color <- function(piece) {
  if (is_white(piece)) return("white")
  if (is_black(piece)) return("black")
  return(NA)
}

# Check if a square is empty
is_empty <- function(board, row, col) {
  if (row < 1 || row > 8 || col < 1 || col > 8) return(FALSE)
  return(board[row, col] == "  ")
}

# Check if a square contains an opponent's piece
is_opponent <- function(board, row, col, is_white_turn) {
  if (row < 1 || row > 8 || col < 1 || col > 8) return(FALSE)
  piece <- board[row, col]
  if (piece == "  ") return(FALSE)
  if (is_white_turn) return(is_black(piece))
  return(is_white(piece))
}

# Validate pawn move
validate_pawn_move <- function(board, from_row, from_col, to_row, to_col, is_white_turn) {
  direction <- if (is_white_turn) 1 else -1
  start_row <- if (is_white_turn) 2 else 7
  
  # Move forward one square
  if (to_col == from_col && to_row == from_row + direction) {
    return(is_empty(board, to_row, to_col))
  }
  
  # Move forward two squares from start position
  if (to_col == from_col && to_row == from_row + 2 * direction && from_row == start_row) {
    return(is_empty(board, from_row + direction, from_col) && is_empty(board, to_row, to_col))
  }
  
  # Capture diagonally
  if (abs(to_col - from_col) == 1 && to_row == from_row + direction) {
    return(is_opponent(board, to_row, to_col, is_white_turn))
  }
  
  return(FALSE)
}

# Validate rook move
validate_rook_move <- function(board, from_row, from_col, to_row, to_col, is_white_turn) {
  if (from_row != to_row && from_col != to_col) return(FALSE)
  
  # Check path is clear
  if (from_row == to_row) {
    if (abs(to_col - from_col) <= 1) {
      return(is_empty(board, to_row, to_col) || is_opponent(board, to_row, to_col, is_white_turn))
    }
    step <- if (to_col > from_col) 1 else -1
    for (col in seq(from_col + step, to_col - step, step)) {
      if (!is_empty(board, from_row, col)) return(FALSE)
    }
  } else {
    if (abs(to_row - from_row) <= 1) {
      return(is_empty(board, to_row, to_col) || is_opponent(board, to_row, to_col, is_white_turn))
    }
    step <- if (to_row > from_row) 1 else -1
    for (row in seq(from_row + step, to_row - step, step)) {
      if (!is_empty(board, row, from_col)) return(FALSE)
    }
  }
  
  return(is_empty(board, to_row, to_col) || is_opponent(board, to_row, to_col, is_white_turn))
}

# Validate knight move
validate_knight_move <- function(board, from_row, from_col, to_row, to_col, is_white_turn) {
  row_diff <- abs(to_row - from_row)
  col_diff <- abs(to_col - from_col)
  
  if (!((row_diff == 2 && col_diff == 1) || (row_diff == 1 && col_diff == 2))) {
    return(FALSE)
  }
  
  return(is_empty(board, to_row, to_col) || is_opponent(board, to_row, to_col, is_white_turn))
}

# Validate bishop move
validate_bishop_move <- function(board, from_row, from_col, to_row, to_col, is_white_turn) {
  if (abs(to_row - from_row) != abs(to_col - from_col)) return(FALSE)
  
  # Check path is clear
  row_step <- if (to_row > from_row) 1 else -1
  col_step <- if (to_col > from_col) 1 else -1
  
  row <- from_row + row_step
  col <- from_col + col_step
  
  while (row != to_row && col != to_col) {
    if (!is_empty(board, row, col)) return(FALSE)
    row <- row + row_step
    col <- col + col_step
  }
  
  return(is_empty(board, to_row, to_col) || is_opponent(board, to_row, to_col, is_white_turn))
}

# Validate queen move (combination of rook and bishop)
validate_queen_move <- function(board, from_row, from_col, to_row, to_col, is_white_turn) {
  return(validate_rook_move(board, from_row, from_col, to_row, to_col, is_white_turn) ||
         validate_bishop_move(board, from_row, from_col, to_row, to_col, is_white_turn))
}

# Validate king move
validate_king_move <- function(board, from_row, from_col, to_row, to_col, is_white_turn) {
  if (abs(to_row - from_row) > 1 || abs(to_col - from_col) > 1) return(FALSE)
  
  return(is_empty(board, to_row, to_col) || is_opponent(board, to_row, to_col, is_white_turn))
}

# Validate move based on piece type
is_valid_move <- function(board, from_pos, to_pos, is_white_turn) {
  from <- algebraic_to_indices(from_pos)
  to <- algebraic_to_indices(to_pos)
  
  from_row <- from[1]
  from_col <- from[2]
  to_row <- to[1]
  to_col <- to[2]
  
  # Check boundaries
  if (from_row < 1 || from_row > 8 || from_col < 1 || from_col > 8 ||
      to_row < 1 || to_row > 8 || to_col < 1 || to_col > 8) {
    return(FALSE)
  }
  
  # Check if moving to same square
  if (from_row == to_row && from_col == to_col) return(FALSE)
  
  piece <- board[from_row, from_col]
  
  # Check if there's a piece to move
  if (piece == "  ") return(FALSE)
  
  # Check if it's the right player's turn
  if (is_white_turn && !is_white(piece)) return(FALSE)
  if (!is_white_turn && !is_black(piece)) return(FALSE)
  
  # Check if trying to capture own piece
  target <- board[to_row, to_col]
  if (target != "  " && get_color(piece) == get_color(target)) return(FALSE)
  
  # Validate move based on piece type
  piece_type <- tolower(piece)
  
  if (piece_type == "p") {
    return(validate_pawn_move(board, from_row, from_col, to_row, to_col, is_white_turn))
  } else if (piece_type == "r") {
    return(validate_rook_move(board, from_row, from_col, to_row, to_col, is_white_turn))
  } else if (piece_type == "n") {
    return(validate_knight_move(board, from_row, from_col, to_row, to_col, is_white_turn))
  } else if (piece_type == "b") {
    return(validate_bishop_move(board, from_row, from_col, to_row, to_col, is_white_turn))
  } else if (piece_type == "q") {
    return(validate_queen_move(board, from_row, from_col, to_row, to_col, is_white_turn))
  } else if (piece_type == "k") {
    return(validate_king_move(board, from_row, from_col, to_row, to_col, is_white_turn))
  }
  
  return(FALSE)
}

# Make a move
make_move <- function(board, from_pos, to_pos) {
  from <- algebraic_to_indices(from_pos)
  to <- algebraic_to_indices(to_pos)
  
  board[to[1], to[2]] <- board[from[1], from[2]]
  board[from[1], from[2]] <- "  "
  
  return(board)
}

# Get all valid moves for a piece
get_valid_moves <- function(board, row, col, is_white_turn) {
  valid_moves <- list()
  from_pos <- indices_to_algebraic(row, col)
  
  for (to_row in 1:8) {
    for (to_col in 1:8) {
      to_pos <- indices_to_algebraic(to_row, to_col)
      if (is_valid_move(board, from_pos, to_pos, is_white_turn)) {
        valid_moves <- append(valid_moves, list(c(from_pos, to_pos)))
      }
    }
  }
  
  return(valid_moves)
}

# Get all valid moves for current player
get_all_valid_moves <- function(board, is_white_turn) {
  all_moves <- list()
  
  for (row in 1:8) {
    for (col in 1:8) {
      piece <- board[row, col]
      if (piece != "  ") {
        if ((is_white_turn && is_white(piece)) || (!is_white_turn && is_black(piece))) {
          moves <- get_valid_moves(board, row, col, is_white_turn)
          all_moves <- append(all_moves, moves)
        }
      }
    }
  }
  
  return(all_moves)
}

# Simple evaluation function for the board
evaluate_board <- function(board) {
  piece_values <- list(
    p = 1, P = 1,
    n = 3, N = 3,
    b = 3, B = 3,
    r = 5, R = 5,
    q = 9, Q = 9,
    k = 100, K = 100
  )
  
  score <- 0
  for (row in 1:8) {
    for (col in 1:8) {
      piece <- board[row, col]
      if (piece != "  ") {
        value <- piece_values[[piece]]
        if (is_white(piece)) {
          score <- score + value
        } else {
          score <- score - value
        }
      }
    }
  }
  
  return(score)
}

# Simple AI to choose a move
ai_choose_move <- function(board, is_white_turn) {
  valid_moves <- get_all_valid_moves(board, is_white_turn)
  
  if (length(valid_moves) == 0) {
    return(NULL)
  }
  
  # Simple strategy: choose move that results in best board evaluation
  best_score <- if (is_white_turn) -Inf else Inf
  best_move <- NULL
  
  for (move in valid_moves) {
    # Simulate the move
    temp_board <- board
    temp_board <- make_move(temp_board, move[1], move[2])
    score <- evaluate_board(temp_board)
    
    # Choose move based on who's playing
    if (is_white_turn && score > best_score) {
      best_score <- score
      best_move <- move
    } else if (!is_white_turn && score < best_score) {
      best_score <- score
      best_move <- move
    }
  }
  
  # Add some randomness to make it less predictable
  if (runif(1) < 0.3 && length(valid_moves) > 0) {
    best_move <- valid_moves[[sample(1:length(valid_moves), 1)]]
  }
  
  return(best_move)
}

# Check if king is in check (simplified version)
is_in_check <- function(board, is_white_king) {
  # Find king position
  king <- if (is_white_king) "k" else "K"
  king_pos <- which(board == king, arr.ind = TRUE)
  
  if (nrow(king_pos) == 0) return(FALSE)
  
  king_row <- king_pos[1, 1]
  king_col <- king_pos[1, 2]
  
  # Check if any opponent piece can attack the king
  for (row in 1:8) {
    for (col in 1:8) {
      piece <- board[row, col]
      if (piece != "  " && 
          ((is_white_king && is_black(piece)) || (!is_white_king && is_white(piece)))) {
        from_pos <- indices_to_algebraic(row, col)
        to_pos <- indices_to_algebraic(king_row, king_col)
        if (is_valid_move(board, from_pos, to_pos, !is_white_king)) {
          return(TRUE)
        }
      }
    }
  }
  
  return(FALSE)
}

# Main game function
play_chess <- function(mode = "ai_vs_ai", max_moves = 50) {
  board <- initialize_board()
  is_white_turn <- TRUE
  move_count <- 0
  
  cat("=== Chess Game Simulation ===\n")
  cat("Mode:", mode, "\n")
  cat("Pieces: lowercase = white, UPPERCASE = black\n")
  cat("p/P=pawn, r/R=rook, n/N=knight, b/B=bishop, q/Q=queen, k/K=king\n\n")
  
  display_board(board)
  
  while (move_count < max_moves) {
    move_count <- move_count + 1
    current_player <- if (is_white_turn) "White" else "Black"
    
    cat("Move", move_count, ":", current_player, "to move\n")
    
    # Get valid moves
    valid_moves <- get_all_valid_moves(board, is_white_turn)
    
    if (length(valid_moves) == 0) {
      cat(current_player, "has no valid moves. Game Over!\n")
      break
    }
    
    # Choose move
    if (mode == "ai_vs_ai") {
      move <- ai_choose_move(board, is_white_turn)
      if (is.null(move)) {
        cat(current_player, "has no valid moves. Game Over!\n")
        break
      }
      cat(current_player, "moves:", move[1], "->", move[2], "\n")
      board <- make_move(board, move[1], move[2])
    }
    
    display_board(board)
    
    # Check if king is captured (simplified game over condition)
    white_king_exists <- any(board == "k")
    black_king_exists <- any(board == "K")
    
    if (!white_king_exists) {
      cat("Black wins! White's king has been captured.\n")
      break
    }
    
    if (!black_king_exists) {
      cat("White wins! Black's king has been captured.\n")
      break
    }
    
    # Switch turns
    is_white_turn <- !is_white_turn
    
    # Small delay for readability
    Sys.sleep(0.5)
  }
  
  if (move_count >= max_moves) {
    cat("\nGame ended after", max_moves, "moves (maximum reached).\n")
  }
  
  cat("\nFinal board evaluation:", evaluate_board(board), "\n")
  
  return(board)
}

# Run a simulated game only if this script is executed directly
if (sys.nframe() == 0) {
  cat("\n*** Starting Chess Game Simulation ***\n\n")
  final_board <- play_chess(mode = "ai_vs_ai", max_moves = 30)
}
