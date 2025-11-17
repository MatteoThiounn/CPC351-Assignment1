allowed <- c(as.character(1:9), "A", "B", "C")

read_puzzle <- function(filename) {
  as.matrix(read.table(filename,
                       sep = ";",
                       stringsAsFactors = FALSE,
                       colClasses = "character"))
}

find_duplicates <- function(vec) {
  tab <- table(vec)
  dup <- names(tab[tab > 1])
  return(dup)
}

check_rows <- function(puzzle) {
  violations <- list()
  for (i in 1:nrow(puzzle)) {
    row_vals <- puzzle[i, ]
    dup <- find_duplicates(row_vals)
    invalid <- setdiff(row_vals, allowed)
    
    if (length(dup) > 0)
      violations[[length(violations)+1]] <-
      paste("Row", i, "has duplicate values:", paste(dup, collapse = ", "))
    
    if (length(invalid) > 0)
      violations[[length(violations)+1]] <-
      paste("Row", i, "contains invalid symbols:", paste(invalid, collapse = ", "))
  }
  return(violations)
}

check_columns <- function(puzzle) {
  violations <- list()
  for (j in 1:ncol(puzzle)) {
    col_vals <- puzzle[, j]
    dup <- find_duplicates(col_vals)
    invalid <- setdiff(col_vals, allowed)
    
    if (length(dup) > 0)
      violations[[length(violations)+1]] <-
      paste("Column", j, "has duplicate values:", paste(dup, collapse = ", "))
    
    if (length(invalid) > 0)
      violations[[length(violations)+1]] <-
      paste("Column", j, "contains invalid symbols:", paste(invalid, collapse = ", "))
  }
  return(violations)
}

check_blocks <- function(puzzle) {
  violations <- list()
  block_num <- 1
  
  for (r in seq(1, 12, by = 3)) {
    for (c in seq(1, 12, by = 4)) {
      block <- puzzle[r:(r+2), c:(c+3)]
      vals <- as.vector(block)
      dup <- find_duplicates(vals)
      
      if (length(dup) > 0)
        violations[[length(violations)+1]] <-
        paste("Block", block_num, "( Rows", r, "-", r+2,
              ", Cols", c, "-", c+3,
              ") has duplicate values:", paste(dup, collapse = ", "))
      
      block_num <- block_num + 1
    }
  }
  return(violations)
}

validate_puzzle <- function(puzzle) {
  violations <- c(check_rows(puzzle),
                  check_columns(puzzle),
                  check_blocks(puzzle))
  
  if (length(violations) == 0) {
    cat("The puzzle is feasible.\n")
  } else {
    cat("The puzzle is infeasible.\n")
    cat("The following violations have been found:\n")
    for (v in violations) cat(" -", v, "\n")
  }
}

# -------------------------------------
setwd("C:/Data/CPC351")
puzzle1 <- read_puzzle("input1.txt")
puzzle2 <- read_puzzle("input2.txt")
validate_puzzle(puzzle1)
validate_puzzle(puzzle2)
# -------------------------------------
