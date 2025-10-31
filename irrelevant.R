
n <- 100
length <- 61

data <- replicate(n, rnorm(length, mean = 1, sd = 0.5))

avg_row <- apply(data, 2, function(x){
  result <- (x[seq(1,length-1)] + x[seq(2,length)]) /2
})

# Step 1: Compute pairwise averages between adjacent columns
num_cols <- ncol(avg_row)  # Get the number of columns
num_rows <- nrow(avg_row)  # Get the number of rows

# Ensure even number of columns
if (num_cols %% 2 == 1) {
  avg_row <- avg_row[, -num_cols]  # Remove the last column if odd
}

# Compute pairwise column-wise averages
avg_adjacent_matrix <- matrix(NA, nrow = num_rows, ncol = num_cols / 2)

for (j in seq(1, num_cols - 1, by = 2)) {
  avg_adjacent_matrix[, (j + 1) / 2] <- (avg_row[, j] + avg_row[, j + 1]) / 2
}



# Step 2: Compute pairwise averages between adjacent columns
num_cols1 <- ncol(avg_adjacent_matrix)  # Get the number of columns
num_rows1 <- nrow(avg_adjacent_matrix)  # Get the number of rows

# Ensure even number of columns
if (num_cols %% 2 == 1) {
  avg_adjacent_matrix <- avg_adjacent_matrix[, -num_cols]  # Remove the last column if odd
}

# Compute pairwise column-wise averages
avg_adjacent_matrix1 <- matrix(NA, nrow = num_rows1, ncol = num_cols1 / 2)

for (j in seq(1, num_cols1 - 1, by = 2)) {
  avg_adjacent_matrix1[, (j + 1) / 2] <- (avg_adjacent_matrix[, j] + avg_adjacent_matrix[, j + 1]) / 2
}



# Step 3: Compute pairwise averages between adjacent columns
num_cols2 <- ncol(avg_adjacent_matrix1)  # Get the number of columns
num_rows2 <- nrow(avg_adjacent_matrix1)  # Get the number of rows

# Ensure even number of columns
if (num_cols2 %% 2 == 1) {
  avg_adjacent_matrix1 <- avg_adjacent_matrix1[, -num_cols]  # Remove the last column if odd
}

# Compute pairwise column-wise averages
avg_adjacent_matrix2 <- matrix(NA, nrow = num_rows2, ncol = num_cols2 / 2)

for (j in seq(1, num_cols2 - 1, by = 2)) {
  avg_adjacent_matrix2[, (j + 1) / 2] <- (avg_adjacent_matrix1[, j] + avg_adjacent_matrix1[, j + 1]) / 2
}



# Step 4: Compute pairwise averages between adjacent columns
num_cols3 <- ncol(avg_adjacent_matrix2)  # Get the number of columns
num_rows3 <- nrow(avg_adjacent_matrix2)  # Get the number of rows

# Ensure even number of columns
if (num_cols3 %% 2 == 1) {
  avg_adjacent_matrix2 <- avg_adjacent_matrix2[, -num_cols]  # Remove the last column if odd
}

# Compute pairwise column-wise averages
avg_adjacent_matrix3 <- matrix(NA, nrow = num_rows3, ncol = num_cols3 / 2)

for (j in seq(1, num_cols3 - 1, by = 2)) {
  avg_adjacent_matrix3[, (j + 1) / 2] <- (avg_adjacent_matrix2[, j] + avg_adjacent_matrix2[, j + 1]) / 2
}




# Step 5: Compute pairwise averages between adjacent columns
num_cols4 <- ncol(avg_adjacent_matrix3)  # Get the number of columns
num_rows4 <- nrow(avg_adjacent_matrix3)  # Get the number of rows

# Ensure even number of columns
if (num_cols4 %% 2 == 1) {
  avg_adjacent_matrix3 <- avg_adjacent_matrix3[, -num_cols]  # Remove the last column if odd
}

# Compute pairwise column-wise averages
avg_adjacent_matrix4 <- matrix(NA, nrow = num_rows4, ncol = num_cols4 / 2)

for (j in seq(1, num_cols4 - 1, by = 2)) {
  avg_adjacent_matrix4[, (j + 1) / 2] <- (avg_adjacent_matrix3[, j] + avg_adjacent_matrix3[, j + 1]) / 2
}



# Step 5: Compute pairwise averages between adjacent columns
num_cols5 <- ncol(avg_adjacent_matrix4)  # Get the number of columns
num_rows5 <- nrow(avg_adjacent_matrix4)  # Get the number of rows

# Ensure even number of columns
if (num_cols5 %% 2 == 1) {
  avg_adjacent_matrix4 <- avg_adjacent_matrix4[, -num_cols]  # Remove the last column if odd
}

# Compute pairwise column-wise averages
avg_adjacent_matrix5 <- matrix(NA, nrow = num_rows5, ncol = num_cols5 / 2)

for (j in seq(1, num_cols5 - 1, by = 2)) {
  avg_adjacent_matrix5[, (j + 1) / 2] <- (avg_adjacent_matrix4[, j] + avg_adjacent_matrix4[, j + 1]) / 2
}

final_values <- as.data.frame(avg_adjacent_matrix5) 
ser <- c(1:60)

plot(final_values[,"V1"],ser)
