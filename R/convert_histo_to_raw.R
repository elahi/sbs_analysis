# size_bin_vector = childs$length_mm
# count_vector = childs$count_B
# size_interval = 1
# i = 9


get_random_sizes <- function(size_bin_vector, count_vector, size_interval){
  # Initialize empty vector
  raw_sizes <- vector()
  # Draw a random value according to a normal distribution
  # mean = center of size bin
  # sd = half of size bin (so that 96% [2 SDs] of values will lie within size bin)
  for(i in 1:length(size_bin_vector)){
    size_bin_i <- size_bin_vector[i]
    count_i <- count_vector[i]
    size_vector_i <- rnorm(n = count_i, mean = size_bin_i, sd = size_interval/2)
    
    # Correct min and max sizes
    size_vector_i[size_vector_i > size_bin_i + size_interval] <- size_bin_i + size_interval
    size_vector_i[size_vector_i < size_bin_i - size_interval] <- size_bin_i - size_interval
    
    raw_sizes <- c(raw_sizes, size_vector_i)
  }
  return(raw_sizes)
}

repeat_sizes <- function(size_bin_vector, count_vector){
  # Initialize empty vector
  raw_sizes <- vector()
  
  for(i in 1:length(size_bin_vector)){
    size_bin_i <- size_bin_vector[i]
    count_i <- count_vector[i]
    size_vector_i <- rep(size_bin_i, count_i)
    raw_sizes <- c(raw_sizes, size_vector_i)
  }
  return(raw_sizes)
}
