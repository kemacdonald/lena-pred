### Entropy helpers

get_symbol_entropy <- function(x) {
  if(x == 0) {
    0
  } else {
    (x * log2(x)) * -1  
  }
  
}
compute_entropy <- function(vect) {
  # get counts of each symbol in the vector
  counts <- table(vect)
  # get probability of each symobol in the vector
  probs <- counts / length(vect)
  # compute entropy 
  sapply(probs, get_symbol_entropy) %>% sum()
}