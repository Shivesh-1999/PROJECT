library(data.table)
library(ggplot2)
set.seed(0)

FUN_ruin <- function(p, N, j) {
  
  # loop
  while (TRUE) {
    
    
    # break condition
    if (j[length(j)] == N || j[length(j)] == 0) {
      return(j)
    }
    
    # iterate
    const_play <- rbinom(1, 1, p)
    
    if (const_play == 1) {
      j <- c(j, j[length(j)] + 1)
    }
    
    if (const_play == 0) {
      j <- c(j, j[length(j)] - 1)
    }
  }
}


FUN_ruinprob <- function(N, p, j) {
  
  # edge cases
  if (p == 0) {
    return(0)
  }
  
  if (p == 1) {
    return(1)
  }
  if (p == 1 / 2) {
    return(j / N)
  }
  
  q <- 1 - p
  return((1 - (q / p) ^ j) / (1 - (q / p) ^ N))
}

FUN_ruinprob(50,0.83,10)

data <- data.table(A=FUN_ruin(p = .83, N = 50, j = 10))
data$t <- 1:nrow(data)
data
ggplot(data, aes(x = t, y = A)) +
  geom_line() +
  ggtitle("Gambler's Ruin") +
  theme_bw() + ylim(c(0, 100)) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = 1)