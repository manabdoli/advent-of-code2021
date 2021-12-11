# Day 7 Test
x <- c(16,1,2,0,4,2,7,1,2,14)

# The answer is median
median(x)

# cost is sum of absolute deviations
sum(abs(x-2))

# Alternatively, we could find the best by finding the minimum cost
x.rng <- range(x)
x.seq <- seq(x.rng[1], x.rng[2])
costs <- sapply(x.seq,
                function(m) sum(abs(x-m)))
minIdx <- which.min(costs)
x.seq[minIdx]
c(x.seq[minIdx], costs[minIdx])

# Part 2
# new cost function 1 + 2 + ... + x-s, where s is the solution
# it turns out that if x>s, this sum is (x-s)*(x-s+1)/2
cost <- function(x, s) abs(x-s)*(abs(x-s)+1)/2

# Using a search approach:
x.rng <- range(x)
x.seq <- seq(x.rng[1], x.rng[2])
costs <- sapply(x.seq,
                function(m) sum(cost(x, m)))
minIdx <- which.min(costs)
c(x.seq[minIdx], costs[minIdx])
