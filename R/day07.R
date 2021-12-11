# Read inout data
xtxt <- readLines(con = file('data/day07/input.txt'))
x <- eval(parse(text = paste0('c(', xtxt, ')')))
# The answer is median
(x.med <- median(x))

# cost is sum of absolute deviations
sum(abs(x-x.med))


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
print(c(x.seq[minIdx], costs[minIdx]))
