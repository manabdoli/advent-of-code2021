# Advent of Code:
x <- read.table(file = 'data/day1/input')
#1
sum(diff(x)>0)
#2
n=length(x)
r <- 3
s3 <- sapply(r:n, function(k) sum(x[k-(0:2)]))
sum(diff(s3)>0)
