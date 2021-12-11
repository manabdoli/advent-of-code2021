# Advent of Code:
x <- c(199, 200, 208, 210, 200, 207, 240, 269, 260, 263)

#1
sum(diff(x)>0)
#2
n=length(x)
r <- 3
s3 <- sapply(r:n, function(k) sum(x[k-(0:2)]))
sum(diff(s3)>0)


sum(diff(x = x, lag = 3)>0)
