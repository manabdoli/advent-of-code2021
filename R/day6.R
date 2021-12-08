# day 6 test
y <- readLines(con = file('data/day6/input'))
x <- eval(expr = parse(text = paste0('c(', y, ')')))
idx0 <- which(x==0)
for(i in 1:80){
  # Reduce by 1
  x <- x-1
  # Replace previous zeros and add new fish
  if(length(idx0)>0){
    x[idx0] <- 6
    x <- c(x, rep(8, length(idx0)))
  }
  # find 0's
  idx0 <- which(x==0)
}
length(x)


# More efficient
y <- readLines(con = file('data/day6/input'))
x <- eval(expr = parse(text = paste0('c(', y, ')')))
cnt <- rep(0L, 9)
for(j in x) cnt[j+1] <- cnt[j+1]+1

for(i in 1:256){
  # Count the 0 days
  cnt0 <- cnt[1]
  # Reduce counts by 1
  cnt <- cnt[-1]
  # Add 8's
  cnt <- c(cnt, cnt0)
  # Add 6's
  cnt[6+1] <- cnt[6+1]+cnt0
}
sum(cnt)
print(sprintf('%25f', sum(cnt)))
