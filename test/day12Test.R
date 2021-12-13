# day12Test
# After a lot of internal debates, I decided to use a matrix to represent the
# connectivity graph any ay time (where 1 represents a possible transition from
# a row to a column.)
# I also decided to use a stacked list (stack of list objects) to track the
# possible paths. The process would be a recursive process, where the list
# grows at each step and the matrix's 1 shrinks.
#   (I am assuming that the graph is eventually skips cycles like A-B-A-B-...)
#

source('R/util.R')
# read data
x <- readtxtlines(
  'start-A
start-b
A-c
A-b
b-d
A-end
b-end'
)
x

# consruct the graph
connections <- do.call(rbind, strsplit(x, split = "-"))
caves <- sort(unique(c(connections[,1], connections[,2])))
n <- length(caves)
icaves <- lapply(1:n, function(x) x)
names(icaves) <- caves

congraph <- matrix(0, nrow = n, ncol = n)
for(i in 1:dim(connections)[1]){
  fr <- icaves[[connections[i, 1]]]
  to <- icaves[[connections[i, 2]]]
  congraph[fr, to] <- congraph[to, fr] <- 1
}

