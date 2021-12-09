# R9 Test
x <- readLines(con = textConnection('2199943210
3987894921
9856789892
8767896789
9899965678'))
floorMap <- t(sapply(sapply(x, strsplit, split=''), as.numeric))
rownames(floorMap) <- NULL

minAdjacent <- (floorMap*0)>0
rc <- dim(minAdjacent)

for(i in 1:rc[1]){
  ridx <-
    if(i==1) 2 else
      if(i==rc[1]) rc[1]-1 else
        i+c(-1, 1)
  for(j in 1:rc[2]){
    cidx <-
      if(j==1) 2 else
        if(j==rc[2]) rc[2]-1 else
          j+c(-1, 1)
    idx <- rbind(
      cbind(ridx, j),
      cbind(i, cidx)
    )
    minAdjacent[i, j] <- all(
      apply(idx, 1, function(acell)
        floorMap[i,j]<floorMap[acell[1], acell[2]])
    )
  }
}

sum(floorMap[minAdjacent]+1)


# Part II ####
adjacentIdx <- function(i, j, rc){
  ridx <-
    if(i==1) 2 else
      if(i==rc[1]) rc[1]-1 else
        i+c(-1, 1)
  cidx <-
    if(j==1) 2 else
      if(j==rc[2]) rc[2]-1 else
        j+c(-1, 1)
  idx <- rbind(
    cbind(ridx, j),
    cbind(i, cidx)
  )
}

which(minAdjacent)

# Part II
