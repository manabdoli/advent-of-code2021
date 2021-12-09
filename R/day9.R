# R9
inputFile <- file('data/day9/input')
x <- readLines(con = inputFile)
close(inputFile)

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
# Part II ####
adjacentIdx <- function(i, j, nm=rc){
  #browser()
  ridx <-
    if(i==1) 2 else
      if(i==nm[1]) nm[1]-1 else
        i+c(-1, 1)
  cidx <-
    if(j==1) 2 else
      if(j==nm[2]) nm[2]-1 else
        j+c(-1, 1)
  idx <- rbind(
    cbind(ridx, j),
    cbind(i, cidx)
  )
  colnames(idx) <- c('i', 'j')
  # remove duplicates
  matrix(idx, ncol=2, byrow = FALSE)
}

# low points
lows <- which(minAdjacent)
cols <- (lows%/%rc[1])+1
rows <- (lows-1)%%rc[1]+1
lows <- cbind(rows, cols)

lows <- apply(lows, 1, list)
# Grow each basin
basins <- lapply(lows,
                 function(l){
                   # Basin
                   cur.basin <- matrix(l[[1]], ncol=2)
                   grow.nodes <- cur.basin
                   while(dim(grow.nodes)[1]>0){
                     # Find adjacent nodes
                     new.nodes <- lapply(1:dim(grow.nodes)[1],
                                         function(k)
                                           adjacentIdx(grow.nodes[k,1], grow.nodes[k,2]))
                     # Turn into a matrix
                     new.nodes <- unique(do.call(rbind, new.nodes))
                     # remove nodes already in basin
                     ridx <- apply(new.nodes, 1,
                                   function(l){
                                     # remove if value is 9
                                     floorMap[l[1], l[2]]==9 |
                                       # or if it is already in cur.basin
                                       any(cur.basin[,1]==l[1] & cur.basin[,2]==l[2])
                                   })
                     # clean new nodes
                     new.nodes <- new.nodes[!ridx, ,drop=FALSE]
                     if(dim(new.nodes)[1]>0)
                       cur.basin <- rbind(cur.basin, new.nodes)
                     grow.nodes <- new.nodes
                   }
                   cur.basin
                 })
prod(sort(sapply(basins, function(b) dim(b)[1]), decreasing = TRUE)[1:3])


