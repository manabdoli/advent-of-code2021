# day12Test
# I first decided to use a matrix to represent the connectivity graph
# at any time (where 1 represents a possible transition from a row to
# a column). But, I realized many copies of this is needed in a recursive
# approach to finding the paths, which means a lot of wasted memory. So,
# I settled for a pair of lists (or a list of two arrays).
# I also decided to use a stacked list (stack of list objects) to track the
# possible paths. But a recursive process may solve the need to constructing
# a stack object.
#   (I am assuming that the graph is eventually skips cycles like A-B-A-B-...)
#

source('R/util.R')
paths <- isLower <- fromtolist <- NULL
day12setup <- function(x){
  # possible connections
  connections <- do.call(rbind, strsplit(x, split = "-"))
  fromto <- cbind(from=c(connections[,1], connections[,2]),
                  to=c(connections[,2], connections[,1]))
  # list of caves
  caves <- sort(unique(fromto[,1]))
  names(caves) <- caves
  isLower <- grepl('[a-z]', caves)
  names(isLower) <- caves

  n <- length(caves)
  # for part II
  repallowed <- sapply(names(isLower),
                       function(acave) {
                         if(isLower[acave])
                           ifelse(nchar(acave)==1, 2L, 1L) else
                             Inf
                       }
  )

  # eliminate "start" from "to" and "end" from "from"
  fromto <- fromto[-which(fromto[,1]=="end" | fromto[,2]=="start"),]

  # construct the graph
  fromtolist <- lapply(caves, function(a) fromto[which(fromto[,1]==a), 2])
  fromtolist$end <- NULL
  list(isLower=isLower, repallowed=repallowed, fromtolist=fromtolist)
}

# remove a cave
rmCave <- function(tos, acave){
  rmIdx <- NULL
  for(i in 1:length(tos)){
    rmidx <- which(tos[[i]]==acave)
    if(length(rmidx)>0){
      tos[[i]] <- tos[[i]][-rmidx]
      if(length(tos[[i]]==0)) rmIdx <- c(rmIdx, i)
    }
  }
  tos
}

nextCave <- function(from='start', tos=fromtolist, history='start'){
  # any place to go to
  #cat('From: ', from, '\n')
  #cat('TOS: ')
  #print(tos)
  #cat('History ', history, '\n')
  if(length(tos)==0 | is.null(tos[[from]]) | length(tos[[from]]==0)){
    # go to each of available cave; remove the lower case caves
    for(j in 1:length(tos[[from]])){
      newCave <- tos[[from]][j]
      #print(newCave)
      # if end, add the path
      if(newCave=='end'){
        #print(paste0(history, ',', newCave))
        paths <<- c(paths, paste0(history, ',', newCave))
      } else{
        if(isLower[newCave]){
          nextCave(newCave, rmCave(tos, newCave),
                   paste0(history, ",", newCave))
        } else{
          nextCave(newCave, tos, paste0(history, ",", newCave))
        }
      }
    }
  }
  NULL
}

# For part II
nextCave2 <- function(from='start', tos=fromtolist, history='start',
                      repallowed=FALSE){
  # any place to go to
  if(length(tos)==0 | is.null(tos[[from]]) | length(tos[[from]]==0)){
    # go to each of available cave; remove the lower case caves
    for(j in 1:length(tos[[from]])){
      newCave <- tos[[from]][j]
      #print(newCave)
      # if end, add the path
      if(newCave=='end'){
        #print(paste0(history, '-', newCave))
        paths <<- c(paths, paste0(history, ',', newCave))
      } else{
        if(isLower[newCave]){
          nextCave2(newCave, rmCave(tos, newCave),
                    paste0(history, ",", newCave),
                    repallowed)
          if(repallowed){
            # this creates duplicates - a better approach should be used
            nextCave2(newCave, tos, paste0(history, ",", newCave), FALSE)
            }
          } else{
            nextCave2(newCave, tos, paste0(history, ",", newCave),
                      repallowed)
          }
      }
    }
  }
}


# Test 1####
## Part I
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
setupList <- day12setup(x)

# Run Part I
paths <- NULL
isLower <- setupList$isLower
fromtolist <- setupList$fromtolist
nextCave2()
length(paths)

# Run Part II
paths <- NULL
nextCave2(repallowed = TRUE)
length(unique(paths))

# Test 2
x <- readtxtlines(
  'dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc'
)
# Run
paths <- NULL
setupList <- day12setup(x)
isLower <- setupList$isLower
fromtolist <- setupList$fromtolist
nextCave()
length(paths)

paths <- NULL
nextCave2(repallowed = TRUE)
length(unique(paths))

# Test 3
x <- readtxtlines(
  'fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW'
)
# Run
paths <- NULL
setupList <- day12setup(x)
isLower <- setupList$isLower
fromtolist <- setupList$fromtolist
nextCave()
length(paths)
paths <- NULL
nextCave2(repallowed = TRUE)
length(unique(paths))
