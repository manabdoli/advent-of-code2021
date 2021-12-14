#day 12 # Worst algorithm so far
source('R/util.R')
# read data
x <- readfilelines('data/day12/input')

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

setupList <- day12setup(x)

# Part I
paths <- NULL
isLower <- setupList$isLower
fromtolist <- setupList$fromtolist
nextCave2()
length(paths)

# Part I
paths <- NULL
nextCave2(repallowed = TRUE)
length(unique(paths))
