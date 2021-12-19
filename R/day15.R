# day15test
t0 <- proc.time()
source('R/util.R')

x <- readfilelines('data/day15/input')
# x <- readtxtlines('116
# 138
# 213')
# Non-recursive approach
riskMat <- do.call(rbind,
                   lapply(1:length(x),
                          function(r)
                            as.integer(strsplit(x[r], split = '')[[1]])))
riskMat <- riskMat[1:10, 1:10]
nm <- dim(riskMat)
# Recursive approach
bestFound <- list(path=matrix(c(1,1), nrow=1, dimnames = list(NULL, c('x', 'y'))),
                  maxRisk = min(sum(riskMat[1,], riskMat[,nm[2]]),
                                sum(riskMat[,1], riskMat[nm[1],])))
trackFound <-stackobj()
initSearch <- riskMat*0-1
initSearch[1,1] <- 0
initPath <- bestFound$path
# customized for this problem
pos2idx <- function(pos) apos2midx(pos, nrow = nrow(initSearch))
adjcells <- function(xy) adjmatidx(xy[1], xy[2],
                                   nrow = nrow(initSearch),
                                   ncol = ncol(initSearch),
                                   diag = FALSE)

# Starting search
search.mat <- initSearch
xy <- initPath
numSol <- 0
cum.risk <- sum(riskMat)
searchStack <- stackobj(list(
  list(xy = xy, cur.step=1, cum.risk=0, proccessed = FALSE)
))
cur.path <- NULL
#
while(searchStack$len()>0){
  # current node
  cur_node <- searchStack$pop()
  cat(sprintf('Solutions found %2d, Max Risk = %6d, Step: %5d at (%2d, %2d)\n',
              numSol, bestFound$maxRisk, cur_node$cur.step, cur_node$xy[1],
              cur_node$xy[2]))
  curidx <- cur_node$xy
  if(cur_node$proccessed){
    # reset the search and go 1 step backward
    search.mat[curidx[1], curidx[2]] <- -1
    cur.path <- head(cur.path, -1)
  } else{
    search.mat[curidx[1], curidx[2]] <- cur_node$cur.step
    cur.path <- rbind(cur.path, curidx)
    # if it is the last node
    if(curidx[1]==nrow(riskMat) & curidx[2]==ncol(riskMat)){
      # update the search
      numSol <- numSol + 1
      trackFound$push(list(path=cur.path, cum.risk=cur_node$cum.risk))
      if(cur_node$cum.risk<bestFound$maxRisk){
        bestFound$path <- cur.path
        bestFound$maxRisk <- cur_node$cum.risk

        #print(search.mat)
        print(bestFound$maxRisk)
      }
      # done; reset
      search.mat[curidx[1], curidx[2]] <- -1
      cur.path <- head(cur.path, -1)
    } else{
      # find possible moves
      next.idx <- adjcells(curidx)
      # remove if searched already
      rmidx <- which(search.mat[next.idx]!=-1)
      if(length(rmidx)>0) next.idx <- next.idx[-rmidx, , drop=FALSE]
      if(NROW(next.idx)>0){
        # Save the current node:
        cur_node$proccessed <- TRUE
        searchStack$push(cur_node)

        # Add potential new nodes if useful
        # order by worst to best (so best is proccessed first)
        if(nrow(next.idx)>1){
          ordidx <- order(riskMat[next.idx], decreasing = TRUE)
          next.idx <- next.idx[ordidx, ]
        }
        for(i in 1:NROW(next.idx)){
          irsk <- cur_node$cum.risk + riskMat[next.idx[i,1], next.idx[i,2]]
          if(irsk<=bestFound$maxRisk)
            searchStack$push(list(xy = next.idx[i, , drop=FALSE],
                                  cur.step=cur_node$cur.step+1,
                                  cum.risk=irsk,
                                  proccessed=FALSE))
        }
      }else {
        # reset
        # reset the search and go 1 step backward
        search.mat[curidx[1], curidx[2]] <- -1
        cur.path <- head(cur.path, -1)
      }
    }
  }
}

proc.time()-t0
