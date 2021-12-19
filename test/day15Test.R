# day15test
source('R/util.R')

x <- readtxtlines('1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581')
# x <- readtxtlines('116
# 108
# 213')

if(FALSE){
  riskMat <- do.call(rbind,
                     lapply(1:length(x),
                            function(r)
                              as.integer(strsplit(x[r], split = '')[[1]])))

  # Recursive approach
  bestFound <- list(path=matrix(c(1,1), nrow=1, dimnames = list(NULL, c('x', 'y'))),
                    maxRisk = sum(riskMat))
  initSearch <- riskMat*0-1
  initSearch[1,1] <- 0
  initPath <- bestFound$path
  # customized for this problem
  pos2idx <- function(pos) apos2midx(pos, nrow = nrow(initSearch))
  adjcells <- function(xy) adjmatidx(xy[1], xy[2],
                                     nrow = nrow(initSearch),
                                     ncol = ncol(initSearch),
                                     diag = FALSE)
  findNext <- function(search.mat=initSearch, cur.path=initPath, cum.risk=0,
                       cur.step=1){
    # find the search spot
    xy <- pos2idx(which(search.mat==0))
    search.mat[xy[1], xy[2]] <- cur.step
    # find possible moves
    next.idx <- adjcells(xy)
    # remove if searched already
    rmidx <- which(search.mat[next.idx]!=-1)
    if(length(rmidx)>0) next.idx <- next.idx[-rmidx, , drop=FALSE]
    # Process each new move
    ## first order by risk
    if(nrow(next.idx)>1){
      ordidx <- order(riskMat[next.idx])
      next.idx <- next.idx[ordidx, ]
    }
    while(nrow(next.idx)>0){
      #
      #if(nrow(next.idx)==1) browser()
      # pick the next cell
      curidx <- next.idx[1,, drop=FALSE]
      next.idx <- next.idx[-1,, drop=FALSE]
      # Check if search should end
      if(curidx[1]==nrow(riskMat) & curidx[2]==ncol(riskMat)){
        # update the search
        if(cum.risk+ riskMat[curidx[1], curidx[2]]<bestFound$maxRisk){
          bestFound$path <<- rbind(cur.path, curidx)
          bestFound$maxRisk <<- cum.risk + riskMat[curidx[1], curidx[2]]
          print(bestFound)
          search.mat[curidx[1], curidx[2]] <- cur.step+1
          print(search.mat)
        }
      } else{
        # curtail the search if not fruitful
        if(cum.risk + riskMat[curidx[1], curidx[2]]<=bestFound$maxRisk){
          # Update the status
          search.mat[curidx[1], curidx[2]] <- 0
          findNext(search.mat, rbind(cur.path, curidx),
                   cum.risk + riskMat[curidx[1], curidx[2]],
                   cur.step+1)
          search.mat[curidx[1], curidx[2]] <- -1
        }
      }
    }
  }

  findNext()


}

# Non-recursive approach
riskMat <- do.call(rbind,
                   lapply(1:length(x),
                          function(r)
                            as.integer(strsplit(x[r], split = '')[[1]])))

# Recursive approach
bestFound <- list(path=matrix(c(1,1), nrow=1, dimnames = list(NULL, c('x', 'y'))),
                  maxRisk = sum(riskMat))
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
cur.step <- 1
cum.risk <- sum(riskMat)
searchStack <- stackobj(list(
  list(xy = xy, cur.step=1, cum.risk=0, proccessed = FALSE)
))
cur.path <- NULL
#
while(searchStack$len()>0){
  # current node
  cur_node <- searchStack$pop()
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
      trackFound$push(list(path=cur.path, cum.risk=cur_node$cum.risk))
      if(cur_node$cum.risk<bestFound$maxRisk){
        bestFound$path <- cur.path
        bestFound$maxRisk <- cur_node$cum.risk

        print(bestFound)
        print(search.mat)
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
