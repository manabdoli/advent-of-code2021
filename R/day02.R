# day 2
x <- read.table(file='data/day02/input.txt', header = FALSE, as.is = TRUE)
pos <- c(0,0)
#
for(i in 1:dim(x)[1]){
  switch (x$V1[i],
    down = {pos[2] <- pos[2]+x$V2[i]},
    forward = {pos[1] <- pos[1]+x$V2[i]},
    up = {pos[2] <- pos[2]-x$V2[i]}
  )
  #print(rbind(unlist(x[i, ]), pos))
}

prod(pos)


# Part 2
pos <- c(H=0, aim=0, depth=0)
#
for(i in 1:dim(x)[1]){
  switch (x$V1[i],
          down = {pos['aim'] <- pos['aim']+x$V2[i]},
          forward = {
            pos['H'] <- pos['H']+x$V2[i]
            pos['depth'] <- pos['depth']+pos['aim']*x$V2[i]
          },
          up = {pos['aim'] <- pos['aim']-x$V2[i]}
  )
  #print(rbind(unlist(x[i, ]), pos))
}

pos['H']*pos['depth']
