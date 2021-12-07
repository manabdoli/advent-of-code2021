# day 5
## Test ####
source('l.seg.R')
x <- read.table(header = FALSE, stringsAsFactors = FALSE,
  file = 'input'
)
frm <- do.call(rbind, sapply(strsplit(x$V1, split = ','), as.integer, simplify = F))
to <- do.call(rbind, sapply(strsplit(x$V3, split = ','), as.integer, simplify = F))
# y <- cbind(paste0(frm[,1], ',', frm[,2]), '->', paste0(to[,1], ',', to[,2]))
# any(x!=y)
#rm(y)
rm(x)

segs <- lapply(1:dim(frm)[1], function(k) l.seg(frm[k,], to[k,]))

v.rng <- range(c(frm[,1], to[,1])) # vertical
h.rng <- range(c(frm[,2], to[,2])) # horizontal
xy.map <- matrix(0, ncol = diff(v.rng)+1, nrow = diff(h.rng)+1)

for(i in 1:length(segs)){
  isg <- segs[[i]]
  switch (isg$type(simple = TRUE),
          horizontal={
            for(h in seq(isg$from()[1], isg$to()[1]))
                xy.map[isg$from()[2]-h.rng[1]+1, h-h.rng[1]+1] =
                xy.map[isg$from()[2]-h.rng[1]+1, h-h.rng[1]+1] + 1
          },
          vertical={
            for(v in seq(isg$from()[2], isg$to()[2]))
              xy.map[v-v.rng[1]+1, isg$from()[1]-v.rng[1]+1] =
                xy.map[v-v.rng[1]+1, isg$from()[1]-v.rng[1]+1] + 1
          })
}

#xy.map

sum(xy.map>=2)


## Part 2: Also count diagonals
xy.map <- matrix(0, ncol = diff(h.rng)+1, nrow = diff(v.rng)+1)
for(i in 1:length(segs)){
  isg <- segs[[i]]
  switch (isg$type(simple = TRUE),
          horizontal={
            for(h in seq(isg$from()[1], isg$to()[1]))
              xy.map[isg$from()[2]-h.rng[1]+1, h-h.rng[1]+1] =
                xy.map[isg$from()[2]-h.rng[1]+1, h-h.rng[1]+1] + 1
          },
          vertical={
            for(v in seq(isg$from()[2], isg$to()[2]))
              xy.map[v-v.rng[1]+1, isg$from()[1]-v.rng[1]+1] =
                xy.map[v-v.rng[1]+1, isg$from()[1]-v.rng[1]+1] + 1
          },
          line={
            if(abs(isg$m())==1){
              for(h in seq(isg$from()[1], isg$to()[1])){
                v = isg$y(h)
                xy.map[v-v.rng[1]+1, h-h.rng[1]+1] =
                  xy.map[v-v.rng[1]+1, h-h.rng[1]+1] + 1
              }
            }
          })
}


#xy.map

sum(xy.map>=2)
