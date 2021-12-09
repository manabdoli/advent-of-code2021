# day 8
x <- readLines(con = file('data/day8/input'))

xy <- strsplit(x, split=' | ', fixed=TRUE)
puzzle <- lapply(xy, function(aCase){
  list(
    learn = strsplit(aCase[1], split = ' ')[[1]],
    solve = strsplit(aCase[2], split = ' ')[[1]]
  )
}
)

# 7-segment characteristics
segs <- function(digit, invert=FALSE){
  if(!invert){
    switch(digit+1,
           'abcefg', 'cf', 'acdeg', 'acdfg',
           'bcdf', 'abdfg', 'abdefg', 'acf',
           'abcdefg', 'abcdfg'
    )
  } else{
    switch(digit+1,
           'd', 'abdeg', 'bf', 'be',
           'aeg', 'ce', 'c', 'bdeg',
           '', 'e'
    )
  }
}

# Just to test if I got them right
print7seg <- function(digit){
  segstr <- ' aaaa
b    c
b    c
 dddd
e    f
e    f
 gggg '
  rmvSegs <- strsplit(segs(digit, invert = TRUE), split = '')[[1]]
  for(ch in rmvSegs)
    segstr <- gsub(pattern = ch, replacement = '', segstr)
  cat(segstr, '\n')
}

#print7seg(9)


segDF <- data.frame(digits=0:9, segs=sapply(0:9, segs),
                    len=nchar(sapply(0:9, segs)))
segDF$is.uniq <- sapply(0:9,
                        function(k) sum(segDF$len[k+1]==segDF$len)==1)
uniq.lens <- segDF$len[segDF$is.uniq]
uniq.count <- sapply(puzzle, function(l){
  sum(sapply(l$solve, function(ges) nchar(ges)%in%uniq.lens))
})
sum(uniq.count)

