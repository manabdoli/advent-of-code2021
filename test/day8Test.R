# day8 test
# x <- 'acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab |
# cdfeb fcadb cdfeb cdbaf'

x <- readLines(con = textConnection(
'be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce'
               )
)

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

segEnd <- letters[1:7] # position of segments for a to g
sigEnd <- rep(NA, 7)  # position of wires for a to g (not known yet)

# Reading through the 10 signals, transition between signals and segments
# should be built by eliminating non-possible transitions
transMat <- matrix(1, nrow=10, ncol=10) # all transitions are possible

# Part 1:
# the number of characters are used to eliminate the impossible transitions
xy[[1]]
