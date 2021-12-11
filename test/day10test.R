# day10test
x <- readLines(
  con = textConnection(
    '[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]'))

# Creating a stack with push and pop functions
stackobj <- function(x=NULL){
  #
  len <- length(x)
  # push
  push <- function(element){
    x <<- c(element, x)
    len <<- len + 1
    x
  }
  # pop
  pop <- function(){
    element <- x[1]
    len <<- len - 1
    x <<- x[-1]
    element
  }

  structure(
    list(
      push=push,
      pop=pop,
      len=function() len,
      top=function() x[1],
      clear=function(){
        x<<-NULL
        len<<-0
        x},
      stack=function() x
    ),
    class = 'stack'
  )
}

print.stack <- function(x, ...){
  cat('Stack of ', x$len(), ' objects: ')
  cat(paste0(x$stack(), collapse = ', '))
  cat('\n')
}


mystack <- stackobj()
o.brakets <- c('(', '[', '{', '<')
c.brakets <- c(')', ']', '}', '>')
score <- c(3, 57, 1197, 25137)
o.match <- function(cb){
  o.brakets[grep(cb, c.brakets, fixed = T)]
}
c.match <- function(ob){
  c.brakets[grep(ob, o.brakets, fixed = T)]
}

is.o.bracket <- function(b) {
  if(any(grepl(b, o.brakets, fixed = TRUE))) TRUE else
    if(any(grepl(b, c.brakets, fixed = TRUE))) FALSE else
      stop('invalid character!')
}

corrup <- data.frame()
incomplete <- data.frame()
for(i in 1:length(x)){
  xi <- strsplit(x[i], split = '')[[1]]
  mystack$clear()
  corrupted <- FALSE
  for(j in 1:length(xi)){
    if(is.o.bracket(xi[j])){
      mystack$push(xi[j])
    } else{
      obrket <- mystack$pop()
      corrupted <- xi[j]!=c.match(obrket)
    }
    if(corrupted) break
  }
  if(corrupted){
    cat(x[i], ': Expected ', c.match(obrket),
        ', but found ', xi[j], ' instead.\n')
    corrup <- rbind(corrup, data.frame(idx=j, line=x[i],
                                       score=score[c.brakets==xi[j]]))
  } else if(mystack$len()>0){
    incomplete <- rbind(incomplete, data.frame(idx=j, line=x[i]))
  }
}

corrup
sum(corrup$score)

incomplete
