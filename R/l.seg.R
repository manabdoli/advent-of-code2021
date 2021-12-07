# Line Seqment
l.seg <- function(from, to=from){
  # Type: point, vertical, horizontal or line
  type <- if(from[1]==to[1]){
    if(from[2]==to[2]){
      'point'
    } else{
      'vertical'
    }
  } else{
    if(from[2]==to[2]){
      'horizontal'
    } else{
      'line'
    }
  }
  # slope
  slope <- switch (
    type,
    point = NA,
    horizontal = 0,
    vertical = sign(to[2]-from[2])*Inf,
    line = (to[2]-from[2])/(to[1]-from[1])
  )

  # intercept
  intercept <- switch(
    type,
    point = if(from[1]==0) from[2] else NA,
    horizontal = from[2],
    vertical = if(from[1]==0) Inf else NA,
    line = to[2]-slope*to[1]
  )

  # pass through
  passes <- function(x, y){
    switch (
      type,
      point = (from[1]==x & from[2]==y),
      horizontal = (from[2]==y),
      vertical = (from[1]==x),
      line = (y==intercept+slope*x)
    )
  }

  onsegment <- function(x, y){
    if(passes()){
      x>=from[1] & x<=from[2]
    } else{
      FALSE
    }
  }

  # formula
  formula <- function(showType=TRUE){
    switch(
      type,
      point = sprintf('%s(X=%g, Y=%g)',
                      if(showType) 'Point: ' else '', from[1], from[2]),
      horizontal = sprintf('%sY = %g',
                           if(showType) 'Horizontal: ' else '', from[2]),
      vertical = sprintf('%sX = %g',
                         if(showType) 'Horizontal: ' else '', from[1]),
      line = sprintf('%sY=%g + %g * x',
                     if(showType) 'Line: ' else '', intercept, slope)
    )
  }

  y <- function(x){
    switch(
      type,
      point = if(x==from[1]) from[2] else NA,
      horizontal = intercept,
      vertical = intercept,
      line = intercept+slope*x
    )
  }

  x <- function(y){
    switch(
      type,
      point = if(y==from[2]) from[1] else NA,
      horizontal = if(y==from[2]) Inf else NA,
      vertical = from[1],
      line = (y-intercept)/slope
    )
  }

  structure(
    list(m=function() slope,
         b=function() intercept,
         y = y, x = x,
         from=function() from,
         to=function() to,
         type=function(simple=TRUE)
           if(simple) type else
             paste0(type, if(! type %in% c('point', 'line')) ' line'),
         formula = formula,
         onsegment = onsegment,
         passes = passes),
    class = 'mxbLine'
  )
}

print.mxbLine <- function(x, ...){
  if(x$type(simple=TRUE)=='point'){
    cat(x$formula(showType=TRUE), '\n')
  } else{
    cat(sprintf('A %s from (%g, %g) to (%g, %g): %s',
                x$type(simple=FALSE), x$from()[1], x$from()[2],
                x$to()[1], x$to()[2], x$formula(showType=FALSE)))
  }
}
