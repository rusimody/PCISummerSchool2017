f <- function( x, lambda ) { lambda * x * ( 1 - x ) }

iterate.f <- function( f, x, n, ... )
 {
  for ( i in 2:n ) x[i] <- f( x[i-1], ... )
  return( x )
 }

x <- iterate.f( f, 0.1, 1000, lambda = 4 )

plot( x[-n], x[-1], cex = 0.5 )
