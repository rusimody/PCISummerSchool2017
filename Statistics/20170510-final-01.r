x <- read.csv( '20170510-final-01.tsv', sep = '\t', quote = '', as.is = TRUE )

n.datasets <- length( unique( x[,1] ) )

op <- par( mfrow = c( 3, 4 ) )

summaries <- NULL

for ( d in unique( x[,1] ) )
 {
  plot.new( )
  plot.window( c( 0, 105 ), c( 0, 105 ), asp = 1 )

  axis( 1, at = seq( 0, 100, by = 25 ), pos = -4 )
  axis( 2, at = seq( 0, 100, by = 25 ), pos = -4 )
  rect( 0, 0, 100, 100, col = 'lightgray', border = NA )

  for ( i in seq( 0, 100, by = 25 ) )
   {
    lines( c( 0, 100 ), rep( i, 2 ), col = 'white' )
    lines( rep( i, 2 ), c( 0, 100 ), col = 'white' )
   }

  title( main = paste( 'Dataset', d ) )
  title( xlab = 'x', line = +3 )
  title( ylab = 'y', line = -4 )

  i <- which( x[,1] == d )
  points( x[i,2:3], pch = 19, cex = 0.75 )

  lf <- lm( y ~ x, data = x )
  lines( c( 0, 100 ), coef( lf )[1] + c( 0, 100 * coef( lf )[2] ), lty = 2, col = 'red' )

  boxplot( x[i,2], add = TRUE, at = 105, horizontal = TRUE,  axes = FALSE, pars = list( boxwex = 10 ), pch = 19 )
  boxplot( x[i,3], add = TRUE, at = 105, horizontal = FALSE, axes = FALSE, pars = list( boxwex = 10 ), pch = 19 )
 }

par( op )
