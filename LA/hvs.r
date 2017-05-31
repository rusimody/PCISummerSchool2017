op <- par( mfrow = c( 2, 3 ) )

for ( epsilon in c( 1, 0.3, 0.09, 0.027, 0.0081, 0 ) )
 { 
  curve( 2 - ( 1 - epsilon ) * x, from = -15, to = +15, bty = 'n', lwd = 1.5, xlab = 'x', ylab = 'y' )
  curve( 3 - ( 1 + epsilon ) * x, from = -15, to = +15, add = TRUE, lwd = 1.5 )
  abline( h = 0, v = 0, col = 'gray' )
  title( main = parse( text = paste( 'epsilon ==', epsilon ) ) )
 }

par( op )
