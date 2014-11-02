simplePlot = function(){
	# This function generate 2D scatter plot
	x = seq(0, 24, 1) # making an array started with 0, end with 24 and go up by 1 each step
	y = logGrowth(x, 0.3, 1)
	z = logGrowth(x, 0.2, 1)
	plot(x,y, xlab = 'time (hr)', ylab = 'OD', pch = 21, bg = "red",
     xlim=c(0, 24), ylim=c(0, 1)) #pch specify characteristic of a dot (21 is for a circle), bg is color of the dot
	points(x,z, xlab = 'time (hr)', ylab = 'OD', pch = 21, bg = "blue")
}

logGrowth = function(t, r, K){
   # This function calculated a logistic growth function at time t for population with growth rate r and carrying capacity K
   OD = K/(1 + exp(-r*t))
   return(OD)
}