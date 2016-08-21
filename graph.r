# Plots the histograns and the theta , beta distributions
# for a given value of the exponential distribution .
plot_theta_betas <- function ( exp_theta ) {
	theta_bet = c (0.9 , 0.9 , 2 , 2)
	let = c ( "A" , "B" , "C" , "D" )
	for ( i in 1:4) {
	x_vec = modified_weibull(theta_bet[i] , theta_bet [ i ] , 10000)
	name = paste ( "Histogram" , let [ i ] , ".png" , sep = "" )
	png ( filename = name , bg = "white" , width =1280 , height =720)
	par ( ps =18)
	tit = paste ( "Histogram for x with l = " , theta_bet[ i ] , ", b = ", theta_bet [ i ] , " and N = 10000" , sep = "" )
	hist ( x_vec , breaks =49 , col = "black" , border = "white" , xlab = "x" , main= tit )
	box()
	dev.off ()
	x_vec = x_vec [1:100]
	#x_vec = modified_weibull ( theta_bet [ i ] , theta_bet [ i ] , 100)
	l_b = theta_beta ( theta_bet [ i ] , theta_bet [ i ] , x_vec , exp_theta )
	name = paste ( "Plot" , let[ i ] , "1" , ".png" , sep = "" )
    png ( filename = name , bg = "white" , width =1280 , height =720)
	par ( ps =18)
	tit = paste ( "Plot of theta vs i with exponential distribution’s l= " , exp_theta , sep = "" )
	plot ( l_b[[1]] , type = "l" , ylab = "theta" , xlab = "i" , main = tit )
	box()
	dev.off()
	name = paste ( "Plot" , let[i] , "2" , ".png" , sep = "" )
	png ( filename = name , bg = "white" , width =1280 , height =720)
	par ( ps =18)
	tit = paste ( "Plot of beta vs i with exponential distribution’s l =" , exp_theta , sep = "" )
	plot ( l_b[[2]] , type = "l" , ylab = "beta" , xlab = "i" , main = tit )
	box()
	dev.off()
}
}

