# This function generates from the Modified Weibull Distribution
# using the inverse transform method .
modified_weibull <- function (l , b , N )
{
	set.seed(1221);
	u = runif ( N );
	ret = (1/l)*((-log(1-u))^(1/b));
		return ( ret );
}
# Markov Chain method .
theta_beta <- function( l0 , b0 , x_vec , l_exp) {
	l_t = l0
	b_t = b0
	ret_l <- vector()
	ret_b <- vector()
	ret_l[1] = l0
	ret_b[1] = b0
	N = length ( x_vec )
	e_dist_l = rexp (N , rate = l_exp )
	e_dist_b = rexp (N , rate = l_exp )
	u_l = runif ( N )
	u_b = runif ( N )
	for ( i in 1:( N -1) ) {
		tmp = (p1 (e_dist_l[i] , ret_b[i] , x_vec ) / p1 ( ret_l[ i ] , ret_b[ i ] , x_vec) ) * ( dist_q ( ret_l[ i ] , l_exp ) / dist_q ( e_dist_l[i] , l_exp ) )
				if ( is.nan( tmp ) )
					tmp = 1
				alpha = min (1 , tmp )
				if ( u_l [ i ] <= alpha )
					ret_l [ i +1] = e_dist_l [ i ]
				else
					ret_l [ i +1] = ret_l [ i ]
		tmp = (p2 ( ret_l[ i +1] , e_dist_b[ i ] , x_vec ) / p2 ( ret_l[ i +1] , ret_b[i] , x_vec) )*( dist_q (ret_b[ i ] , l_exp ) / dist_q ( e_dist_b[ i ] , l_exp ) )
				if ( is.nan( tmp ) )
					tmp = 1
				alpha = min (1 , tmp )
				if ( u_b[ i ] <= alpha )
					ret_b[ i +1] = e_dist_b[ i ]
				else
					ret_b[ i +1] = ret_b[ i ]
		}
	return ( list ( ret_l , ret_b ) )
}

# Returns the value of the target distribution function p1 .
p1 <- function (l , b , x_vec ) {
	N = length ( x_vec )					
	s_exp = sum ( exp ( x_vec^b ) )				
	tmp = ( l^(b*N) ) * exp (-(l^b) * s_exp )
	return ( tmp )
}
# Returns the value of the exponential distribution q .
dist_q <- function (v , l_exp ) {
	return ( l_exp * exp ( - l_exp * v ) )
	}
# Returns the value of the target distribution function p2 .
p2 <- function (l , b , x_vec ) {
	N = length ( x_vec )
	s_exp = sum ( exp ( x_vec^b ) )
	tmp = (b^N)*( l^(b*N) )* ( prod ( x_vec )^(b -1) ) * exp (-(l^b) * s_exp )
	return ( tmp )
}
