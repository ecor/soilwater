NULL
#' 
#' The water table recharge: the response unit  
#'
#' @param t time coordinate
#' @param d depth of unsaturated zone along the slope-normal direction
#' @param H soil depth
#' @param D soil water diffusivity
#' @param m maximum limit of summary truncation. Default is 100.
#' 
#' 
#' @note This function is experimental and under development by Emanuele Cordano. 
#' @export
#' 
#' @examples 
#' 
#' library(soilwater)
#' 
#' 
#' t <- seq(0,2,by=0.001)
#' d <- c(1,0.75,0.5,0.25)
#' val1 <- unitResponse(t, d = d[1], D = 1, H = 1, m = 500)
#' 
#' val2 <- unitResponse(t, d = d[2], D = 1, H = 1, m = 500)
#' 
#' val3 <- unitResponse(t, d = d[3], D = 1, H = 1, m = 500)
#' 
#' val4 <- unitResponse(t, d = d[4], D = 1, H = 1, m = 500)
#' 
#' 
#' 



unitResponse <- function(t,d=1,D=1,H=d,m=100) {
	
	sum <- 0 
	
	d <-d/H 
	
	tscale <- H^2/D 
	
	t <- t/tscale
	vect <- -m:m
	
	for (m in vect) {
		
		val <- (pi*t)^(-0.5)*(-1/(2*t)+(2*m*d+d)^2/(2*t)^2)*exp(-(2*m*d+d)^2/(4*t))
		sum <- sum+val
	}
	
	out <- sum*tscale 
	
	#
    # INPUT is DELTA rescaled with hydraulic conductivy
	# OUTPUT is rescaled with (d/tscale)
    #
	return(out)
	
	
}