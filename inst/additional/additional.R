NULL
#' 
#' The game of dimensionless shape of soil water retantion curve in the unsaturated zones: educational purposes or just to have an idea!!
#' 
#' @title The shape of Soil Water Retention Curve
#' @param p dimensionless suction head 
#' @param n n exponent of Van Genucten formula (see \code{Details})
#' @param m m exponent of Van Genucten formula (see \code{Details})
#' @param lamda m exponent of Brooks-Corey formula (see \code{Details})
#' @param type string vector indicating the type of Soil Water Retanction Curve Formula. Default is \code{c("VG","BC","VE")}. 
#' @param npoints number of points for the dimensionless Soil Water Retention Curve. It is ignored if the vector \code{p} is assigned.
#' @param data.frame logical value. If \code{TRUE} it returns a \code{data.fram} instead of a \code{list}. 
#' Default is \code{FALSE}. It works if \code{type} has length more than 1.
#' @export 
#'  @details The parameters \code{n,m,lambda} are expinents of the rescaled dimansionless Soil Water Retention Curve which can be reperented by the following formulae: 
#' 
#'  \eqn{s=(1+p^n)^{-m}} (Van Genuchten)
#' 
#' \eqn{s=p^{-lambda}} for \eqn{p>1} and \eqn{s=1} for \eqn{p<=1}  (Brooks and Coorey)
#' 
#' The argument \code{type} establishes the choice of the formula: @aliases 
#' 
#' 
#' \code{VG}: Van Genuchten Formula
#' \code{BC}: Brooks and Coorey Formula 
#' \code{VE}: Van Genuchten Forumla with \code{m} and {n} according to Veerecken PedoTransfert Function (i.e. \code{m=1} and \code{n=lambda} . 
#' 
#' By Default the following relationships among \code{n,m,lambda} are used: 
#' 
#' \eqn{n=lambda+1}
#' 
#' \eqn{m=1-1/n}
#' 
#' \eqn{lambda=m*n}
#' 
#' In case \code{type} is a vector longer than 1, the function return a list with sevaral soil saturation values. 
#' 
#' If \code{type} is equal to \code{HEAD} the function returns  the vector of sution head \code{p}, otherwise it returns a vactor of \code{NAs}
#' 
#' @examples 
#' 
#' library(soilwater)
#' library(ggplot2)
#' 
#' swc <- swc_shape(lambda=0.5,data.frame=TRUE) 
#' 
#' 
#' plot(swc)
#' 
#' ggplot(swc, aes(x =VG, y=HEAD))+geom_line()
#' ###autoplot(swc)
#' 
#' 
#' ggplot(swc, aes(x =BC, y=log(HEAD)))+geom_line()+geom_line(aes(x =VG, y=log(HEAD)))+geom_line(aes(x =VE, y=log(HEAD)))
##' > ggplot(swc, aes(x =VE, y=HEAD))+geom_line()
##' > ggplot(swc, aes(x =VE-VG, y=HEAD))+geom_line()
##' > ggplot(swc, aes(x =VE-VG, y=VG))+geom_line()
##' > ggplot(swc, aes(x =VE-VG, y=VG))+geom_point()

#' @references 
#' 
#' # TO DO 
#' 
#' @author Emanuele Cordano
#' 
 
### OLD DEFALTp=log(seq(0,1,length.out=npoints+1)[-1])[npoints:1]

 swc_shape <- function(p="Default",n=lambda+1,m=1-1/n,lambda=n*m,type=c("VG","BC","VE","HEAD"),npoints=1000,data.frame=FALSE) {
	 
	 out <- NULL
	
	 print(as.numeric(n))
	 if (is.null(n)) n <- NA
	 if (is.null(m)) m <- NA
	 if (is.null(lambda)) lambda <- NA
	 
	
	 
	 
	 
	 cond <- c(is.na(n),is.na(m),is.na(lambda))
	 
	 cond <- length(which(cond))>1
	 
	 
	 if (cond ) {
		 
		 # error message at least two parameters mst be assigned not TRUE!!!
		
		
	} else if (is.na(n)) {
		 
		 n <- lambda/m
		 
	 } else if (is.na(m)) {
		 
		 m <- lambda/n
	 } else if (is.na(lambda)) {
		 
		 lambda <- n*m
	 }
	
	 if (p[1]=="Default") { 
	      s <- seq(0,1,length.out=npoints+1)[-1]
		  
		####  s=(1+p^n)^(-m)
		
		  p <- (s^(-1/m)-1)^(1/n)
		  
	 }
	 p[p<0] <- -p[p<0]
	 if (length(type)==1) {
		
		 
		 if ((type[1]=="VG") | (type[1]=="VE")) {
			
			 if (type[1]=="VE") {
				 
				 m <- 1
				 n <- lambda/m
			 }
			 
			s <- (1+p^n)^(-m) 
		 } else if (type[1]=="BC") {
			 
			s <- p^(-lambda)
			s[s>1] <- 1
			
		 } else if (type[1]=="HEAD"){
			 
			 s <- p  # it prints the pressure head
			 
		 } else {
			 
			 s <- p*NA
		 }
		 
		 out <- s
		 
	 } else {
		 type <- c(type[type=="HEAD"],type[type!="HEAD"])
		 out <- lapply(X=type,FUN=function(x,n,m,lambda,p){
					 
					 outs <- swc_shape(p=p,n=n,m=m,lambda=lambda,type=x)
					 
					 return(outs)
				 },n=n,m=m,lambda=lambda,p=p) 
		 
		 names(out) <- type
		 
		 if (data.frame) {
			 dim <- c(length(out[[1]]),length(out))
			 names <- names(out)
			 out <- unlist(out)
			 out <- as.data.frame(array(out,dim=dim))
			 names(out) <- names
			 
		 }
		 
	 }
#	 out <- lapply
	 
	 
	
	 
	 
	 return(out)
	 
 }
 
 
