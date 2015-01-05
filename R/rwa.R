#' @title Risk-weighted Asset (RWA) and the required capital, k, calculation for
#'   retail portfolios under the Basel II framework
#' @name rwa
#' @return The asset correlation correpoding to the asset class as specified in
#'    Basel II
#' @param ead The exposure at default in dollar amount. If not specified the 
#'   function returns the RWA\%
#' @param reg.mult Regulator specified multiplier to the RWA
#' @param ... arguments that feed into the k.capital function
#' @examples
#' rwa(pd = 0.01, lgd = 0.2) 
#' 
#' @export
rwa <- function(..., ead = 1, reg.mult = 1) {
  k <- k.capital(...)
  12.5 * k * reg.mult * ead
}

#' @rdname rwa
#' @export
asset.corr <- function(asset.class = c("Mortgage", "QRRE", "Other Retail"), pd = NULL) {
  asset.class <- match.arg(asset.class)
    
  if(asset.class == "Mortgage") {
    r <- 0.15
  } else if (asset.class == "QRRE") {
    r <- 0.04      
  } else {
    if(is.null(pd)) stop("You need to specify PD values for the Other Retail Asset class")
    stopifnot(all(pd >0 & pd <1))
    r <- 0.03*(1-exp(-35*pd))/(1-exp(-35))+0.16*(1-(1-exp(-35*pd)))/(1-exp(-35))     
  }
  return(r)
}

#' @param pd The probability of default (PD) of the record
#' @param lgd The loss given default (LGD) of the record
#' @param asset.class The assest class.
#' @param r The assest correlation. Usually not required as it is specifeid under Basel II bassed on asset class
#' @return The capital requirement as specifed under Basel II A-IRB
#' @rdname rwa
#' @export
#' 
k.capital <- function(pd, lgd, asset.class = c("Mortgage", "QRRE", "Other Retail"), r = NULL) {  
  # terminate if pd, lgd, or r contain invalid values
  stopifnot(all(lgd >= 0 & lgd <=1))
  stopifnot(all(pd >= 0 & pd <=1))
  stopifnot(is.null(r) || all(r > 0 & r < 1))
  
  # if asset correlation is not directly specified then derive them from the asset.corr function based on asset.class
  if(is.null(r)) {
    r <- asset.corr(asset.class, pd)  
  }
  
  lgd*(pnorm(sqrt(1/(1-r))*qnorm(0.999) + sqrt(r/(1-r))*qnorm(pd)) - pd)
}