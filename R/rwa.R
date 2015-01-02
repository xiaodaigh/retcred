#' Risk-weighted Asset (RWA) and the required capital, k, calculation for retail
#' portfolios under the Basel II framework
#' 
#' @param pd The probability of default (PD) of the record
#' @param lgd The loss given default (LGD) of the record
#' @param asset.class The assest class of the account
#' @param r The assest correlation. Usually not required as it is specifeid under BaselII bassed on asset class
#' @export
#' 
k.capital <- function(pd, lgd, asset.class = c("Mortgage", "QRRE", "Other Retail"), r = NULL) {
  
  asset.class <- match.arg(asset.class)

  if(is.null(r)) {    
    r <- 0.15    
    r <- 0.04    
    r <- 0.03*(1-exp(-35*pd))/(1-exp(-35))+0.16*(1-(1-exp(-35*pd)))/(1-exp(-35))    
  }
  
  ifelse(lgd >=0 & lgd <= 1, lgd*(pnorm(sqrt(1/(1-r))*qnorm(0.999) + sqrt(r/(1-r))*qnorm(pd)) - pd), {warning("LGD should be between 0 and 1");NA})
}


#' @rdname k.capital
#' @param ead The exposure at default in dollar amount. If not specified the 
#'   function returns the RWA\%
#' @param reg.mult Regulator specified multiplier to the RWA
#' @param ... arguments that feed into the k.capital function
#' @examples
#' rwa(pd = 0.01, lgd = 0.2) 
#' 
#' # sometimes the regulator may impose a different correlation to the Basel II
#' # standard. YOu can use r to get the correaltion
#' rwa(pd = 0.01, lgd = 0.2, r = 0.21) 
#' @export
rwa <- function(..., ead = 1, reg.mult = 1) {
  k <- k.capital(...)
  12.5 * k * reg.mult * ead
}