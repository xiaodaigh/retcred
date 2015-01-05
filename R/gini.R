#   boiler plate to pass CRAN so that R CMD Check --as-cran does not produce
#   no visible binding for global variable notes
globalVariables(c("sum.bad","sum.all","cum.sum.bad","cum.sum","cum.sum.bad.p",'cum.sum.p',"area"))

#' GINI coefficient for binary targets
#' 
#' @param score A vector of numbers that is used to predict the target.
#' @param target A binary vector of targets. The first (alphanumerically) sorted level is used as the bad indicator.
#' @param bad The bad target. It must be one of the two values in target.
#' @param return.data Return the data and the intermediate computation results.
#' @param return.data.table Return the data as data.table. By default this is FALSE because data.frame is more common.
#' @export
#' @importFrom data.table data.table
#' @importFrom magrittr %<>%
#' @import dplyr
#' @examples
#' # this should return a gini close to 0 since the scores are assigned randomly
#' gini(rnorm(100), rnorm(100) < 0) 
#' 
gini <- function(score, target, bad = NULL, return.data = T, return.data.table = F) {  
  browser()
  # determine the bad if not already specified
  if(is.null(bad))
    bad <- sort(unique(target))[1]  

  # use data.table to compute the score and target table
  dt <- data.table(score,target, key = "score")
  
  # computate the cumulative bads# and cumulative totals# 
  dt <- dt %>% group_by(score) %>%     
    summarise(sum.bad = sum(target == bad), sum.all = n()) %>% 
    mutate(cum.sum.bad = cumsum(sum.bad), cum.sum = cumsum(sum.all))
    
  # compute the total bads and totals # of accounts
  sums <- dt[nrow(dt)] %>% select(cum.sum.bad,cum.sum)
  
  # compute the cumulative proportions of bads% and totals%  
  dt %<>% mutate(cum.sum.bad.p = cum.sum.bad / sums[,cum.sum.bad],
                cum.sum.p = cum.sum / sums[,cum.sum])
  
  # compute the area under the curve using the Trapezoidal rule
  dt %<>% mutate(area = (cum.sum.bad.p + lag(cum.sum.bad.p)) * (cum.sum.p - lag(cum.sum.p))/2 )
 
  # compute the gini
  gini <- (sum(select(dt,area), na.rm = T) - 1/2) * 2
  
  # return the data as specified
  if(!return.data) 
    dt <- NULL
  if(return.data.table)
    list(gini = gini, data = dt)
  else 
    list(gini = gini, data = as.data.frame(dt))
}