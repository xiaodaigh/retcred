#' Population Stability Index (PSI)
#' 
#' @param base.segments The base segment's distribution for comparison. It needs
#'   to be a vector of positive numbers in the open interval (0,1) that sums to 
#'   exactly 1.
#' @param comparison.segments The distribution of the segments that is to be 
#'   compared to the base.segments. It needs to be a vector of positive numbers 
#'   in the open interval (0,1) that sums to exactly 1 and also be of the same
#'   length as base.segments
#' @export
#' @examples
#' x <- runif(10) 
#' base.seg <- x/sum(x)
#' # this should return a PSI of zero since the segments' distributions did not change
#' psi(base.seg, base.seg) 
#' 

psi <- function(base.segments, comparison.segments) {
  stopifnot(sum(base.segments) == 1)
  stopifnot(all(base.segments > 0))
  stopifnot(all(base.segments < 1))
  stopifnot(sum(comparison.segments) == 1)
  stopifnot(all(comparison.segments > 0))
  stopifnot(all(comparison.segments < 1))
  stopifnot(length(base.segments) == length(comparison.segments))
  
  sum((base.segments - comparison.segments)*log(base.segments / comparison.segments))
}