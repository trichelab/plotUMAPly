#' arcsine square root (asr) is a variance stabilizing transform for p in 0 to 1
#' 
#' the inverse (inv.asr) is provided for completeness; reconstruction error is 
#' typically in the range of 1e-9 to 1e-15 where err = abs(p - inv.asr(asr(p)))
#' 
#' @param   p   the proportion(s) to transform (0 <= p <= 1)
#'
#' @return      a nonnegative transformation of the proportion(s) 
#' 
#' @examples
#' p0 <- c(0.001, 0.01, 0.1, 0.5, 0.9, 0.99, 0.999)
#' x <- asr(p0)
#' p <- inv.asr(x)
#' max(abs(p0 - p)) 
#' 
#' @seealso     inv.asr
#' 
#' @export
asr <- function(p) asin(sqrt(p))
