#' inverse arcsine square root (inv.asr) recovers asr'ed proportions in (0, 1)
#' 
#' the inverse (asr) is provided for completeness; reconstruction error is 
#' typically in the range of 1e-3 to 1e-9 where err = abs(x - asr(inv.asr(x)))
#' assuming that 0 <= x <= 1.57079 or thereabouts (i.e., asr(0) <= x <= asr(1)).
#' 
#' @param   x   a nonnegative transformed proportion
#'
#' @return      the original proportion p, IFF x = asr(p) and 0 <= p <= 1
#' 
#' @examples
#' x0 <- c(0, 0.032, 0.1, 0.32, 0.79, 1.25, 1.47, 1.54, 1.57079)
#' p <- inv.asr(x)
#' max(abs(x0 - asr(p))) 
#' 
#' @seealso     asr
#' 
#' @export
inv.asr <- function(p) sin(p)**2
