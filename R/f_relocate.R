#' @title Relocation function
#'
#' @description Relocation function
#'
#' @param x_pos_dominant positive class dominant sample matrix or dataframe
#' @param x_syn synthetically generated positive class sample matrix or dataframe
#' @param radii_pos_dominant positive class dominant sample radii
#' @param p_of proportion to increase cover radius. A real number between
#'  \eqn{(0,\infty)}. Default is 0. Higher values tolerate other classes more.
#'
#' @return relocated data matrix
#'
#' @author Fatih Saglam, saglamf89@gmail.com
#'
#' @importFrom Rfast dista
#'
#' @examples
#' rnorm(1)
#'
#' @rdname f_relocate
#' @export

f_relocate <- function(
    x_pos_dominant,
    x_syn,
    radii_pos_dominant,
    p_of = 0) {

  n_dominant <- nrow(x_pos_dominant)
  dist_dom2syn <- dista(xnew = x_pos_dominant, x = x_syn)
  dist_dom2syn_prop <- dist_dom2syn/radii_pos_dominant
  i_dom_belongto <- apply(dist_dom2syn_prop, 2, which.min)

  x_syn_relocated <- x_syn

  for (i in 1:n_dominant) {
    i_syn_overflowed <- which(dist_dom2syn_prop[i,] > 1 + p_of & i_dom_belongto == i)
    if(length(i_syn_overflowed) == 0) {
      next
    }
    diff_required <- (x_syn[i_syn_overflowed,] -
                        x_pos_dominant[rep(i, length(i_syn_overflowed)),, drop = FALSE])/
      dist_dom2syn_prop[i,i_syn_overflowed]
    x_syn_relocated[i_syn_overflowed,] <- x_pos_dominant[rep(i, length(i_syn_overflowed)),, drop = FALSE] +
      diff_required
  }

  return(x_syn_relocated)
}



