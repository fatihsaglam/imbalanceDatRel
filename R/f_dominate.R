#' @title Determining cover balls
#'
#' @description Determining cover balls
#'
#' @param x_main asd
#' @param x_other asd
#' @param proportion asd
#' @param p_of asd
#'
#' @details
#' asd
#'
#' @return asd
#'  \item{results}{asd}
#'
#' @author Fatih Saglam, saglamf89@gmail.com
#'
#' @importFrom RANN nn2
#' @importFrom Rfast Dist
#' @import rcccd
#'
#' @examples
#' rnorm(1)
#'
#' @rdname f_dominate
#' @export

f_dominate <- function(x_main, x_other, proportion = 1, p_of = 0) {
  n_main <- nrow(x_main)
  dist_main2other <- nn2(data = x_other, query = x_main, k = 1)$nn.dist*(1 + p_of)
  dist_main2main <- Dist(x = x_main)
  M <- dist_main2main < c(dist_main2other)
  M <- matrix(as.numeric(M), n_main)

  cover <- rep(0, n_main)
  thresh <- n_main*proportion

  m_dominant <- rcccd:::f_cover_pcccd(
    cover = cover,
    thresh = thresh,
    dist_main2main = dist_main2main,
    dist_main2other = dist_main2other,
    M = M)

  results <- list(
    i_dominant = m_dominant$i_dominant,
    dist_main2other = dist_main2other,
    dist_main2main = dist_main2main
  )
  return(results)
}
