#' @title Determining cover balls
#'
#' @description Determining cover balls
#'
#' @param x_main Target class samples.
#' @param x_other Non-target class samples.
#' @param proportion proportion of covered samples. A real number between \eqn{(0,1]}.
#' 1 by default. Smaller numbers results in less dominant samples.
#' @param p_of roportion to increase cover radius. A real number between
#'  \eqn{(0,\infty)}. Default is 0. Higher values tolerate other classes more.
#'
#' @details
#' To be used in \code{DatRel}.
#'
#' @return a list object with following:
#'  \item{i_dominant}{dominant sample indexes}
#'  \item{dist_main2other}{distance matrix of target class samples to non-target
#' class samples}
#'  \item{dist_main2main}{distance matrix of target class samples to target
#' class samples}
#'
#' @author Fatih Saglam, saglamf89@gmail.com
#'
#' @importFrom RANN nn2
#' @importFrom Rfast Dist
#' @import rcccd
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
