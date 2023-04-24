#' @title Data Relocation for Resampled Data using Pure and Proper Class Cover Catch Digraph
#'
#' @description \code{DatRel} relocates resampled data using Pure and Proper Class Cover Catch Digraph
#'
#' @param x feature matrix or dataframe.
#' @param y class factor variable.
#' @param x_syn asd
#' @param proportion proportion of covered samples. A real number between \eqn{(0,1]}.
#' 1 by default. Smaller numbers results in less dominant samples.
#' @param p_of asd
#' @param class_pos asd
#'
#' @details
#' asd
#'
#' @return an list which includes:
#'  \item{x_new}{dominant sample indexes.}
#'  \item{y_new}{dominant samples from feature matrix, x}
#'  \item{x_syn}{Radiuses of the circle for dominant samples}
#'  \item{i_dominant}{class names}
#'  \item{x_pos_dominant}{number of classes}
#'  \item{radii_pos_dominant}{proportions each class covered}
#'
#' @author Fatih Saglam, saglamf89@gmail.com
#'
#' @references
#' Priebe, C. E., DeVinney, J. G., & Marchette, D. J. (2001). On the distribution
#' of the domination number for random class cover catch digraphs. Statistics &
#' Probability Letters, 55(3), 239-246.
#'
#' Marchette, C. E. P. D. J., & Socolinsky, J. G. D. D. A. (2003).
#'  Classiﬁcation Using Class Cover Catch Digraphs. Journal of Classiﬁcation, 20, 3-23.
#'
#' Manukyan, A., & Ceyhan, E. (2016). Classification of imbalanced data with a
#' geometric digraph family. The Journal of Machine Learning Research, 17(1), 6504-6543.
#'
#' @examples
#'
#' rnorm(1)
#'
#' @rdname DatRel
#' @export

DatRel <- function(x, y, x_syn, proportion = 1, p_of = 0, class_pos = NULL) {
  var_names <- colnames(x)
  x <- as.matrix(x)
  # p <- ncol(x)

  class_names <- as.character(unique(y))
  if (is.null(class_pos)) {
    class_pos <- names(which.min(table(y)))
  }
  class_neg <- as.character(class_names[class_names != class_pos])

  x_pos <- x[y == class_pos,,drop = FALSE]
  x_neg <- x[y == class_neg,,drop = FALSE]

  n_pos <- nrow(x_pos)
  n_neg <- nrow(x_neg)
  n_syn <- nrow(x_syn)

  x <- rbind(x_pos, x_neg)
  y <- c(y[y == class_pos], y[y == class_neg])

  m_dominate <- f_dominate(x_main = x_pos, x_other = x_neg, proportion = proportion, p_of = p_of)
  dist_pos2neg <- m_dominate$dist_main2other

  i_dominant <- m_dominate$i_dominant
  x_pos_dominant <- x_pos[m_dominate$i_dominant,,drop = FALSE]
  radii_pos_dominant <- dist_pos2neg[m_dominate$i_dominant,]

  x_syn <- f_relocate(x_pos_dominant = x_pos_dominant,
                      x_syn = x_syn,
                      radii_pos_dominant = radii_pos_dominant)
  x_new <- rbind(
    x_syn,
    x_pos,
    x_neg
  )
  y_new <- c(
    rep(class_pos, n_syn + n_pos),
    rep(class_neg, n_neg)
  )
  y_new <- factor(y_new, levels = levels(y), labels = levels(y))
  colnames(x_new) <- var_names

  return(list(
    x_new = x_new,
    y_new = y_new,
    x_syn = x_new[1:n_syn,, drop = FALSE],
    i_dominant = i_dominant,
    x_pos_dominant = x_pos_dominant,
    radii_pos_dominant = radii_pos_dominant
  ))
}
