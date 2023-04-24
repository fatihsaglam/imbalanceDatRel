#' @title Oversampling and Data Relocation for Resampled Data
#'
#' @description \code{oversampleDatRel} first oversamples using selected method
#' then relocates resampled data using Pure and Proper Class Cover Catch Digraph.
#'
#' @param x feature matrix or dataframe.
#' @param y class factor variable.
#' @param method asd
#' @param proportion proportion of covered samples. A real number between \eqn{(0,1]}.
#' 1 by default. Smaller numbers results in less dominant samples.
#' @param p_of asd
#' @param class_pos asd
#' @param ... asd
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
#' @rdname oversampleDatRel
#' @export

oversampleDatRel <- function(x, y, method = "SMOTE", proportion = 1, p_of = 0, class_pos = NULL, ...) {
  f_oversample <- get(method)

  m <- f_oversample(x = x, y = y, ...)
  m_DatRel <- DatRel(x = x, y = y, x_syn = m$x_syn, proportion = proportion, p_of = p_of, class_pos = class_pos)

  return(m_DatRel)
}
