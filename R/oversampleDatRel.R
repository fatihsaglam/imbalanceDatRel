#' @title Oversampling and Data Relocation for Resampled Data
#'
#' @description \code{oversampleDatRel} first oversamples using selected method
#' then relocates resampled data using Pure and Proper Class Cover Catch Digraph.
#'
#' @param x feature matrix or dataframe.
#' @param y class factor variable.
#' @param method oversampling method. Default is "SMOTE". Available methods are: \cr
#'      "ADASYN": Adaptive Synthetic Sampling \cr
#'      "ROS": Random Oversampling \cr
#'      "ROSE": Randomly Over Sampling Examples \cr
#'      "RSLSMOTE": Relocating safe-level SMOTE with minority outcast handling \cr
#'      "RUS": Random Undersampling \cr
#'      "SLSSMOTE": Safe-level Synthetic Minority Oversampling Technique \cr
#'      "SMOTE": Synthetic Minority Oversampling Technique \cr
#'      "SMOTEWB": SMOTE with boosting
#' @param proportion proportion of covered samples. A real number between \eqn{(0,1]}.
#' 1 by default. Smaller numbers results in less dominant samples.
#' @param p_of proportion to increase cover radius. A real number between
#'  \eqn{(0,\infty)}. Default is 0. Higher values tolerate other classes more.
#' @param class_pos Class name of synthetic data. Default is NULL. If NULL,
#' positive class is minority class.
#' @param ... arguments to be used in specified method.
#'
#' @details
#' Oversampling using \code{DatRel}. Available oversampling methods are from
#' \code{SMOTEWB} package. "ROSE" generates samples from all classes. DatRel
#' relocates all class samples.
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
#' @examples
#'
#' library(SMOTEWB)
#' library(rcccd)
#'
#' set.seed(10)
#' # adding data
#' x <- rbind(matrix(rnorm(2000, 3, 1), ncol = 2, nrow = 1000),
#'            matrix(rnorm(60, 6, 1), ncol = 2, nrow = 30))
#' y <- as.factor(c(rep("negative", 1000), rep("positive", 30)))
#'
#' # adding noise
#' x[1001,] <- c(3,3)
#' x[1002,] <- c(2,2)
#' x[1003,] <- c(4,4)
#'
#' # resampling
#' m_SMOTE <- SMOTE(x = x, y = y, k = 3)
#'
#' # resampled data
#' plot(x, col = y, main = "SMOTE")
#' points(m_SMOTE$x_syn, col = "green")
#'
#' m_DatRel <- oversampleDatRel(x = x, y = y, method = "SMOTE")
#'
#' # resampled data after relocation
#' plot(x, col = y, main = "SMOTE + DatRel")
#' points(m_DatRel$x_syn, col = "green")
#'
#' @import SMOTEWB
#'
#' @rdname oversampleDatRel
#' @export

oversampleDatRel <- function(x, y, method = "SMOTE", proportion = 1, p_of = 0, class_pos = NULL, ...) {
  f_oversample <- get(method)

  m <- f_oversample(x = x, y = y, ...)

  if (method == "ROSE") {
    class_names <- levels(y)
    k_class <- length(class_names)
    p <- ncol(x)

    x_new <- matrix(NA, nrow = 0, ncol = p)
    y_new <- c()
    i_dominant <- list()
    radii_pos_dominant <- list()
    x_pos_dominant <- list()

    for (i in 1:k_class) {
      x_main <- m$x_new[m$y_new == class_names[i],, drop = FALSE]
      m_DatRel <- DatRel(x = x, y = y, x_syn = x_main, proportion = proportion, p_of = p_of, class_pos = as.character(class_names[i]))

      x_new <- rbind(x_new, m_DatRel$x_syn)
      y_new <- c(y_new, rep(as.character(class_names[i]), nrow(x_main)))
      i_dominant[[i]] <- m_DatRel$i_dominant
      radii_pos_dominant[[i]] <- m_DatRel$radii_pos_dominant
      x_pos_dominant[[i]] <- m_DatRel$x_pos_dominant
    }

    y_new <- factor(y_new, levels = class_names, labels = class_names)

    return(list(
      x_new = x_new,
      y_new = y_new,
      x_syn = NULL,
      i_dominant = i_dominant,
      x_pos_dominant = x_pos_dominant,
      radii_pos_dominant = radii_pos_dominant
    ))

  } else {

    m_DatRel <- DatRel(x = x, y = y, x_syn = m$x_syn, proportion = proportion, p_of = p_of, class_pos = class_pos)
    return(m_DatRel)
  }
}
