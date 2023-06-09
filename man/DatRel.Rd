% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DatRel.R
\name{DatRel}
\alias{DatRel}
\title{Data Relocation for Resampled Data using Pure and Proper Class Cover Catch Digraph}
\usage{
DatRel(x, y, x_syn, proportion = 1, p_of = 0, class_pos = NULL)
}
\arguments{
\item{x}{feature matrix or dataframe.}

\item{y}{class factor variable.}

\item{x_syn}{synthetic data generated by an oversampling method.}

\item{proportion}{proportion of covered samples. A real number between \eqn{(0,1]}.
1 by default. Algorithm stops when desired percent of coverage achieved in each class.
Smaller numbers results in less dominant samples.}

\item{p_of}{proportion to increase cover radius. A real number between
\eqn{(0,\infty)}. Default is 0. Higher values tolerate other classes more.}

\item{class_pos}{Class name of synthetic data. Default is NULL. If NULL,
positive class is minority class.}
}
\value{
an list object which includes:
 \item{x_new}{Oversampled and relocated feature matrix}
 \item{y_new}{Oversampled class variable}
 \item{x_syn}{Generated and relocated sample matrix}
 \item{i_dominant}{Indexes of dominant samples}
 \item{x_pos_dominant}{Dominant samples for positive class}
 \item{radii_pos_dominant}{Positive class cover percentage}
}
\description{
\code{DatRel} relocates resampled data using Pure and Proper Class Cover Catch Digraph
}
\details{
Calculates cover areas using pure and proper class cover catch digraphs (PCCCD) for
original dataset. Any sample outside of cover area is relocated towards a
specific dominant point. Determination of dominant point to move towards is
based on distance based on radii of PCCCD balls. \code{p_of} is to increase
obtained radii to be more tolerant to noise. \code{prooportion} argument is
cover percentage for PCCCD to stop when desired percentage is covered for
each class. PCCCD models are determined using \code{rcccd} package.
\code{class_pos} argument is used to specify oversampled class.
}
\examples{

library(SMOTEWB)
library(rcccd)

set.seed(10)
# adding data
x <- rbind(matrix(rnorm(2000, 3, 1), ncol = 2, nrow = 1000),
           matrix(rnorm(60, 6, 1), ncol = 2, nrow = 30))
y <- as.factor(c(rep("negative", 1000), rep("positive", 30)))

# adding noise
x[1001,] <- c(3,3)
x[1002,] <- c(2,2)
x[1003,] <- c(4,4)

# resampling
m_SMOTE <- SMOTE(x = x, y = y, k = 3)

# relocation of resampled data
m_DatRel <- DatRel(x = x, y = y, x_syn = m_SMOTE$x_syn)

# resampled data
plot(x, col = y, main = "SMOTE")
points(m_SMOTE$x_syn, col = "green")

# resampled data after relocation
plot(x, col = y, main = "SMOTE + DatRel")
points(m_DatRel$x_syn, col = "green")

}
\author{
Fatih Saglam, saglamf89@gmail.com
}
