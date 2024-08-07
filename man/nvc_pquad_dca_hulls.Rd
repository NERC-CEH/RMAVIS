% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{nvc_pquad_dca_hulls}
\alias{nvc_pquad_dca_hulls}
\title{Two dimensional hulls for each NVC communities pseudo-quadrats}
\format{
A data frame with 25973 rows and 7 columns, the definitions of which are:
\describe{
\item{NVC}{The NVC community code.}
\item{Quadrat}{The code of the pseudo-quadrat which forms part of the perimeter of the hull drawn from all pseudo-quadrats in that given NVC community.}
\item{dcaAxes}{The combination of DCA axes from representing the 2d hull.}
\item{DCA1}{The DCA scores for axis 1.}
\item{DCA2}{The DCA scores for axis 2.}
\item{DCA3}{The DCA scores for axis 3.}
\item{DCA4}{The DCA scores for axis 4.}
}
}
\usage{
nvc_pquad_dca_hulls
}
\description{
A data frame containing the two dimensional hulls for each NVC communities
pseudo-quadrats DCA scores taken from \code{RMAVIS::nvc_pquad_dca}
for three combinations of axes: DCA1 x DCA2, DCA1 x DCA3, and DCA2 x DCA3.
}
\details{
\code{nvc_pquad_dca_hulls}
}
\keyword{datasets}
