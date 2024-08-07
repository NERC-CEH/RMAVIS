% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{nvc_pquad_dca_centroids}
\alias{nvc_pquad_dca_centroids}
\title{The centroids of each NVC communities pseudo-quadrats}
\format{
A data frame with 886 rows and 5 columns, the definitions of which are:
\describe{
\item{NVC}{The NVC community code.}
\item{DCA1}{The DCA scores for axis 1.}
\item{DCA2}{The DCA scores for axis 2.}
\item{DCA3}{The DCA scores for axis 3.}
\item{DCA4}{The DCA scores for axis 4.}
}
}
\usage{
nvc_pquad_dca_centroids
}
\description{
A data frame containing the centroids of each NVC communities pseudo-quadrats
in an ordination space constructed by performing a Detrended Correspondence
Analysis on all pseudo-quadrats present in \code{RMAVIS::nvc_pquads_wide},
and returned in \code{RMAVIS::nvc_pquad_dca}.
}
\details{
\code{nvc_pquad_dca_centroids}
}
\keyword{datasets}
