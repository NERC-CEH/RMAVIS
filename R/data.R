#' Example vegetation survey data
#'
#' A list of data frames containing four example vegetation survey datasets.
#'
#' \code{example_data} 
#'
#' @format A list with `r length(RMAVIS::example_data)` entries:
#' \describe{
#'   \item{Parsonage Down}{}
#'   \item{Leith Hill Place Wood}{}
#'   \item{Whitwell Common}{}
#'   \item{Newborough Warren}{}
#' }
"example_data"

#' Accepted Taxa
#'
#' The taxon names accepted by RMAVIS. Equivalent to the unique species in the `UKVegTB::taxonomic_backbone` 'full_name' column
#' with the addition of taxa with strata suffixes as present in `RMAVIS::nvc_taxa_lookup`.
#'
#' \code{accepted_taxa} 
#'
#' @format A data frame with `r nrow(RMAVIS::accepted_taxa)` rows and `r ncol(RMAVIS::accepted_taxa)` columns, the definitions of which are:
#' \describe{
#'   \item{TVK}{The UKSI taxon version keys for the accepted taxa.}
#'   \item{taxon_name}{The taxon names of the accepted taxa.}
#' }
"accepted_taxa"