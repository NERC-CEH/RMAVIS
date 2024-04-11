#' The taxa accepted by RMAVIS
#' 
#' A three-column data frame containing the accepted taxon names, associated
#' BSBI taxonID (if a vascular plant), and associated TVK.
#'
#' \code{acceptedSpecies} 
#'
#' @format A data frame with `r nrow(RMAVIS::acceptedSpecies)` rows and `r ncol(RMAVIS::acceptedSpecies)` columns, the definitions of which are:
#' \describe{
#'   \item{Accepted_Species}{The taxon names accepted by RMAVIS.}
#'   \item{BSBI_taxonId}{The BSBI taxonId associated with the accepted taxon, vascular plants only.}
#'   \item{TVK}{The United Kingdom Species Inventory (UKSI) Taxon Version Key (TVK) associated with the accepted taxon.}
#' }
"acceptedSpecies"


#' Habitat Classifications
#'
#' A vector containing the names of the habitat classification for which there is correspondence data in `RMAVIS::habCor_data`
#'
#' \code{habCor_classifications} 
#'
"habCor_classifications"   

#' Habitat correspondence data
#'
#' A dataset containing habitat correspondences between NVC communities and the
#' following classifications.
#' -   UKHab - Level 2
#' -   UKHab - Level 3
#' -   UKHab - Level 4
#' -   UKHab - Level 5
#' -   Biodiversity Action Plan Priority Habitats
#' -   Phase 1 Habitat Classification
#' -   Biodiversity Action Plan Broad Habitats
#' -   EUNIS Classification
#' -   Vegetation communities of British lakes
#' -   EU Habitats Directive Annex 1
#' -   NPMS Broad
#' -   NPMS Fine
#'
#' \code{habCor_data} 
#'
#' @format A data frame with `r nrow(RMAVIS::habCor_data)` rows and `r ncol(RMAVIS::habCor_data)` columns, the definitions of which are:
#' \describe{
#'   \item{NVC.Code}{The NVC community code.}
#'   \item{Relationship}{The relationship between the NVC community and the habitat present in the 'label' column.}
#'   \item{Code}{The code associated with the habitat present in the 'label' column.}
#'   \item{Label}{The corresponding habitat.}
#'   \item{Classification}{The habitat classification system.}
#' }
"habCor_data"               

#' Concordance information for taxon names present in RMAVIS
#'
#' A dataset containing concordance information to facilitate the matching of
#' species present in the original NVC dataset (and in the {assignNVC} R package)
#' to additional taxonomic backbones.
#'
#' \code{concordance} 
#'
#' @format A data frame with `r nrow(RMAVIS::concordance)` rows and `r ncol(RMAVIS::concordance)` columns, the definitions of which are:
#' \describe{
#'   \item{bsbiTaxonId}{The BSBI taxon ID}
#'   \item{TVK}{The United Kingdom Species Inventory (UKSI) Taxon Version Key (TVK)}
#'   \item{ddb_name}{The taxon name as present in the BSBI database.}
#'   \item{assignNVCSpecies}{The taxon name as present in assignNVC}
#'   \item{proposedSpecies}{The proposed species name - the name used in RMAVIS}
#' }
"concordance"               

#' 
#'
#' A dataset containing 
#'
#' \code{} 
#'
#' @format A data frame with `r nrow(RMAVIS::example_data)` rows and `r ncol(RMAVIS::example_data)` columns, the definitions of which are:
#' \describe{
#'   \item{}{}
#' }
"example_data"                   

#' 
#'
#' A dataset containing 
#'
#' \code{} 
#'
#' @format A data frame with `r nrow(RMAVIS::master_data)` rows and `r ncol(RMAVIS::master_data)` columns, the definitions of which are:
#' \describe{
#'   \item{}{}
#' }
"master_data"

#' 
#'
#' A dataset containing 
#'
#' \code{} 
#'
#' @format A data frame with `r nrow(RMAVIS::nvc_community_namesCodes)` rows and `r ncol(RMAVIS::nvc_community_namesCodes)` columns, the definitions of which are:
#' \describe{
#'   \item{}{}
#' }
"nvc_community_namesCodes"                  

#' 
#'
#' A dataset containing 
#'
#' \code{} 
#'
#' @format A data frame with `r nrow(RMAVIS::nvc_floristic_tables_numeric)` rows and `r ncol(RMAVIS::nvc_floristic_tables_numeric)` columns, the definitions of which are:
#' \describe{
#'   \item{}{}
#' }
"nvc_floristic_tables_numeric"               

#' 
#'
#' A dataset containing 
#'
#' \code{} 
#'
#' @format A data frame with `r nrow(RMAVIS::nvc_floristic_tables)` rows and `r ncol(RMAVIS::nvc_floristic_tables)` columns, the definitions of which are:
#' \describe{
#'   \item{}{}
#' }
"nvc_floristic_tables"                        

#' 
#'
#' A dataset containing 
#'
#' \code{} 
#'
#' @format A data frame with `r nrow(RMAVIS::nvc_pquad_dca_centroids)` rows and `r ncol(RMAVIS::nvc_pquad_dca_centroids)` columns, the definitions of which are:
#' \describe{
#'   \item{}{}
#' }
"nvc_pquad_dca_centroids"                

#' 
#'
#' A dataset containing 
#'
#' \code{} 
#'
#' @format A data frame with `r nrow(RMAVIS::nvc_pquad_dca_hulls)` rows and `r ncol(RMAVIS::nvc_pquad_dca_hulls)` columns, the definitions of which are:
#' \describe{
#'   \item{}{}
#' }
"nvc_pquad_dca_hulls"                      

#' 
#'
#' A dataset containing 
#'
#' \code{} 
#'
#' @format A data frame with `r nrow(RMAVIS::nvc_pquad_dca_noBryophytes_centroids)` rows and `r ncol(RMAVIS::nvc_pquad_dca_noBryophytes_centroids)` columns, the definitions of which are:
#' \describe{
#'   \item{}{}
#' }
"nvc_pquad_dca_noBryophytes_centroids"         

#' 
#'
#' A dataset containing 
#'
#' \code{} 
#'
#' @format A data frame with `r nrow(RMAVIS::nvc_pquad_dca_noBryophytes_hulls)` rows and `r ncol(RMAVIS::nvc_pquad_dca_noBryophytes_hulls)` columns, the definitions of which are:
#' \describe{
#'   \item{}{}
#' }
"nvc_pquad_dca_noBryophytes_hulls"             

#' 
#'
#' A dataset containing 
#'
#' \code{} 
#'
#' @format A data frame with `r nrow(RMAVIS::nvc_pquad_dca_noBryophytes)` rows and `r ncol(RMAVIS::nvc_pquad_dca_noBryophytes)` columns, the definitions of which are:
#' \describe{
#'   \item{}{}
#' }
"nvc_pquad_dca_noBryophytes"

#' 
#'
#' A dataset containing 
#'
#' \code{} 
#'
#' @format A data frame with `r nrow(RMAVIS::nvc_pquad_dca)` rows and `r ncol(RMAVIS::nvc_pquad_dca)` columns, the definitions of which are:
#' \describe{
#'   \item{}{}
#' }
"nvc_pquad_dca" 

#' 
#'
#' A dataset containing 
#'
#' \code{} 
#'
#' @format A data frame with `r nrow(RMAVIS::nvc_pquads_final_wide)` rows and `r ncol(RMAVIS::nvc_pquads_final_wide)` columns, the definitions of which are:
#' \describe{
#'   \item{}{}
#' }
"nvc_pquads_final_wide"                        

#' The pseudo-quadrats for each NVC unit
#'
#' A dataset containing pseudo-quadrats for each NVC unit.
#'
#' \code{nvc_pquads_final} 
#'
#' @format A data frame with `r nrow(RMAVIS::nvc_pquads_final)` rows and `r ncol(RMAVIS::nvc_pquads_final)` columns, the definitions of which are:
#' \describe{
#'   \item{NVC}{The NVC community or sub-community code}
#'   \item{Pid3}{The pseudo-quadrat ID}
#'   \item{species}{The taxon name}
#' }
"nvc_pquads_final"                             

#'
#'
#'
#'
#' \code{} 
#'
#' @format A data frame with `r nrow(RMAVIS::nvc_pquads_mean_unweighted_eivs_noBryophytes)` rows and `r ncol(RMAVIS::nvc_pquads_mean_unweighted_eivs_noBryophytes)` columns, the definitions of which are:
#' \describe{
#'   \item{}{}
#' }
"nvc_pquads_mean_unweighted_eivs_noBryophytes"

#' 
#'
#' A dataset containing 
#'
#' \code{} 
#'
#' @format A data frame with `r nrow(RMAVIS::nvc_pquads_mean_unweighted_eivs)` rows and `r ncol(RMAVIS::nvc_pquads_mean_unweighted_eivs)` columns, the definitions of which are:
#' \describe{
#'   \item{}{}
#' }
"nvc_pquads_mean_unweighted_eivs"              
