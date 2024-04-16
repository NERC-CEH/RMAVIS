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
#' A vector containing the names of the habitat classification systems for which there is correspondence data in `RMAVIS::habCor_data`
#'
#' \code{habCor_classifications} 
#'
"habCor_classifications"   

#' Habitat correspondence data
#'
#' A dataset containing habitat correspondences between NVC communities and the
#' following classification systems.
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

#' A named list of prefixes for NVC habitats
#'
#' A named list of prefixes for NVC habitats, for each of the broad NVC habitats
#' (`r names(RMAVIS::habitatRestrictionPrefixes)`)
#'
#' \code{habitatRestrictionPrefixes} 
#'
#' @format A named list with `r nrow(RMAVIS::habitatRestrictionPrefixes)` entries.
"habitatRestrictionPrefixes"

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
#'   \item{TaxonGroup}{The name of the taxonomic group that species belongs to.}
#' }
"concordance"               

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

#' Hill-Ellenberg values for vascular plant and bryophyte taxa.
#'
#' A data frame containing the Hill-Ellenberg environmental indicator values for 
#' vascular plant and bryophyte taxa.
#' Named 'master_data' as additional trait and conservation status data will be
#' added in the future.
#'
#' \code{master_data} 
#'
#' @format A data frame with `r nrow(RMAVIS::master_data)` rows and `r ncol(RMAVIS::master_data)` columns, the definitions of which are:
#' \describe{
#'   \item{species}{The taxon name}
#'   \item{F}{The Hill-Ellnberg moisture score}
#'   \item{L}{The Hill-Ellnberg light score}
#'   \item{N}{The Hill-Ellnberg nitrogen score}
#'   \item{R}{The Hill-Ellnberg reaction score}
#'   \item{S}{The Hill-Ellnberg salinity score}
#' }
"master_data"

#' NVC community names and codes.
#'
#' A data frame containing the names and codes of each NVC community.
#'
#' \code{nvc_community_namesCodes} 
#'
#' @format A data frame with `r nrow(RMAVIS::nvc_community_namesCodes)` rows and `r ncol(RMAVIS::nvc_community_namesCodes)` columns, the definitions of which are:
#' \describe{
#'   \item{NVC.Code}{The code associated with the NVC community named in the NVC.Name column.}
#'   \item{NVC.Name}{The name of each NVC community.}
#' }
"nvc_community_namesCodes"                  

#' The NVC floristic tables with numeric constancy values
#'
#' A dataset containing the floristic tables for each NVC community, with the
#' ordinal constancy classes transformed to numeric values such that I = 1,
#' II = 2, III = 3, IV = 4, and V = 5.
#'
#' \code{nvc_floristic_tables_numeric} 
#'
#' @format A data frame with `r nrow(RMAVIS::nvc_floristic_tables_numeric)` rows and `r ncol(RMAVIS::nvc_floristic_tables_numeric)` columns, the definitions of which are:
#' \describe{
#'   \item{Species}{The name of the taxon in a given floristic table.}
#'   \item{NVC.Code}{The code of the NVC community.}
#'   \item{Constancy}{The numeric constancy value.}
#' }
"nvc_floristic_tables_numeric"               

#' The NVC floristic tables
#'
#' A dataset containing the floristic tables for each NVC community, with 
#' ordinal constancy classes representing the ranges of each species
#' relative frequency of occurrence across the samples which constitute the
#' NVC unit, which are as follows: I (1-20%), II (21-40%), III (41-60%), 
#' IV (61-80%), and V(81-100%).
#'
#' \code{nvc_floristic_tables} 
#'
#' @format A data frame with `r nrow(RMAVIS::nvc_floristic_tables)` rows and `r ncol(RMAVIS::nvc_floristic_tables)` columns, the definitions of which are:
#' \describe{
#'   \item{Species}{The name of the taxon in a given floristic table.}
#'   \item{NVC.Code}{The code of the NVC community.}
#'   \item{Constancy}{The ordinal constancy value.}
#' }
"nvc_floristic_tables"                        

#' The centroids of each NVC communities pseudo-quadrats
#'
#' A data frame containing the centroids of each NVC communities pseudo-quadrats
#' in an ordination space constructed by performing a Detrended Correspondence
#' Analysis on all pseudo-quadrats present in `RMAVIS::nvc_pquads_wide`,
#' and returned in `RMAVIS::nvc_pquad_dca`.
#'
#' \code{nvc_pquad_dca_centroids} 
#'
#' @format A data frame with `r nrow(RMAVIS::nvc_pquad_dca_centroids)` rows and `r ncol(RMAVIS::nvc_pquad_dca_centroids)` columns, the definitions of which are:
#' \describe{
#'   \item{NVC}{The NVC community code.}
#'   \item{DCA1}{The DCA scores for axis 1.}
#'   \item{DCA2}{The DCA scores for axis 2.}
#'   \item{DCA3}{The DCA scores for axis 3.}
#'   \item{DCA4}{The DCA scores for axis 4.}
#' }
"nvc_pquad_dca_centroids"                

#' Two dimensional hulls for each NVC communities pseudo-quadrats
#'
#' A data frame containing the two dimensional hulls for each NVC communities
#' pseudo-quadrats DCA scores taken from `RMAVIS::nvc_pquad_dca`
#' for three combinations of axes: DCA1 x DCA2, DCA1 x DCA3, and DCA2 x DCA3.
#'
#' \code{nvc_pquad_dca_hulls} 
#'
#' @format A data frame with `r nrow(RMAVIS::nvc_pquad_dca_hulls)` rows and `r ncol(RMAVIS::nvc_pquad_dca_hulls)` columns, the definitions of which are:
#' \describe{
#'   \item{NVC}{The NVC community code.}
#'   \item{Quadrat}{The code of the pseudo-quadrat which forms part of the perimeter of the hull drawn from all pseudo-quadrats in that given NVC community.}
#'   \item{dcaAxes}{The combination of DCA axes from representing the 2d hull.}
#'   \item{DCA1}{The DCA scores for axis 1.}
#'   \item{DCA2}{The DCA scores for axis 2.}
#'   \item{DCA3}{The DCA scores for axis 3.}
#'   \item{DCA4}{The DCA scores for axis 4.}
#' }
"nvc_pquad_dca_hulls"                      

#' The centroids of each NVC communities pseudo-quadrats, excluding bryophytes
#'
#' A data frame containing the centroids of each NVC communities pseudo-quadrats
#' in an ordination space constructed by performing a Detrended Correspondence
#' Analysis on all pseudo-quadrats present in `RMAVIS::nvc_pquads_wide`
#' with bryophyte taxa removed, and returned in 
#' `RMAVIS::nvc_pquad_dca_noBryophytes`.
#'
#' \code{nvc_pquad_dca_noBryophytes_centroids} 
#'
#' @format A data frame with `r nrow(RMAVIS::nvc_pquad_dca_noBryophytes_centroids)` rows and `r ncol(RMAVIS::nvc_pquad_dca_noBryophytes_centroids)` columns, the definitions of which are:
#' \describe{
#'   \item{NVC}{The NVC community code.}
#'   \item{DCA1}{The DCA scores for axis 1.}
#'   \item{DCA2}{The DCA scores for axis 2.}
#'   \item{DCA3}{The DCA scores for axis 3.}
#'   \item{DCA4}{The DCA scores for axis 4.}
#' }
"nvc_pquad_dca_noBryophytes_centroids"         

#' Two dimensional hulls for each NVC communities pseudo-quadrats, excluding bryophytes
#'
#' A data frame containing the two dimensional hulls for each NVC communities
#' pseudo-quadrats DCA scores taken from `RMAVIS::nvc_pquad_dca_noBryophytes` 
#' for three combinations of axes: DCA1 x DCA2, DCA1 x DCA3, and DCA2 x DCA3.
#'
#' \code{nvc_pquad_dca_noBryophytes_hulls} 
#'
#' @format A data frame with `r nrow(RMAVIS::nvc_pquad_dca_noBryophytes_hulls)` rows and `r ncol(RMAVIS::nvc_pquad_dca_noBryophytes_hulls)` columns, the definitions of which are:
#' \describe{
#'   \item{NVC}{The NVC community code.}
#'   \item{Quadrat}{The code of the pseudo-quadrat which forms part of the perimeter of the hull drawn from all pseudo-quadrats in that given NVC community.}
#'   \item{dcaAxes}{The combination of DCA axes from representing the 2d hull.}
#'   \item{DCA1}{The DCA scores for axis 1.}
#'   \item{DCA2}{The DCA scores for axis 2.}
#'   \item{DCA3}{The DCA scores for axis 3.}
#'   \item{DCA4}{The DCA scores for axis 4.}
#' }
"nvc_pquad_dca_noBryophytes_hulls"             

#' The DCA results for all pseudo-quadrats, excluding bryophytes
#'
#' The results of a Detrended Correspondence Analysis (DCA) performed on all
#' pseudo-quadrats present in `RMAVIS::nvc_pquads_wide`, 
#' exluding bryophyte taxa.
#'
#' \code{nvc_pquad_dca_noBryophytes} 
#'
#' @format An object of class "decorana". See `vegan::decanora`.
"nvc_pquad_dca_noBryophytes"

#' The DCA results for all pseudo-quadrats
#'
#' The results of a Detrended Correspondence Analysis (DCA) performed on all
#' pseudo-quadrats present in `RMAVIS::nvc_pquads_wide`.
#'
#' \code{nvc_pquad_dca} 
#'
#' @format An object of class "decorana". See `vegan::decanora`.
"nvc_pquad_dca" 

#' The pseudo-quadrats for each NVC community in wide format
#'
#' A data frame containing the pseudo-quadrats for each NVC community in wide
#' (pseudo-quadrat by species) format
#'
#' \code{nvc_pquads_wide} 
#'
#' @format A data frame with `r nrow(RMAVIS::nvc_pquads_wide)` rows and `r ncol(RMAVIS::nvc_pquads_wide)` columns.
"nvc_pquads_wide"                        

#' The pseudo-quadrats for each NVC unit
#'
#' A dataset containing pseudo-quadrats for each NVC unit.
#'
#' \code{nvc_pquads} 
#'
#' @format A data frame with `r nrow(RMAVIS::nvc_pquads)` rows and `r ncol(RMAVIS::nvc_pquads)` columns, the definitions of which are:
#' \describe{
#'   \item{NVC}{The NVC community or sub-community code}
#'   \item{Pid3}{The pseudo-quadrat ID}
#'   \item{species}{The taxon name}
#' }
"nvc_pquads"                             

#' The mean unweighted Hill-Ellenberg values for all pseudo-quadrats, excluding bryophytes
#'
#' A data frame containing the mean unweighted Hill-Ellenberg values for all 
#' pseudo-quadrats excluding bryophytes, calculated using 
#' `RMAVIS::nvc_pquad_dca_noBryophytes` and `RMAVIS::master_data`.
#'
#' \code{nvc_pquads_mean_unweighted_eivs_noBryophytes} 
#'
#' @format A data frame with `r nrow(RMAVIS::nvc_pquads_mean_unweighted_eivs_noBryophytes)` rows and `r ncol(RMAVIS::nvc_pquads_mean_unweighted_eivs_noBryophytes)` columns, the definitions of which are:
#' \describe{
#'   \item{Pid3}{The pseudo-quadrat ID.}
#'   \item{F}{The mean unweighted Hill-Ellenberg moisture value}
#'   \item{L}{The mean unweighted Hill-Ellenberg light value}
#'   \item{N}{The mean unweighted Hill-Ellenberg nitrogen value}
#'   \item{R}{The mean unweighted Hill-Ellenberg reaction value}
#'   \item{S}{The mean unweighted Hill-Ellenberg salinity value}
#' }
"nvc_pquads_mean_unweighted_eivs_noBryophytes"

#' The mean unweighted Hill-Ellenberg values for all pseudo-quadrats
#'
#' A data frame containing the mean unweighted Hill-Ellenberg values for all 
#' pseudo-quadrats, calculated using `RMAVIS::nvc_pquads` and 
#' `RMAVIS::master_data`.
#'
#' \code{nvc_pquads_mean_unweighted_eivs} 
#'
#' @format A data frame with `r nrow(RMAVIS::nvc_pquads_mean_unweighted_eivs)` rows and `r ncol(RMAVIS::nvc_pquads_mean_unweighted_eivs)` columns, the definitions of which are:
#' \describe{
#'   \item{Pid3}{The pseudo-quadrat ID.}
#'   \item{F}{The mean unweighted Hill-Ellenberg moisture value}
#'   \item{L}{The mean unweighted Hill-Ellenberg light value}
#'   \item{N}{The mean unweighted Hill-Ellenberg nitrogen value}
#'   \item{R}{The mean unweighted Hill-Ellenberg reaction value}
#'   \item{S}{The mean unweighted Hill-Ellenberg salinity value}
#' }
"nvc_pquads_mean_unweighted_eivs"              
