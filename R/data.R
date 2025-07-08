#' Habitat Correspondences
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
#' \code{habitat_correspondences} 
#'
#' @format A data frame with `r nrow(RMAVIS::habitat_correspondences)` rows and `r ncol(RMAVIS::habitat_correspondences)` columns, the definitions of which are:
#' \describe{
#'   \item{NVC.Code}{The NVC community code.}
#'   \item{Relationship}{The relationship between the NVC community and the habitat present in the 'label' column.}
#'   \item{Code}{The code associated with the habitat present in the 'label' column.}
#'   \item{Label}{The corresponding habitat.}
#'   \item{Classification}{The habitat classification system.}
#' }
"habitat_correspondences"

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
#' Named 'hill_ellenberg' as additional trait and conservation status data will be
#' added in the future.
#'
#' \code{hill_ellenberg} 
#'
#' @format A data frame with `r nrow(RMAVIS::hill_ellenberg)` rows and `r ncol(RMAVIS::hill_ellenberg)` columns, the definitions of which are:
#' \describe{
#'   \item{TVK}{The taxon, represented by the UKSI taxon version key.}
#'   \item{F}{The Hill-Ellenberg moisture score}
#'   \item{L}{The Hill-Ellenberg light score}
#'   \item{N}{The Hill-Ellenberg nitrogen score}
#'   \item{R}{The Hill-Ellenberg reaction score}
#'   \item{S}{The Hill-Ellenberg salinity score}
#' }
"hill_ellenberg"

#' NVC floristic tables
#'
#' A dataset containing the floristic tables for each NVC community.
#'
#' \code{nvc_floristic_tables} 
#'
#' @format A data frame with `r nrow(RMAVIS::nvc_floristic_tables)` rows and `r ncol(RMAVIS::nvc_floristic_tables)` columns, the definitions of which are:
#' \describe{
#'   \item{nvc_code}{The NVC unit code.}
#'   \item{nvc_taxon_name}{The taxon name, see `RMAVIS::nvc_taxa_lookup`.}
#'   \item{constancy}{The constancy of occurrence across the plots constituting the NVC unit, see `RMAVIS:::constancyConversion`.}
#'   \item{absolute_frequency}{The number of occurrences in the plots constituting the NVC unit.}
#'   \item{relative_frequency}{The proportion of plots constituting the NVC unit that the taxon is present in.}
#'   \item{minimum_cover}{The minimum cover of the taxon in the plots constituting the NVC unit.}
#'   \item{mean_cover}{The mean cover of the taxon in the plots constituting the NVC unit.}
#'   \item{maximum_cover}{The maximum cover of the taxon in the plots constituting the NVC unit.}
#' }
"nvc_floristic_tables"

#' NVC floristic tables comparison
#' 
#' A lookup table between the original NVC floristic table taxon names and updated taxon names.#' 
#'
#' \code{nvc_floristic_tables_comparison} 
#'
#' @format A data frame with `r nrow(RMAVIS::nvc_floristic_tables_comparison)` rows and `r ncol(RMAVIS::nvc_floristic_tables_comparison)` columns, the definitions of which are:
#' \describe{
#'   \item{nvc_code}{The NVC unit code.}
#'   \item{original_name}{The taxon name as it appears in the original NVC MS access database and volumes.}
#'   \item{nvc_taxon_name}{The updated taxon name, see `RMAVIS::nvc_taxa_lookup`.}
#' }
"nvc_floristic_tables_comparison"

#' NVC pseudo-quadrats
#'
#' A dataset containing pseudo-quadrats for each NVC unit.
#'
#' \code{nvc_pquads} 
#'
#' @format A data frame with `r nrow(RMAVIS::nvc_pquads)` rows and `r ncol(RMAVIS::nvc_pquads)` columns, the definitions of which are:
#' \describe{
#'   \item{nvc_code}{The NVC unit code.}
#'   \item{psq_id}{The pseudo-quadrat ID}
#'   \item{nvc_taxon_name}{The taxon name, see `RMAVIS::nvc_taxa_lookup`.}
#' }
"nvc_pquads"

#' NVC Community Attributes
#'
#' Selected attributes for each NVC unit, exported from the original NVC MS Access database.
#'
#' \code{nvc_community_attributes} 
#'
#' @format A data frame with `r nrow(RMAVIS::nvc_community_attributes)` rows and `r ncol(RMAVIS::nvc_community_attributes)` columns, the definitions of which are:
#' \describe{
#'   \item{fullname}{The full name of the NVC unit.}
#'   \item{name}{The abridged name of the NVC unit.}
#'   \item{nvc_code}{The NVC unit code.}
#'   \item{nvc_code_parent}{The NVC unit code of the parent community, e.g. A2 for A2a.}
#'   \item{basal}{A boolean. If TRUE the NVC unit does not have any child units e.g. A1. If FALSE the NVC unit has child units, e.g. A.}
#'   \item{rank}{The rank of the NVC unit.}
#'   \item{num_samples}{The number of plots (samples) used to define the NVC unit.}
#'   \item{min_species}{The minimum number of species in the plots constituting the NVC unit.}
#'   \item{max_species}{The maximum number of species in the plots constituting the NVC unit.}
#'   \item{mean_species}{The mean number of species in the plots constituting the NVC unit.}
#'   \item{species_count}{The total number of unique species in the plots constituting the NVC unit. Note that this does not include 'rare' species, species that occurred in less than 5% of plots.}
#' }
"nvc_community_attributes"

#' NVC Taxa Lookup
#'
#' A lookup between the original taxon names (as present in the NVC MS Access database and volumes) 
#' the associated recommended taxon names in version 20250703a of the UKSI, 
#' and the 'nvc taxon names', which are equivalent to the recommended taxon names with strata suffixes.
#' 
#' \code{nvc_taxa_lookup} 
#'
#' @format A data frame with `r nrow(RMAVIS::nvc_taxa_lookup)` rows and `r ncol(RMAVIS::nvc_taxa_lookup)` columns, the definitions of which are:
#' \describe{
#'   \item{original_name}{The taxon name as it appears in the original NVC MS access database and volumes.}
#'   \item{original_TVK}{The UKSI taxon version key associated with the original taxon name, see `RMAVIS::taxa_lookup`.}
#'   \item{recommended_taxon_name}{The latest recommended name for the taxon concept recorded as the original name, see `RMAVIS::taxonomic_backbone`.}
#'   \item{recommended_TVK}{The UKSI taxon version key associated with the recommended taxon name, see `RMAVIS::taxa_lookup`.}
#'   \item{strata}{The strata for the taxon: c = Canopy, S = Shrub, and g = Ground.}
#'   \item{nvc_taxon_name}{The taxon name, equivalent to the recommended taxon names with strata suffixes.}
#' }
"nvc_taxa_lookup"

#' Accepted Taxa
#'
#' The taxon names accepted by RMAVIS. Equivalent to the unique species in the `RMAVIS::taxonomic_backbone` 'taxon_name' column
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

#' Taxa lookup
#'
#' A lookup between the reccomended taxa as present in `RMAVIS::taxonomic_backbone` and associated synonymous taxon concepts present in the UKSI.
#'
#' \code{taxa_lookup} 
#'
#' @format A data frame with `r nrow(RMAVIS::taxa_lookup)` rows and `r ncol(RMAVIS::taxa_lookup)` columns, the definitions of which are:
#' \describe{
#'   \item{taxon_name}{The taxon names of the synonymous taxon concepts associated with the reccomended taxa.}
#'   \item{TVK}{The UKSI taxon version keys associated with the synonymous taxa in taxon_name.}
#'   \item{recommended_taxon_name}{The recommended taxon names.}
#'   \item{recommended_TVK}{The UKSI taxon version keys associated with the taxa in recommended_taxon_name.}
#' }
"taxa_lookup"

#' Taxonomic Backbone
#' 
#' The taxonomic backbone used in RMAVIS. Formed by selecting the recommended taxa from the following informal groups:
#' "alga", "chromist", "clubmoss", "conifer", "fern", "flowering plant", "ginkgo", "hornwort", "horsetail", "lichen", "liverwort", "moss", "quillwort", and "stonewort"
#' in version 20250703a of the UKSI, then retrieving the parent taxa.
#' 
#'
#' \code{taxonomic_backbone} 
#'
#' @format A data frame with `r nrow(RMAVIS::taxonomic_backbone)` rows and `r ncol(RMAVIS::taxonomic_backbone)` columns, the definitions of which are:
#' \describe{
#'   \item{TVK}{}
#'   \item{taxon_name}{}
#'   \item{rank}{}
#'   \item{qualifier}{}
#'   \item{authority}{}
#'   \item{informal_group}{}
#'   \item{Unranked}{}
#'   \item{Domain}{}
#'   \item{Kingdom}{}
#'   \item{Subkingdom}{}
#'   \item{Infrakingdom}{}
#'   \item{Division}{}
#'   \item{Subdivision}{}
#'   \item{Phylum}{}
#'   \item{Subphylum}{}
#'   \item{Infraphylum}{}
#'   \item{Superclass}{}
#'   \item{Class}{}
#'   \item{Subclass}{}
#'   \item{Superorder}{}
#'   \item{Order}{}
#'   \item{Suborder}{}
#'   \item{Superfamily}{}
#'   \item{Family}{}
#'   \item{Subfamily}{}
#'   \item{Family aggregate}{}
#'   \item{Tribe}{}
#'   \item{Genus}{}
#'   \item{Genus aggregate}{}
#'   \item{Subgenus}{}
#'   \item{Section}{}
#'   \item{Subsection}{}
#'   \item{Species group}{}
#'   \item{Series}{}
#'   \item{Species aggregate}{}
#'   \item{Species sensu lato}{}
#'   \item{Generic hybrid}{}
#'   \item{Species}{}
#'   \item{Species pro parte}{}
#'   \item{Species hybrid}{}
#'   \item{Subspecies}{}
#'   \item{Subspecies hybrid}{}
#'   \item{Subspecies aggregate}{}
#'   \item{Nothosubspecies}{}
#'   \item{Microspecies}{}
#'   \item{Cultivar}{}
#' }
"taxonomic_backbone"

"nvc_psquad_cm_he"

"nvc_comm_cm_he"

"dca_psquad_centroids"

"dca_psquad_hulls"

"dca_species_scores"

"dca_psquad_scores"

"dca_psquad"

"broad_habitat_indicators"
