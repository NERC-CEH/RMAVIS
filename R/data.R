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

#' NVC community-mean Hill-Ellenberg values
#'
#' A dataset containing community-mean Hill-Ellenberg environmental indicator valueas for each NVC community.
#'
#' \code{nvc_cm_he} 
#'
#' @format A data frame with `r nrow(RMAVIS::nvc_cm_he)` rows and `r ncol(RMAVIS::nvc_cm_he)` columns, the definitions of which are:
#' \describe{
#'   \item{nvc_code}{The NVC unit code.}
#'   \item{indicator}{The Hill-Ellenberg indicator code, one of: F, L, N, R, S}
#'   \item{mean}{The mean indicator value.}
#'   \item{sd}{The standard deviation of the indicator value.}
#' }
"nvc_cm_he"

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
#'   \item{original_taxon_name}{The taxon name as it appears in the original NVC MS access database and volumes.}
#'   \item{original_TVK}{The UKSI taxon version key associated with the original taxon name, see `UKVegTB::taxa_lookup`.}
#'   \item{recommended_taxon_name}{The latest recommended name for the taxon concept recorded as the original name, see `UKVegTB::taxonomic_backbone`.}
#'   \item{recommended_TVK}{The UKSI taxon version key associated with the recommended taxon name, see `UKVegTB::taxa_lookup`.}
#'   \item{strata}{The strata for the taxon: c = Canopy, S = Shrub, and g = Ground.}
#'   \item{nvc_taxon_name}{The taxon name, equivalent to the recommended taxon names with strata suffixes.}
#' }
"nvc_taxa_lookup"

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

#' NVC pseudo-quadrat community-mean Hill-Ellenberg values
#'
#' NVC pseudo-quadrat community-mean Hill-Ellenberg values
#'
#' \code{nvc_psquad_cm_he} 
#'
#' @format A data frame with `r nrow(RMAVIS::nvc_psquad_cm_he)` rows and `r ncol(RMAVIS::nvc_psquad_cm_he)` columns, the definitions of which are:
#' \describe{
#'   \item{nvc_code}{The NVC unit code.}
#'   \item{psq_id}{The pseudo-quadrat ID.}
#'   \item{F}{The Hill-Ellenberg Moisture score.}
#'   \item{L}{The Hill-Ellenberg Light score.}
#'   \item{N}{The Hill-Ellenberg Nitrogen score.}
#'   \item{R}{The Hill-Ellenberg Reaction score.}
#'   \item{S}{The Hill-Ellenberg Salinity score.}
#' }
"nvc_psquad_cm_he"

# "dca_psquad_centroids"
# 
# "dca_psquad_hulls"
# 
# "dca_species_scores"
# 
# "dca_psquad_scores"
# 
# "dca_psquad"

#' SOWG floristic tables
#'
#' A dataset containing the floristic tables for each Scottish Oceanic Wet Grassland NVC community.
#'
#' \code{sowg_floristic_tables} 
#'
#' @format A data frame with `r nrow(RMAVIS::broad_habitat_indicators)` rows and `r ncol(RMAVIS::broad_habitat_indicators)` columns, the definitions of which are:
#' \describe{
#'   \item{recommended_taxon_name}{The taxon name, see `UKVegTB::taxonomic_backbone`}
#'   \item{recommended_TVK}{The UKSI taxon version key, see `UKVegTB::taxonomic_backbone`.}
#'   \item{1}{The broad habitat 'Broadleaved, mixed and yew woodland'.}
#'   \item{2}{The broad habitat 'Coniferous woodland'.}
#'   \item{3}{The broad habitat 'Boundary and linear features'.}
#'   \item{4}{The broad habitat 'Arable and horticultural'.}
#'   \item{5}{The broad habitat 'Improved grassland'.}
#'   \item{6}{The broad habitat 'Neutral grassland'.}
#'   \item{7}{The broad habitat 'Calcareous grassland'.}
#'   \item{8}{The broad habitat 'Acid grassland'.}
#'   \item{9}{The broad habitat 'Bracken'.}
#'   \item{10}{The broad habitat 'Dwarf shrub heath'.}
#'   \item{11}{The broad habitat 'Fen, marsh and swamp'.}
#'   \item{12}{The broad habitat 'Bog'.}
#'   \item{13}{The broad habitat 'Standing water and canals'.}
#'   \item{14}{The broad habitat 'Rivers and streams'.}
#'   \item{15}{The broad habitat 'Montane habitats'.}
#'   \item{16}{The broad habitat 'Inland rock'.}
#'   \item{17}{The broad habitat 'Built-up areas and gardens'.}
#'   \item{18}{The broad habitat 'Supralittoral rock'.}
#'   \item{19}{The broad habitat 'Supralittoral sediment'.}
#'   \item{21}{The broad habitat 'Littoral sediment'.}
#'   \item{23}{The broad habitat 'Inshore sublittoral sediment'.}
#' }
"broad_habitat_indicators"

#' SOWG floristic tables
#'
#' A dataset containing the floristic tables for each Scottish Oceanic Wet Grassland NVC community.
#'
#' \code{sowg_floristic_tables} 
#'
#' @format A data frame with `r nrow(RMAVIS::sowg_floristic_tables)` rows and `r ncol(RMAVIS::sowg_floristic_tables)` columns, the definitions of which are:
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
"sowg_floristic_tables"

#' SOWG Community Attributes
#'
#' Selected attributes for each Scottish Oceanic Wet Grassland NVC unit.
#'
#' \code{sowg_community_attributes} 
#'
#' @format A data frame with `r nrow(RMAVIS::sowg_community_attributes)` rows and `r ncol(RMAVIS::sowg_community_attributes)` columns, the definitions of which are:
#' \describe{
#'   \item{fullname}{The full name of the NVC unit.}
#'   \item{name}{The abridged name of the NVC unit.}
#'   \item{nvc_code}{The NVC unit code.}
#'   \item{nvc_code_parent}{The NVC unit code of the parent community, e.g. AgBp for AgBpa}
#'   \item{basal}{A boolean. If TRUE the NVC unit does not have any child units e.g. CnPe. If FALSE the NVC unit has child units, e.g. AgBp.}
#'   \item{rank}{The rank of the NVC unit.}
#'   \item{num_samples}{The number of plots (samples) used to define the NVC unit.}
#'   \item{min_species}{The minimum number of species in the plots constituting the NVC unit.}
#'   \item{max_species}{The maximum number of species in the plots constituting the NVC unit.}
#'   \item{mean_species}{The mean number of species in the plots constituting the NVC unit.}
#'   \item{species_count}{The total number of unique species in the plots constituting the NVC unit. Note that this does not include 'rare' species, species that occurred in less than 5% of plots.}
#' }
"sowg_community_attributes"

#' SOWG pseudo-quadrats
#'
#' A dataset containing pseudo-quadrats for each Scottish Oceanic Wet Grassland NVC unit.
#'
#' \code{sowg_pquads} 
#'
#' @format A data frame with `r nrow(RMAVIS::sowg_pquads)` rows and `r ncol(RMAVIS::sowg_pquads)` columns, the definitions of which are:
#' \describe{
#'   \item{nvc_code}{The NVC unit code.}
#'   \item{psq_id}{The pseudo-quadrat ID}
#'   \item{nvc_taxon_name}{The taxon name, see `RMAVIS::nvc_taxa_lookup`.}
#' }
"sowg_pquads"

#' SOWG pseudo-quadrat community-mean Hill-Ellenberg values
#'
#' SOWG pseudo-quadrat community-mean Hill-Ellenberg values
#'
#' \code{sowg_psquad_cm_he} 
#'
#' @format A data frame with `r nrow(RMAVIS::sowg_psquad_cm_he)` rows and `r ncol(RMAVIS::sowg_psquad_cm_he)` columns, the definitions of which are:
#' \describe{
#'   \item{nvc_code}{The SOWG NVC unit code.}
#'   \item{psq_id}{The pseudo-quadrat ID.}
#'   \item{F}{The Hill-Ellenberg Moisture score.}
#'   \item{L}{The Hill-Ellenberg Light score.}
#'   \item{N}{The Hill-Ellenberg Nitrogen score.}
#'   \item{R}{The Hill-Ellenberg Reaction score.}
#'   \item{S}{The Hill-Ellenberg Salinity score.}
#' }
"sowg_psquad_cm_he"

#' SOWG community-mean Hill-Ellenberg values
#'
#' A dataset containing community-mean Hill-Ellenberg environmental indicator valueas for each SOWG community.
#'
#' \code{sowg_cm_he} 
#'
#' @format A data frame with `r nrow(RMAVIS::sowg_cm_he)` rows and `r ncol(RMAVIS::sowg_cm_he)` columns, the definitions of which are:
#' \describe{
#'   \item{nvc_code}{The SOWG unit code.}
#'   \item{indicator}{The Hill-Ellenberg indicator code, one of: F, L, N, R, S}
#'   \item{mean}{The mean indicator value.}
#'   \item{sd}{The standard deviation of the indicator value.}
#' }
"sowg_cm_he"

#' Calthion floristic tables
#'
#' A dataset containing the floristic tables for each Calthion NVC community.
#'
#' \code{calthion_floristic_tables} 
#'
#' @format A data frame with `r nrow(RMAVIS::calthion_floristic_tables)` rows and `r ncol(RMAVIS::calthion_floristic_tables)` columns, the definitions of which are:
#' \describe{
#'   \item{nvc_code}{The Calthion unit code.}
#'   \item{nvc_taxon_name}{The taxon name, see `RMAVIS::nvc_taxa_lookup`.}
#'   \item{constancy}{The constancy of occurrence across the plots constituting the NVC unit, see `RMAVIS:::constancyConversion`.}
#'   \item{absolute_frequency}{The number of occurrences in the plots constituting the NVC unit.}
#'   \item{relative_frequency}{The proportion of plots constituting the NVC unit that the taxon is present in.}
#'   \item{minimum_cover}{The minimum cover of the taxon in the plots constituting the NVC unit.}
#'   \item{mean_cover}{The mean cover of the taxon in the plots constituting the NVC unit.}
#'   \item{maximum_cover}{The maximum cover of the taxon in the plots constituting the NVC unit.}
#' }
"calthion_floristic_tables"

#' Calthion pseudo-quadrats
#'
#' A dataset containing pseudo-quadrats for each Calthion community unit.
#'
#' \code{calthion_pquads} 
#'
#' @format A data frame with `r nrow(RMAVIS::calthion_pquads)` rows and `r ncol(RMAVIS::calthion_pquads)` columns, the definitions of which are:
#' \describe{
#'   \item{nvc_code}{The calthion unit code.}
#'   \item{psq_id}{The pseudo-quadrat ID}
#'   \item{nvc_taxon_name}{The taxon name, see `RMAVIS::nvc_taxa_lookup`.}
#' }
"calthion_pquads"

#' Calthion pseudo-quadrat community-mean Hill-Ellenberg values
#'
#' Calthion pseudo-quadrat community-mean Hill-Ellenberg values
#'
#' \code{calthion_psquad_cm_he} 
#'
#' @format A data frame with `r nrow(RMAVIS::calthion_psquad_cm_he)` rows and `r ncol(RMAVIS::calthion_psquad_cm_he)` columns, the definitions of which are:
#' \describe{
#'   \item{nvc_code}{The Calthion community unit code.}
#'   \item{psq_id}{The pseudo-quadrat ID.}
#'   \item{F}{The Hill-Ellenberg Moisture score.}
#'   \item{L}{The Hill-Ellenberg Light score.}
#'   \item{N}{The Hill-Ellenberg Nitrogen score.}
#'   \item{R}{The Hill-Ellenberg Reaction score.}
#'   \item{S}{The Hill-Ellenberg Salinity score.}
#' }
"calthion_psquad_cm_he"

#' Calthion Community Attributes
#'
#' Selected attributes for each Calthion NVC unit.
#'
#' \code{calthion_community_attributes} 
#'
#' @format A data frame with `r nrow(RMAVIS::calthion_community_attributes)` rows and `r ncol(RMAVIS::calthion_community_attributes)` columns, the definitions of which are:
#' \describe{
#'   \item{fullname}{The full name of the NVC unit.}
#'   \item{name}{The abridged name of the NVC unit.}
#'   \item{nvc_code}{The NVC unit code.}
#'   \item{nvc_code_parent}{The NVC unit code of the parent community, e.g. AgBp for AgBpa}
#'   \item{basal}{A boolean. If TRUE the NVC unit does not have any child units e.g. CnPe. If FALSE the NVC unit has child units, e.g. AgBp.}
#'   \item{rank}{The rank of the NVC unit.}
#'   \item{num_samples}{The number of plots (samples) used to define the NVC unit.}
#'   \item{mean_species}{The mean number of species in the plots constituting the NVC unit.}
#' }
"calthion_community_attributes"

#' Calthion community-mean Hill-Ellenberg values
#'
#' A dataset containing community-mean Hill-Ellenberg environmental indicator valueas for each Calthion community.
#'
#' \code{calthion_cm_he} 
#'
#' @format A data frame with `r nrow(RMAVIS::calthion_cm_he)` rows and `r ncol(RMAVIS::calthion_cm_he)` columns, the definitions of which are:
#' \describe{
#'   \item{nvc_code}{The Calthion unit code.}
#'   \item{indicator}{The Hill-Ellenberg indicator code, one of: F, L, N, R, S}
#'   \item{mean}{The mean indicator value.}
#'   \item{sd}{The standard deviation of the indicator value.}
#' }
"calthion_cm_he"