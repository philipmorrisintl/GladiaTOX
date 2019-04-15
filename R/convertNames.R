#####################################################################
## This program is distributed in the hope that it will be useful, ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of  ##
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    ##
## GNU General Public License for more details.                    ##
#####################################################################

#-------------------------------------------------------------------------------
# convertNames: Convert assay names to their abbreviations
#-------------------------------------------------------------------------------

#' @title Convert assay names to their abbreviations
#' 
#' @description 
#' \code{.convertNames} converts the assay names as they appear in the gtox
#' database to their respective abbreviations
#' 
#' @param names Character, strings to convert
#' 
#' @keywords internal
#' 
#' @return The same character vector given with any name strings converted to 
#' the abbreviated version

.convertNames <- function(names) {

    names <- sub("aenm", "assay_component_endpoint_name", names)
    names <- sub("acnm", "assay_component_name", names)
    names <- sub("anm",  "assay_name", names)
    names <- sub("asnm", "assay_source_name", names)
    names <- sub("asph", "assay_source_phase", names)
    names <- sub("gtox_waid", "waid", names)
    names <- sub("gtox_apid", "apid", names)

    names

}
