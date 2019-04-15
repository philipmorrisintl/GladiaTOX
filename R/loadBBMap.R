#####################################################################
## This program is distributed in the hope that it will be useful, ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of  ##
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    ##
## GNU General Public License for more details.                    ##
#####################################################################

#-------------------------------------------------------------------------------
# .loadBBMap: register the biobanking fields
#-------------------------------------------------------------------------------

#' @title Register the biobanking fields
#' @description This function parses the output from the GUI after the GladiaTOX
#' IDs have been loaded into BioBanking and registers the BioBanking IDs into
#' the 'bb_apid_map' and 'bb_waid_map' fields.
#'
#' @param input path to JSON file produced by the GUI
#'
#' @keywords internal
#' 
#' @importFrom stringr str_extract
#' @import data.table
#' 
#' @return None

.loadBBMap <- function(input) {
    
    dat <- .readInput(input)

    dat[ , waid := as.integer(substr(u_smkid,  1,  9))]
    dat[ , apid := as.integer(substr(u_smkid, 10, 19))]

    dat[ , u_boxtrack := gsub("_.*$", "", u_boxtrack)]

    gtoxRegister("bb_waid", unique(dat[ , list(waid, s_sampleid)]))
    gtoxRegister("bb_apid", unique(dat[ , list(apid, u_boxtrack)]))

}
