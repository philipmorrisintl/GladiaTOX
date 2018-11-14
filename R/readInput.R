#####################################################################
## This program is distributed in the hope that it will be useful, ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of  ##
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    ##
## GNU General Public License for more details.                    ##
#####################################################################

#-------------------------------------------------------------------------------
# .readInput: accessory function to parse JSON output from GUI
#-------------------------------------------------------------------------------

#' @title Parse GUI JSON output
#'
#' @param x path to JSON file produced by the GUI
#'
#' @details
#' Will also accept a data.frame and convert it to data.table
#'
#' @note PMI-specific
#' 
#' @importFrom RJSONIO isValidJSON fromJSON
#' @importFrom stringr str_extract
#' @importFrom utils type.convert
#' @import data.table
#' 

.readInput <- function(x) {

    if (is.data.frame(x)) {
        x <- as.data.table(x)
    } else { # If not a data.table, attempt to parse JSON
        if (isValidJSON(x, asText=FALSE)) {
            x <- do.call(rbind, fromJSON(x, simplify=TRUE))
            x <- as.data.table(x)
            x[] <- lapply(
                as.data.table(x),
                na.string="",
                type.convert,
                as.is=TRUE
            )
        } else {
            stop(
                "Provided input does appear",
                "to be JSON or a data.frame object."
            )
        }
        x[]
    }

}
