#####################################################################
## This program is distributed in the hope that it will be useful, ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of  ##
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    ##
## GNU General Public License for more details.                    ##
#####################################################################

#-------------------------------------------------------------------------------
# gtoxLoadVehicle: Load vehicle information
#-------------------------------------------------------------------------------

#' @title Load vehicle information
#'
#' @description
#' \code{gtoxLoadVehicle} queries the gtox database and returns the vehicle
#' information for the given field and values.
#'
#' @param field Character of length 1, the field to query on
#' @param val Vector of values to subset on
#'
#' @examples
#' ## Store the current config settings, so they can be reloaded at the end 
#' ## of the examples
#' conf_store <- gtoxConfList()
#' gtoxConfDefault()
#' 
#' ## Prepare for analysis before QC + process data
#' gtoxLoadVehicle()
#' 
#' ## Reset configuration
#' options(conf_store)
#' 
#' @return A data.table with the list of vehicles and vehicles ids.
#'
#' @import data.table
#' @export

gtoxLoadVehicle <- function(field=NULL, val=NULL) {

    if (!is.null(field)) {
        vfield <- c("vehicle_name", "vhid")
        if (!field %in% vfield) stop("Invalid 'field' value.")
    }

    qformat <- "SELECT vehicle_name, vhid FROM vehicle"

    if (!is.null(field)) {

        qformat <- paste(qformat, "WHERE %s IN (%s);")
        qstring <- sprintf(
            qformat,
            field,
            paste0(
                "\"", val, "\"", collapse=","
            )
        )

    } else {

        qstring <- paste0(qformat, ";")

    }

    dat <- gtoxQuery(query=qstring)

    if (nrow(dat) == 0) {
        warning("The given ", field,"(s) are not in the gtox database.")
        return(dat)
    }

    dat[]

}

#-------------------------------------------------------------------------------
