#####################################################################
## This program is distributed in the hope that it will be useful, ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of  ##
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    ##
## GNU General Public License for more details.                    ##
#####################################################################

#-------------------------------------------------------------------------------
# tcplLoadVmad: Load cutoff values for assay endpoints
#-------------------------------------------------------------------------------

#' @title Load cutoff values for assay endpoints
#'
#' @description
#' \code{tcplLoadVmad} queries the tcpl databases and returns a data.table
#' with the cutoff values for the given assay endpoint ids (aeid).
#'
#' @param aeid Integer, assay endpoint ids
#'
#' @examples
#' ## Store the current config settings, so they can be reloaded at the end 
#' ## of the examples
#' conf_store <- tcplConfList()
#' tcplConfDefault()
#' 
#' ## Prepare for analysis before QC + process data
#' tcplLoadVmad()
#' 
#' ## Reset configuration
#' options(conf_store)
#' 
#' @return A data.table containing cutoff values for the given aeids.
#'
#' @import data.table
#' @export

tcplLoadVmad <- function(aeid=NULL) {

    qformat <-
        "
        SELECT
            aeid,
            vmad,
            inputs,
            notes
        FROM
            assay_component_endpoint_vmad
        "
    if (!is.null(aeid)) qformat <- paste(qformat, "WHERE aeid IN (%s)")

    qstring <- sprintf(qformat, paste(aeid, collapse=","))

    dat <- tcplQuery(query=qstring, db=getOption("TCPL_DB"))

    if (nrow(dat) == 0) {
        warning("The given aeid(s) do not have cutoff values.")
        return(dat)
    }

    len_miss <- lw(!aeid %in% dat$aeid)
    if (len_miss > 0) {
        warning(len_miss, "of the given aeid(s) do not have cutoff values.")
    }

    dat[]

}

#-------------------------------------------------------------------------------
