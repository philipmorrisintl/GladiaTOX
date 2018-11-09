#####################################################################
## This program is distributed in the hope that it will be useful, ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of  ##
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    ##
## GNU General Public License for more details.                    ##
#####################################################################

#-------------------------------------------------------------------------------
# tcplSetWllq: Change the well quality for a vector of lvl 0 IDs
#-------------------------------------------------------------------------------

#' @title Change the well quality for a vector of lvl 0 IDs
#'
#' @description
#' \code{tcplSetWllq} changes the well quality to either 100 or 0 for a given
#' list of 'm0id' or 's0id' values. Changing the well quality initiates a
#' delete cascade for the affected assay components.
#'
#' @param ids Integer, the 'm0id' or 's0id' values to change
#' @param wllq Integer of length 1, the new well quality value, 0 or 1
#' @param type Character of length 1, the data type, "sc" or "mc"
#'
#' @examples
#' ## Store the current config settings, so they can be reloaded at the end
#' ## of the examples
#' conf_store <- tcplConfList()
#' tcplConfDefault()
#'
#' ## Set well quality to zero for specific lvl zero ids.
#' tcplSetWllq(ids = 1633, wllq = 0, type = "mc")
#' 
#' ## Reset configuration
#' options(conf_store)
#'
#' @return TRUE if successful.
#'
#' @import data.table
#' @export


tcplSetWllq <- function(ids, wllq, type) {

    if (length(wllq) > 1 | !(wllq %in% c(0:1))) {
        stop("Invalid 'wllq' value: must be of length 1 and either 0 or 1.")
    }

    if (length(type) > 1 | !(type %in% c("mc", "sc"))) {
        stop("Invalid 'type' value: ",
             "must be of length 1 and either 'mc' or 'sc.'")
    }

    id_str <- paste(ids, collapse=", ")
    tbl <- switch(type, mc="mc0",  sc="sc0")
    fld <- switch(type, mc="m0id", sc="s0id")

    qf1 <- paste("SELECT", fld, ", acid", "FROM", tbl, "WHERE", fld, "IN (%s);")
    qs1 <- sprintf(qf1, id_str)

    dat <- tcplQuery(qs1)
    miss <- !(ids %in% dat[ , get(fld)])

    if (any(miss)) {
        stop("No changes made. The following ids ",
             "are missing from the database:\n",
             paste(ids[miss], collapse="\n"))
    }

    qf2 <- paste("UPDATE", tbl, "SET wllq =", wllq, "WHERE", fld, "IN (%s);")
    qs2 <- sprintf(qf2, id_str)

    tcplSendQuery(qs2)

    tcplCascade(lvl=1, type=type, id=dat[ , unique(acid)])

    TRUE

}
