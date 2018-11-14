#####################################################################
## This program is distributed in the hope that it will be useful, ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of  ##
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    ##
## GNU General Public License for more details.                    ##
#####################################################################

#-------------------------------------------------------------------------------
# tcplWriteLvl0: Write level 0 screening data into the tcpl databases
#-------------------------------------------------------------------------------

#' @title Write level 0 screening data into the tcpl databases
#'
#' @description
#' \code{tcplWriteLvl0} takes a data.table with level 0 screening data and
#' writes the data into the level 0 tables in the tcpl databases.
#'
#' @param dat data.table, the screening data to load
#' @param type Character of length 1, the data type, "sc" or "mc"
#'
#' @details
#' This function appends data onto the existing table. It also deletes all the
#' data for any acids or aeids dat contains from the given and all downstream
#' tables.
#'
#' Before writing any data the function maps the  assay component id (acid),
#' ensures the proper class on each field
#' and checks for every test compound sample id (spid where wllt == "t") in the
#' tcpl chemical database. If field types get changed a warning is given
#' listing the affected fields and they type they were coerced to. If the
#' acsn(s) or spid(s) do not map to the tcpl databases the function will return
#' an error and the data will not be written.
#'
#' The data type can be either 'mc' for mutliple concentration data, or 'sc'
#' for single concentration data. Multiple concentration data will be loaded
#' into the level tables, whereas the single concentration will be loaded into
#' the single tables.
#'
#' @note
#' This function should only be used to load level 0 data.
#'
#' @seealso \code{\link{tcplCascade}}, \code{\link{tcplAppend}}
#' 
#' @return None
#'
#' @import data.table
#' @export

tcplWriteLvl0 <- function(dat, type) {

    if (!type %in% c("mc", "sc")) stop("Invalid 'type', see help page.")

    tcplWriteData(
        dat=dat[ , list(waid, acid, rval, wllq)],
        lvl=0L,
        type=type
    )

}

#-------------------------------------------------------------------------------
