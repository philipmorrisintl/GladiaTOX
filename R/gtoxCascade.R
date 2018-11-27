#####################################################################
## This program is distributed in the hope that it will be useful, ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of  ##
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    ##
## GNU General Public License for more details.                    ##
#####################################################################

#-------------------------------------------------------------------------------
# gtoxCascade: Do a cascading delete on gtox screening data
#-------------------------------------------------------------------------------

#' @title Do a cascading delete on gtox screening data
#'
#' @description
#' \code{gtoxCascade} deletes the data for the given id(s) starting at
#' the processing level given. The delete will cascade through all subsequent
#' tables.
#'
#' @param lvl Integer of length 1, the first level to delete from
#' @param type Character of length 1, the data type, "sc" or "mc"
#' @param id Integer, the id(s) to delete. See details for more information.
#'
#' @details
#' The data type can be either 'mc' for mutliple concentration data, or 'sc'
#' for single concentration data. Multiple concentration data will be loaded
#' into the level tables, whereas the single concentration will be loaded into
#' the single tables.
#'
#' If lvl is less than 3, id is interpreted as acid(s) and if lvl is greater
#' than or equal to 3, id is interpreted as aeid(s).
#'
#' @note
#' This function is not exported and not intended to be used by the user.
#' 
#' @return None
#'
#' @import data.table
#' @importFrom methods is

gtoxCascade <- function(lvl, type, id) {

    stime <- Sys.time()

    if (length(lvl) > 1) {
        stop("Invalid lvl input - must be an integer of length 1.")
    }

    db <- getOption("TCPL_DB")

    if (type == "mc") {

        if (lvl == 0) {
            gtoxDelete(
                tbl="mc0", fld="acid", val=id, db=db
            )
        }
        if (lvl <= 1) {
            gtoxDelete(
                tbl="mc1", fld="acid", val=id, db=db
            )
        }
        if (lvl <= 2) {
            gtoxDelete(
                tbl="mc2", fld="acid", val=id, db=db
            )
        }
        if (lvl <  3) {
            id <- suppressWarnings(
                try(gtoxLoadAeid("acid", id)$aeid, silent=TRUE)
            )
        }
        if (is(id, "try-error")) {
            return(TRUE)
        }
        if (lvl <= 3) {
            gtoxDelete(
                tbl="mc3", fld="aeid", val=id, db=db
            )
        }
        if (lvl <= 4) {
            gtoxDelete(
                tbl="mc4", fld="aeid", val=id, db=db
            )
        }
        if (lvl <= 4) {
            gtoxDelete(
                tbl="mc4_agg", fld="aeid", val=id, db=db
            )
        }
        if (lvl <= 5) {
            gtoxDelete(
                tbl="mc5", fld="aeid", val=id, db=db
            )
        }
        if (lvl <= 6) {
            gtoxDelete(
                tbl="mc6", fld="aeid", val=id, db=db
            )
        }

    }

    if (type == "sc") {

        if (lvl == 0) {
            gtoxDelete(
                tbl="sc0", fld="acid", val=id, db=db
            )
        }
        if (lvl <  1) {
            id <- suppressWarnings(
                try(
                    gtoxLoadAeid("acid", id)$aeid, silent=TRUE
                )
            )
        }
        if (is(id, "try-error")) {
            return(TRUE)
        }
        if (lvl <= 1) {
            gtoxDelete(
                tbl="sc1", fld="aeid", val=id, db=db
            )
        }
        if (lvl <= 2) {
            gtoxDelete(
                tbl="sc2", fld="aeid", val=id, db=db
            )
        }
        if (lvl <= 2) {
            gtoxDelete(
                tbl="sc2_agg", fld="aeid", val=id, db=db
            )
        }

    }

    ttime <- round(difftime(Sys.time(), stime, units="sec"), 2)
    ttime <- paste(unclass(ttime), units(ttime))
    message(
        "Completed delete cascade for ", length(id),
        " ids (", ttime, ")\n", sep=""
    )

}

#-------------------------------------------------------------------------------
