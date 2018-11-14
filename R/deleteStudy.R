#####################################################################
## This program is distributed in the hope that it will be useful, ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of  ##
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    ##
## GNU General Public License for more details.                    ##
#####################################################################

#-------------------------------------------------------------------------------
# .deleteStudy: Completely remove all data for a study  
#-------------------------------------------------------------------------------

#' @title Completely remove all data for a study  
#' 
#' @description
#' \code{.deleteStudy} completely removes all data for a study from the 
#' database.
#' 
#' @param asid The assay source/study ID
#' @param db   (optional) the databse to delete from, defaults to the current
#' database settings
#' 
#' @details 
#' Cannot be undone. Please use carefully. Not exported, as this is
#' intended for development and should not be used with real data.
#' 
#' @note 
#' PMI-specific
#' 
#' @import data.table

.deleteStudy <- function(asid, db=NULL) {

    stopifnot(is.null(db) || (length(db) == 1 & is.character(db)))
    stopifnot(is.numeric(asid))

    prmpt <- sprintf("Delete all references to asid %s? [y/n] ", asid)
    delete <- readline(prompt=prmpt)
    if (delete != "y") return()

    ids <- tcplLoadAcid(fld="asid", val=asid, add.fld=c("aid", "aeid"))
    plt <- tcplLoadApid(fld="aid", val=unique(ids$aid))
    wll <- tcplLoadWaid(fld="apid", val=(plt$apid))
    tcplCascade(lvl=0L, type="mc", id=ids$acid)
    tcplCascade(lvl=0L, type="sc", id=ids$acid)
    tcplDelete(
        tbl="assay_source",
        fld="asid",
        val=asid,
        db=getOption("TCPL_DB")
    )
    tcplDelete(
        tbl="assay",
        fld="aid",
        val=unique(ids$aid),
        db=getOption("TCPL_DB")
    )
    tcplDelete(
        tbl="assay_component",
        fld="acid",
        val=unique(ids$acid),
        db= getOption("TCPL_DB")
    )
    tcplDelete(
        tbl="assay_compnent_endpoint",
        fld="aeid",
        val=unique(ids$aeid),
        db=getOption("TCPL_DB")
    )
    tcplDelete(
        tbl="assay_plate",
        fld="aid",
        val=unique(ids$aid),
        db=getOption("TCPL_DB")
    )
    tcplDelete(
        tbl="assay_plate_well",
        fld="apid",
        val=unique(plt$apid),
        db=getOption("TCPL_DB")
    )
    tcplDelete(
        tbl="bb_apid_map",
        fld="apid",
        val=unique(plt$apid),
        db=getOption("TCPL_DB")
    )
    tcplDelete(
        tbl="bb_waid_map",
        fld="waid",
        val=unique(wll$waid),
        db=getOption("TCPL_DB")
    )
}

#-------------------------------------------------------------------------------
