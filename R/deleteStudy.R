#####################################################################
## This program is distributed in the hope that it will be useful, ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of  ##
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    ##
## GNU General Public License for more details.                    ##
#####################################################################

#-------------------------------------------------------------------------------
# deleteStudy: Completely remove all data for a study  
#-------------------------------------------------------------------------------

#' @title Completely remove all data for a study  
#' 
#' @description
#' \code{deleteStudy} completely removes all data for a study from the 
#' database.
#' 
#' @param asid The assay source/study ID
#' @param db   (optional) the databse to delete from, defaults to the current
#' database settings
#' 
#' @examples
#' 
#' \dontrun{
#' ## Load sample data
#' load(system.file("extdata", "data_for_vignette.rda", package="GladiaTOX"))
#' 
#' ## Build assay table
#' assay <- buildAssayTab(plate, chnmap)
#' 
#' ## Set study parameters
#' std.nm <- "SampleStudy" # study name
#' phs.nm <- "PhaseII" # study phase
#' 
#' ## Load annotation in gtoxDB
#' loadAnnot(plate, assay, NULL)
#' 
#' ## Delete previously loaded study data
#' asid = gtoxLoadAsid(fld=c("asnm", "asph"), val=list(std.nm, phs.nm))$asid
#' if(length(asid)>0){ deleteStudy(asid=asid) }
#' }
#' 
#' @details 
#' Cannot be undone. Please use carefully. Not exported, as this is
#' intended for development and should not be used with real data.
#' 
#' @import data.table
#' @export
#' 
#' @return None

deleteStudy <- function(asid, db=NULL) {

    stopifnot(is.null(db) || (length(db) == 1 & is.character(db)))
    stopifnot(is.numeric(asid))

    ids <- gtoxLoadAcid(fld="asid", val=asid, add.fld=c("aid", "aeid"))
    plt <- gtoxLoadApid(fld="aid", val=unique(ids$aid))
    wll <- gtoxLoadWaid(fld="apid", val=(plt$apid))
    gtoxCascade(lvl=0L, type="mc", id=ids$acid)
    gtoxCascade(lvl=0L, type="sc", id=ids$acid)
    gtoxDelete(
        tbl="assay_source",
        fld="asid",
        val=asid,
        db=getOption("TCPL_DB")
    )
    gtoxDelete(
        tbl="assay",
        fld="aid",
        val=unique(ids$aid),
        db=getOption("TCPL_DB")
    )
    gtoxDelete(
        tbl="assay_component",
        fld="acid",
        val=unique(ids$acid),
        db= getOption("TCPL_DB")
    )
    gtoxDelete(
        tbl="assay_compnent_endpoint",
        fld="aeid",
        val=unique(ids$aeid),
        db=getOption("TCPL_DB")
    )
    gtoxDelete(
        tbl="assay_plate",
        fld="aid",
        val=unique(ids$aid),
        db=getOption("TCPL_DB")
    )
    gtoxDelete(
        tbl="assay_plate_well",
        fld="apid",
        val=unique(plt$apid),
        db=getOption("TCPL_DB")
    )
    gtoxDelete(
        tbl="bb_apid_map",
        fld="apid",
        val=unique(plt$apid),
        db=getOption("TCPL_DB")
    )
    gtoxDelete(
        tbl="bb_waid_map",
        fld="waid",
        val=unique(wll$waid),
        db=getOption("TCPL_DB")
    )
}

#-------------------------------------------------------------------------------
