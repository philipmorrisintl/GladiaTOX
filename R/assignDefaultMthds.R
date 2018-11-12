#####################################################################
## This program is distributed in the hope that it will be useful, ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of  ##
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    ##
## GNU General Public License for more details.                    ##
#####################################################################

#-------------------------------------------------------------------------------
# assignDefaultMthds: assign default processing methods
#-------------------------------------------------------------------------------

#' @title Assign default processing methods
#' @description Function to assign default processing method to asid in input
#' 
#' @param asid Integer, the asid value(s) to assign the default methods to
#'
#' @details
#' This function loads all components and endpoints for the given asid(s) in
#' the database, and assigns a default set of processing methods to them.
#'
#' This funciton will overwrite any previously assigned methods.
#'
#' By default, each assay will receive 'none' at level 2. Level 3 data will
#' receive, in order, 'bval.pmi' (39), 'resp.fc' (9), 'resp.log2' (7), and
#' for endpoints with "down" analysis direction, 'resp.multneg1' (6).
#'
#' @examples
#' 
#' ## Prepare for analysis before QC + process data
#' assignDefaultMthds(asid = 1L)
#' 
#' ## Process data
#' tcplRun(asid = 1L, slvl = 1, elvl = 6)
#' 
#' @return None
#' @import data.table
#' @export
#'
assignDefaultMthds <- function(asid) {

    atbl <- tcplLoadAeid(
        fld="asid",
        val=asid,
        add.fld=c("acid", "analysis_direction"))

    ## Clear all existing methods
    tcplMthdClear(lvl=2, atbl$acid, type="mc")
    tcplMthdClear(lvl=3, atbl$aeid, type="mc")
    tcplMthdClear(lvl=5, atbl$aeid, type="mc")
    tcplMthdClear(lvl=6, atbl$aeid, type="mc")

    ## Update normalized_data_type to log2_fold_induction
    atbl[ , normalized_data_type := "log2_fold_induction"]
    tcplUpdate(
        what="aeid",
        id=atbl$aeid,
        flds=atbl[ , list(normalized_data_type)])

    ## Assign level 2 methods (none for all acid values)
    tcplMthdAssign(lvl=2, id=unique(atbl$acid), mthd_id=1, ordr=1, type="mc")

    ## Assign level 3 methods
    tcplMthdAssign(
        lvl=3,
        id=atbl$aeid,
        mthd_id=c(39, 9, 7),
        ordr=seq_len(3),
        type="mc")

    tcplMthdAssign(
        lvl=3,
        id=atbl[analysis_direction == "down", aeid],
        mthd_id=6,
        ordr=4,
        type="mc")

    ## Assign level 5 methods
    tcplMthdAssign(lvl=5, id=atbl$aeid, mthd_id=9, type="mc")

    ## Assign level 6 methods
    tcplMthdAssign(
        lvl=6,
        id=atbl$aeid,
        mthd_id=c(6:8, 10:12, 15:16),
        type="mc")

}
