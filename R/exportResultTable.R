#####################################################################
## This program is distributed in the hope that it will be useful, ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of  ##
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    ##
## GNU General Public License for more details.                    ##
#####################################################################

#-------------------------------------------------------------------------------
# exportResultTable: create the result table for the asid in input
#-------------------------------------------------------------------------------

#' @title Create the result table for the asi in input
#' @description This function export results
#'
#' @param asid Assay source id
#' @param stats Statistics to export
#' @param outfile Path to the output file
#'
#' @details
#' This funtion is useful to export results in a table format
#'
#' @examples
#'
#' outfile <- "export_stats.csv"
#' exportResultTable(asid=1L, stats=c("modl_acc", "modl_ga"), outfile=outfile)
#'
#' @return None
#'
#' @importFrom utils write.table
#' 
#' @export
#'
exportResultTable <- function(asid, stats, outfile) {
    aetblFlds <- c("asid", "aid", "anm", "acnm", "asnm")
    aetbl<- tcplLoadAeid(fld="asid", val=asid, add.fld=aetblFlds)
    tdat <- tcplLoadData(lvl=5, fld="aeid", val=aetbl$aeid)
    chmtbl <- tcplLoadChem(field="spid", val=tdat$spid)

    dat <- merge(chmtbl, tdat, by="spid")
    dat <- merge(dat, aetbl, by="aeid")

    dat <- dat[, .SD[which.min(modl_rmse)], by=c("spid", "acnm")]
    dat[ , aenm := vapply(
        strsplit(as.character(aenm), "_"), 
        function(xx) xx[[1]], character(1)
    )]

    for(stat in stats) dat[ , (stat) := 10^get(stat)]

    dat <- dat[ , .SD,
        .SDcols=c("asnm", "chnm", "anm", "acnm", "aenm", stats)]

    write.table(dat, file=outfile, quote=TRUE, row.names=FALSE, sep=",")
}
