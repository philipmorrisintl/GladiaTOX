#####################################################################
## This program is distributed in the hope that it will be useful, ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of  ##
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    ##
## GNU General Public License for more details.                    ##
#####################################################################

#-------------------------------------------------------------------------------
# exportResultForToxpiGUI: create the result table compatible to toxpi GUI
#-------------------------------------------------------------------------------

#' @title Create the result table for the asi in input
#' @description This function export results
#'
#' @param asid Assay source id
#' @param tp Time point
#' @param outfile Path to the output file
#' @param stat Character vector of statistic to export
#'
#' @details
#' This funtion is useful to export results in a table format
#'
#' @examples
#' ## Export MEC (or AC50) values to be visualized in ToxPiGUI
#' conf_store <- tcplConfList()
#' tcplConfDefault()
#'
#' out <- "export_for_toxpiGUI.csv"
#' exportResultForToxpiGUI(asid=1L, tp="4h", outfile=out, stat=quote(modl_acc))
#'
#' ## Reset configuration
#' options(conf_store)
#' 
#' @return None
#'
#' @importFrom tidyr spread
#' @importFrom RColorBrewer brewer.pal
#' @importFrom utils write.table
#' @importFrom tcpl tcplLoadAeid tcplPrepOtpt
#'
#' @export
#'
exportResultForToxpiGUI <- function(asid, tp, outfile, stat) {
    ## save data for toxPi GUI visualizatiion
    dat <- tcplLoadData(lvl=5, fld="aeid", val=tcplLoadAeid("asid", asid)$aeid)
    dat <- tcplPrepOtpt(dat)
    othrIDs <- c("asnm", "aid", "anm", "acid", "acnm")
    dat <- merge(
        dat,
        tcplLoadAeid("asid", asid, add.fld=othrIDs),
        c("aeid", "aenm")
    )

    dat <- dat[, .SD[which.min(modl_rmse)], by=c("spid", "acnm")]
    dat[ , aenm := vapply(
        strsplit(as.character(aenm), "_"), 
        function(xx) xx[[1]], character(1)
    )]
    xprtcols <- c(
        "asnm", "chid", "chnm", "logc_min", "logc_max", "spid",
        "aid", "anm", "acid", "acnm", "aeid", "aenm", "modl_ga",
        "modl_tp", "modl_acb", "modl_acc", "fitc"
    )
    dat <- dat[ , .SD, .SDcols=xprtcols]

    ## preapre slice info
    tf <- grepl(paste0("_", tp), dat$anm)
    endpoints <- sort(unique(getsplit(dat$acnm, "_", seq_len(2))[tf]))
    slices <- c(endpoints[!grepl("Cell count", endpoints)], "Cell count")
    slices1 <- getsplit(slices, "_", 1)
    cnts <- table(slices1)[match(slices1, names(table(slices1)))]
    nslices <- sum(slices %in% endpoints)
    mat.rows <- c(
        seq_len(nslices),
        rep(nslices + 1, sum(grepl("Cell count", endpoints)))
    )
    mat.cols <- c(
        match(slices[slices %in% endpoints], endpoints),
        which(grepl("Cell count", endpoints))
    )
    pmat <- matrix("", length(slices), length(endpoints))
    pmat[cbind(mat.rows, mat.cols)] <- "x"
    n <- cnts[!duplicated(names(cnts))]
    name <- c(
        "YlGn", "Reds", "Purples", "YlGnBu", "Blues", "Greys", "BuPu",
        "Oranges", "BuGn"
    )[seq_len(length(n))]
    colFunc <- function(xx, yy) brewer.pal(n=9, name=yy)[3:(xx + 2)]
    colors <- as.character(unlist(mapply(colFunc, n, name)))
    slices_text <- sprintf("# %s!%s!%s!-ln(x)", slices, round(25/cnts), colors)
    slices_tab <- cbind(slices_text, matrix("", length(slices), 3), pmat)
    ## prepare data
    tmat <- dat[tf, .SD, .SDcols=c("chnm", "acnm", as.character(stat))]
    unLogMean <- function(x) mean(10^x, na.rm=TRUE)
    ## Should be able to replace the next lines with...
    tmat <- tmat[, lapply(.SD, unLogMean), by=.(chnm, acnm)]
    colnames(tmat)=c("chnm", "acnm", "stat")
    mat <- spread(tmat, acnm, stat)
    ## something like dcast(dat, acnm ~ chnm, unLogMean)
    rownames <- mat$chnm
    mat <- data.matrix(as.matrix(mat)[, 2:ncol(mat)])
    colnames <- getsplit(colnames(mat), "_", seq_len(2))
    mat <- matrix(as.numeric(mat), nrow(mat), ncol(mat))
    rownames(mat)=rownames
    colnames(mat)=colnames
    mat <- mat[, match(colnames, endpoints)]
    col.max <- apply(mat, 2, function(xx) max(xx, na.rm=TRUE))
    for (kk in seq_len(ncol(mat))) {
        mat[is.na(mat[, kk]), kk] <- col.max[kk]
    }
    mat <- cbind(
        data.frame(
            Row=seq_len(nrow(mat)),
            Source=paste0("source", seq_len(nrow(mat))),
            CASRN=NA,
            Name=rownames(mat)
        ),
        mat
    )
    ## write info on file
    write.table(
        slices_tab, file=outfile, quote=TRUE, row.names=FALSE,
        col.names=FALSE, sep=","
    )
    write.table(
        mat,file=outfile, quote=TRUE, row.names=FALSE,
        sep=",", append=TRUE
    )
}
