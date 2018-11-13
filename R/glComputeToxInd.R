#####################################################################
## This program is distributed in the hope that it will be useful, ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of  ##
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    ##
## GNU General Public License for more details.                    ##
#####################################################################

#-------------------------------------------------------------------------------
# computeToxInd: compute toxicological indicator values
#-------------------------------------------------------------------------------

#' @title Create toxicological indicator values for all chemicals in input
#' @description This function computes the toxicological indicator value for
#' the assay source id in input.
#'
#' @param asid assay source id
#' @param tp Time point to report
#' @param stat statistic to plot
#'
#' @details
#' This funtion is useful to compute toxicological indicator values. These
#' values, for each chemical, represent an average impact of the chemical
#' across the list of endpoints tested. The funtion transform the data to minus
#' log scale. Hence the larger the indicator value, larger is the impact of the
#' chemical.
#'
#' @examples
#' ## Store the current config settings, so they can be reloaded at the end
#' ## of the examples
#' conf_store <- tcplConfList()
#' tcplConfDefault()
#'
#' ## Compute toxicological severity index
#' dat <- glComputeToxInd(asid = 1L)
#' dat[]
#'
#' @return A data.table with toxicological severity index for each chemical.
#'
#' @export
#'
glComputeToxInd <- function(asid, tp=NULL, stat=quote(modl_acc)) {
    ## save data for toxPi GUI visualizatiion
    dat <- tcplLoadData(5, "aeid", tcplLoadAeid("asid", asid)$aeid)
    dat <- tcplPrepOtpt(dat)
    othrIDs <- c("asnm", "aid", "anm", "acid", "acnm")
    dat <- merge(dat,
                 tcplLoadAeid("asid", asid, add.fld=othrIDs),
                 c("aeid", "aenm"))

    dat <- dat[, .SD[which.min(modl_rmse)], by=c("spid", "acnm")]
    dat[ , aenm := vapply(strsplit(as.character(aenm), "_"), 
                          function(xx) xx[[2]], character(1))]
    xprtcols <- c("asnm", "chid", "chnm", "logc_min", "logc_max", "spid",
                  "aid", "anm", "acid", "acnm", "aeid", "aenm", "modl_ga",
                  "modl_tp", "modl_acb", "modl_acc", "fitc")
    dat <- dat[ , .SD, .SDcols=xprtcols]

    if(!is.null(tp))
        dat <- dat[grepl(paste0("_", tp), anm)]

    dat <- dat[, mean(10^eval(stat), na.rm=TRUE),
               by=c("chnm", "aenm")]

    ## log scale data
    dat$V1[dat$V1 > 1e6] <- 1e6
    dat[, V1 := (-log(V1, 10) + 6)]

    dat[, V1 := (V1 - min(V1, na.rm=TRUE))/diff(range(V1, na.rm=TRUE)),
        by="aenm"]
    dat <- dat[, mean(V1, na.rm=TRUE), by="chnm"]
    dat$chnm <- factor(dat$chnm, levels=dat$chnm[order(dat$V1,
                                                       decreasing=TRUE)])

    return(dat)
}
