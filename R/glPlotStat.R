#####################################################################
## This program is distributed in the hope that it will be useful, ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of  ##
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    ##
## GNU General Public License for more details.                    ##
#####################################################################

#-------------------------------------------------------------------------------
# glPlotStat: boxplot to check positive controls
#-------------------------------------------------------------------------------

#' @title Box plot for Minimal Effective Concentrations (MEC) and AC50 plot
#' @description This function plots MEC values
#'
#' @param asid Assay source id
#' @param ref.chm Chemical to adopt as reference
#' @param stat Character vector of statistic to export
#'
#' @details
#' This funtion is useful to show the MEC trend over control chemical
#'
#' @examples
#' ## Store the current config settings, so they can be reloaded at the end
#' ## of the examples
#' conf_store <- tcplConfList()
#' tcplConfDefault()
#'
#' ## Create boxplot of MEC
#' ## plots in a pdf file.
#' pp <- glPlotStat(asid = 1L)
#' pp[[1]]
#'
#' ## Reset configuration
#' options(conf_store)
#'
#' @return A list of ggplot objects, one per assay X timepoint.
#'
#' @import ggplot2
#' @importFrom stringr str_wrap
#' @export
#'
glPlotStat <- function(asid, ref.chm=NULL, stat=quote(modl_acc)) {

    addFlds <- c("asid", "aid", "anm", "acnm")
    aetbl <- tcplLoadAeid(fld="asid", val=asid, add.fld=addFlds)
    dat <- tcplPrepOtpt(tcplLoadData(lvl=5, fld="aeid", val=aetbl$aeid))
    dat <- merge(dat, aetbl, by=c("aeid", "aenm"))
    dat <- dat[ , .SD[which.min(modl_rmse)], by=c("spid", "acnm")]
    dat[ , aenm := vapply(
        strsplit(as.character(aenm), "_"), 
        function(xx) xx[[2]], character(1)
    )]
    dat$aenm_wrap <- str_wrap(string=dat$aenm, width=15)

    dat[ , stat := 10^eval(stat)]

    if (stat=="modl_acb" | stat=="modl_acc") {
        yname <- "Minimal Effective Concentration"
        stnm <- "MEC"
    }
    if (stat=="modl_ga") {
        yname <- "AC50"
        stnm <- "AC50"
    }

    dat$chid <- as.factor(dat$chid)
    dat$aeid <- as.factor(dat$aeid)

    dat$chnm <- factor(
        dat$chnm,
        levels=c(
            ref.chm,
            unique(dat$chnm)[!unique(dat$chnm) %in% ref.chm]
        )
    )
    pp <- NULL
    for(a in sort(unique(dat$aid))) {
        from <- min(unique(dat$logc_min[dat$aid %in% a]))
        to <- max(unique(dat$logc_max[dat$aid %in% a]))
        breaks <- formatC(
            round(10^seq(from, to, by=(to-from)/10), 3),
            mode="real"
        )

        pp[[a]] <-
            ggplot(subset(dat, dat$aid == a), aes(x=chnm, y=stat, col=chnm)) +
            geom_point(aes(col=chnm), na.rm=TRUE) +
            geom_boxplot(alpha=0, lwd=0.2) +
            scale_y_log10(
                name=yname, breaks=as.numeric(breaks), labels=breaks
            ) +
            theme_bw() +
            annotation_logticks(sides="l") +
            theme(
                axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
                axis.title.x=element_blank()
            ) +
            guides(col=guide_legend(title="Chemical")) +
            facet_grid(anm~aenm_wrap)
    }
    return(pp)
}
