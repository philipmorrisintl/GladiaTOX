#####################################################################
## This program is distributed in the hope that it will be useful, ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of  ##
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    ##
## GNU General Public License for more details.                    ##
#####################################################################

#-------------------------------------------------------------------------------
# glPlotPie: Assay pie chart
#-------------------------------------------------------------------------------

#' @title Pie chart for Minimal Effective Concentrations (MEC) and AC50 plot
#' @description This function plots MEC values
#'
#' @param asid Assay source id
#' @param chnms Character vector with list of chemical names
#' @param acids Numeric vector with list of acids
#' @param aeids Character vector with list of assay endpoints IDs
#' @param expos.time.ordr Character vector with sorted list of exposure times
#' @param stat Statistic to plot (e.g. MEC:modl_acc or modl_acb, AC50:modl_ga)
#'
#' @details
#' This funtion is useful to plot MEC or AC50 values
#'
#' @examples
#'
#' ## Create a pie plot of MEC values for all chemicals tested in the study
#' glPlotPie(asid=1L)
#' 
#' @return None
#'
#' @import ggplot2
#' @export
#'
glPlotPie <- function(asid, chnms=NULL, acids=NULL, aeids=NULL,
                      expos.time.ordr=NULL, stat=quote(modl_acc)){
    ## load study annotations
    t1 <- tcplLoadAsid(fld="asid", val=asid)
    t2 <- tcplLoadAid(fld="asid", val=asid)
    t3 <- tcplLoadAcid(fld="aid", val=t2$aid)
    t4 <- tcplLoadAeid(fld="acid", val=t3$acid)
    annotations <- merge(merge(
        merge(t1, t2, by="asid"), t3, by="aid"), t4, by="acid")
    ## filter for aeids in input
    if(!is.null(aeids))
        annotations <- annotations[aeid%in%aeids]

    ## filter for acids in input
    if(!is.null(acids))
        annotations <- annotations[acid%in%acids]

    ## load study data
    t1 <- tcplLoadData(lvl=4L, fld="aeid", val=annotations$aeid)
    t2 <- tcplLoadData(lvl=5L, fld="aeid", val=annotations$aeid)
    t3 <- tcplLoadChem()
    t4 <- unique(tcplLoadWaid()[, c("apid", "spid"), with=FALSE])
    data <- merge(
        merge(t1, t2, by=intersect(colnames(t1), colnames(t2))),
        merge(t3, t4, by="spid"), by="spid")

    ## filter for chemical names
    if(!is.null(chnms))
        data=data[chnm%in%chnms]

                                        # merge annotations and data
    dat <- merge(annotations, data, by="aeid")

    ## strip endpoint names
    dat[ , aenm := vapply(strsplit(as.character(aenm), "_"), 
                          function(xx) xx[[2]], character(1))]

    ## select samples based on min rmse + filter columns
    dat <- dat[, .SD[which.min(modl_rmse)], by=c("spid", "acnm")]

    ## extract exposure time
    dat[ , expTm := unlist(
               lapply(
                   dat$anm,
                   function(xx) {
                       strsplit(xx, "_")[[1]][2]
                   }
               )
           )]
    if(!is.null(expos.time.ordr)){
        dat <- dat[dat$expTm%in%expos.time.ordr, ]
        dat$expTm <- factor(dat$expTm, levels=expos.time.ordr)
    }

    ## extract statistical values to plot and cast to text
    dat[, stat_val := mean(10^eval(stat), na.rm=TRUE),
        by=c("chnm", "aenm", "expTm")]
    dat[, stat_text := stat_val]
    dat[, stat_val := abs(stat_val - max(stat_val, na.rm=TRUE)) +
              min(stat_val, na.rm=TRUE)]

    ## y intercepts for horizontal grey lines
    yintercept <- seq((min(dat$stat_val, na.rm=TRUE)),
    (max(dat$stat_val, na.rm=TRUE)), length.out=7)

    print(
        ggplot(dat, aes(x=aenm, y=stat_val, fill=aenm)) +
        geom_bar(stat="identity", position=position_dodge(width=0.9),
                 width=1, color="gray60") +
        geom_hline(color="gray", yintercept=yintercept, size=.3,
                   linetype="dashed") +
        scale_fill_brewer(palette="Pastel1") +
        theme_minimal() +
        theme(
            legend.position="bottom",
            panel.grid.major.y=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            legend.text=element_text(size=10),
            legend.title=element_blank(),
            strip.text.x=element_text(size=12),
            strip.text.y=element_text(size=12),
            axis.ticks.x=element_blank(),
            axis.text.x=element_blank()) +
        guides(fill=guide_legend(nrow=4, byrow=TRUE)) +
        geom_text(
            aes(label=signif(stat_text, 3), y=max(stat_val, na.rm=TRUE)),
            color="black", vjust=-0.5, size=3) +
        facet_grid(expTm ~ chnm) +
        coord_polar()
    )
}
