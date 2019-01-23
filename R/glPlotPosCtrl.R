#####################################################################
## This program is distributed in the hope that it will be useful, ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of  ##
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    ##
## GNU General Public License for more details.                    ##
#####################################################################

#-------------------------------------------------------------------------------
# glPlotPosCtrl: boxplot to check positive controls
#-------------------------------------------------------------------------------

#' @title Box plot for positive control check
#' @description This function plots positive controls as well as vehicel and
#' treatments normalized values
#'
#' @param asid Assay source id
#'
#' @details
#' This funtion is useful to select plates to mask
#'
#' @examples
#' ## Store the current config settings, so they can be reloaded at the end
#' ## of the examples
#' conf_store <- tcplConfList()
#' tcplConfDefault()
#'
#' ## Create boxplot for all endpoints and chemicals tested. Useful to save
#' ## plots in a pdf file.
#' pp <- glPlotPosCtrl(asid = 1L)
#' pp[[1]]
#'
#' ## Reset configuration
#' options(conf_store)
#'
#' @return A list of ggplot objects, one per assay X timepoint.
#'
#' @note PMI-specific
#' @import ggplot2
#' @importFrom tcpl tcplLoadAid tcplLoadAeid
#' 
#' @export
#'

glPlotPosCtrl <- function(asid) {
    plates <- gtoxLoadApid("aid", tcplLoadAid("asid", asid)$aid)
    plates <- plates[ , list(apid, u_boxtrack)]

    datatab <- merge(tcplLoadData(lvl=3), plates, by="apid")
    addFlds <- c("aid", "anm", "analysis_direction")
    aeidtbl <- tcplLoadAeid(fld="aeid", val=datatab$aeid, add.fld=addFlds)
    aeidtbl <- aeidtbl[analysis_direction == "up" | grepl("Cell count", aenm)]

    datatab <- merge(datatab, aeidtbl, by="aeid")

    datatab[ , aenm := vapply(
        strsplit(as.character(aenm), "_"), 
        function(xx) xx[[1]], character(1)
    )]

    pntshp <- 10^datatab$logc
    pntshp[!datatab$wllt == "c"] <- NA
    datatab$PosCtr_cntn <- as.factor(round(pntshp))

    datatab$wllt[datatab$wllt == "t"] <- "treatments"
    datatab$wllt[datatab$wllt == "c"] <- "ctrl_pos"
    datatab$wllt[datatab$wllt == "n"] <- "neutral_ctrl"

    pp <- NULL
    for(a in sort(unique(datatab$aid))) {
        pp[[a]] <-
            ggplot(
                subset(datatab, datatab$aid == a),
                aes(x=wllt, y=resp, fill=u_boxtrack)
            ) +
            geom_point(
                aes(fill=u_boxtrack, shape=PosCtr_cntn),
                col="black", size=1,
                position=position_jitterdodge(jitter.width=0.05)
            ) +
            geom_boxplot(aes(col=u_boxtrack), alpha=0, lwd=0.5) +
            scale_y_continuous(name="log2FC", limits=c(-4.5, 4.5)) +
            facet_grid(anm~aenm)
    }
    return(pp)
}
