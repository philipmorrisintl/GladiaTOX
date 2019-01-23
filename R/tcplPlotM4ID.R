#####################################################################
## This program is distributed in the hope that it will be useful, ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of  ##
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    ##
## GNU General Public License for more details.                    ##
#####################################################################

#-------------------------------------------------------------------------------
# tcplPlotM4ID: Plot dose-response by m4id
#-------------------------------------------------------------------------------

#' @title Plot fit summary plot by m4id
#'
#' @description
#' \code{tcplPlotM4ID} creates a summary plots for the given m4id(s) by loading
#' the appropriate data from the gtox databases and sending it to
#' \code{\link{tcplPlotFits}}
#'
#' @param m4id Integer, m4id(s) to plot
#' @param lvl Integer, the level of data to plot
#' @param bline Character of length 1, the value used for drawing the baseline
#' noise
#'
#' @details
#' A level 4 plot ('lvl' = 4) will plot the concentration series and the
#' applicable curves, without an indication of the activity call or the
#' winning model. Level 4 plots can be created without having done subsequent
#' processing.
#'
#' Level 5 plots include the level 4 information with the activity call and
#' model selection. The winning model will be highlighted red in the side panel
#' containing the summary statistics. Level 6 plots, in addition the all of the
#' level 4 and 5 information, include the positive flag IDs. If the flag has
#' an associated value, the value will be in parentheses follwing the flag ID.
#'
#' Any values for 'bline' other than 'coff' will use 3*bmad.
#'
#' @examples
#' ## Store the current config settings, so they can be reloaded at the end
#' ## of the examples
#' conf_store <- tcplConfList()
#' tcplConfDefault()
#' 
#' acnm <- "Cytotoxicity (TIER1)_Cytochrome C release_24h"
#' pltnm <- "S-000049119"
#' myaid <- gtoxLoadApid()[u_boxtrack == pltnm, aid]
#' myaid <- myaid[myaid%in%tcplLoadAid(fld = "asid", val = 1L)$aid]
#' apid <- gtoxLoadApid()[u_boxtrack == pltnm & aid == myaid, apid]
#' acid <- tcplLoadAcid(fld = c("aid", "acnm"), val = list(myaid, acnm))[, acid]
#' 
#' aeid = tcplLoadAeid(fld = c("acid", "analysis_direction"), 
#'                     val = list(acid, "up"))[,aeid]
#' spid = gtoxLoadWaid(fld = c("apid", "wllt"), 
#'                     val = list(apid, "c"))[,unique(spid)]
#' m4id = tcplLoadData(lvl = 4L, fld = c("spid", "aeid"), 
#'                     val = list(spid, aeid))[, m4id]
#' 
#' tcplPlotM4ID(m4id = m4id, lvl = 6, bline = "coff") ## Create a level 4 plot
#' tcplPlotM4ID(m4id = m4id, lvl = 5) ## Create a level 5 plot
#' tcplPlotM4ID(m4id = m4id, lvl = 6) ## Create a level 6 plot
#' 
#' ## Reset configuration
#' options(conf_store)
#'
#' @seealso \code{\link{tcplPlotFits}}, \code{\link{tcplMakeAeidPlts}}
#' 
#' @return None
#' 
#' @import data.table
#' @importFrom tcpl tcplConfList
#' @export


tcplPlotM4ID <- function(m4id, lvl=4L, bline="bmad") {

    if (length(lvl) > 1 | !lvl %in% 4:6) stop("invalid lvl input.")

    prs <- list(type="mc", fld="m4id", val=m4id)

    if (lvl == 4L) dat <- do.call(tcplLoadData, args=c(lvl=4L, prs))
    if (lvl >= 5L) dat <- do.call(tcplLoadData, args=c(lvl=5L, prs))
    if (lvl == 6L) {
        flg <- do.call(tcplLoadData, args=c(lvl=6L, prs))
    } else {
        flg <- NULL
    }

    if (nrow(dat) == 0) stop("No data for m4id ", m4id)

    agg <- do.call(tcplLoadData, args=c(lvl="agg", prs))

    tcplPlotFits(dat=dat, agg=agg, flg=flg, bline=bline)

}

#-------------------------------------------------------------------------------
