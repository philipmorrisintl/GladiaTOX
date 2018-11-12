#####################################################################
## This program is distributed in the hope that it will be useful, ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of  ##
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    ##
## GNU General Public License for more details.                    ##
#####################################################################

#-------------------------------------------------------------------------------
# tcplPlotWin: Create winning curve plots
#-------------------------------------------------------------------------------

#' @title Create winning curve plots
#'
#' @description
#' \code{tcplPlotWin} creates the piechart plots.
#'
#' @param chid Integer of length 1, the chid value
#' @param aeid Integer, the aeid values to plot
#' @param collapse Logical, collapse the data by spid when true
#' @param bline Character of length 1, the value used for drawing the baseline
#' noise
#'
#' @details
#' When 'collapse' is TRUE the plotted points will be the mean of the values
#' based on spid.
#'
#' Any values for 'bline' other than 'coff' will use 3*bmad.
#'
#' @examples
#' 
#' chid <- tcplLoadChem(field="chnm", val="acrylamide", include.spid=FALSE)$chid
#' print(chid)
#' aeid <- tcplLoadAeid(fld=c("asid","aenm"), 
#'           val=list(1L, "GSH content_GSH content_4h_dn"), add.fld="asid")$aeid
#' print(aeid)
#' tcplPlotWin(chid = chid, aeid = aeid, bline="bmad", collapse=TRUE)
#' 
#' @return None
#'
#' @importFrom stats sd
#' @importFrom RColorBrewer brewer.pal
#' @importFrom grDevices colorRampPalette
#' @import data.table
#' @importFrom graphics text polygon legend lines axis par strwidth plot.new
#' @importFrom graphics plot.window
#' @export

tcplPlotWin <- function(chid, aeid, bline="bmad", collapse=TRUE) {

    ## Get data type, and set default range
    data_type <- tcplLoadAeid("aeid", aeid, add.fld="normalized_data_type")
    data_type <- data_type[ , unique(normalized_data_type)]
    if (length(data_type) > 1) {
        stop("This function does not currently support plotting multiple ",
             "scales on the same plot.")
    }

    if (is.na(data_type)) data_type <- ""

    if (data_type == "percent_activity") {
        y0 <- c(-50, 150)
        ylab <- "Percent Activity"
    } else {
        if (data_type == "log2_fold_induction") {
            y0 <- c(-1, 4)
            ylab <- "Log2(Fold Induction)"
        } else {
            if (data_type == "log10_fold_induction") {
                y0 <- c(-0.1, 2)
                ylab <- "Log10(Fold Induction)"
            } else {
                warning("Data scale not recognized. ",
                        "Default range set to -50:150.")
                y0 <- c(-50, 150)
                ylab <- "Activity"
            }
        }
    }

    ## Load in concentration/response data and the fit data; mult by -1 for
    ## endpoints analyzed in the down direction
    smp <- tcplLoadChem(field="chid", val=chid)$spid
    aes <- tcplLoadAeid(
        fld="aeid", val=aeid, add.fld="analysis_direction")
    rsp <- tcplLoadData(lvl=3, fld=c("spid" ,"aeid"), val=list(smp, aeid))
    sub <- tcplLoadData(lvl=5, fld=c("spid" ,"aeid"), val=list(smp, aeid))
    plt <- tcplLoadApid(
        fld="aid", val=tcplLoadAeid(fld="aeid", val=aeid,
                                        add.fld="aid")$aid)
    rsp=merge(rsp, plt, by="apid")
    sub=merge(sub, unique(rsp[,list(spid,u_boxtrack)]), by="spid")

    setkey(sub, spid, modl_rmse)
    setkey(rsp, spid)
    min_rmse <- sub[ , list(ind=.I[1]), by="spid"]
    sub <- sub[min_rmse$ind]
    neg_aes <- aes[analysis_direction == "down", aeid]
    sub[ , adir := ifelse(aeid %in% neg_aes, "down", "up")]
    rsp[ , true_resp := resp]
    rsp[aeid %in% neg_aes, true_resp := resp*-1]

    if (collapse) {
        rsp <- rsp[ ,
                   list(true_resp=mean(true_resp)),
                   by=c("aeid", "logc", "spid")]
    }

    colfunc <- colorRampPalette(brewer.pal(n=9, name="Greens")[-c(seq_len(3))])
    grns <- colfunc(rsp[ , lu(spid)])
    p <- list(ylim=range(rsp$true_resp*1.2, y0),
              font.lab=2,
              col="black",
              cex=2,
              xlab=expression(bold(paste("Concentration (",mu,"M)"))),
              ylab=ylab,
              main="",
              bty="n",
              xaxt="n",
              yaxt="n",
              type="n")
    par(mar=c(4, 4, 1, 1) + 0.1)
    do.call(what=plot, args=c(rsp$true_resp ~ rsp$logc, p), quote=TRUE)
    useBmad <- bline != "coff"
    rect(xleft=par()$usr[1],
         xright=par()$usr[2],
         ybottom=-1*ifelse(useBmad, 3*sub[ , max(bmad)], sub[ , max(coff)]),
         ytop=ifelse(useBmad, 3*sub[ , max(bmad)], sub[ , max(coff)]),
         border=NA,
         col="gray70",
         density=15,
         angle=ifelse(useBmad, 45, -45))
    points(rsp$true_resp ~ rsp$logc,
           col=grns[as.factor(rsp[ , spid])])
    for (i in seq_len(nrow(sub))) {
        tcplAddModel(sub[i],
                     adj=switch(sub[i, adir], down=-1, 1),
                     col=grns[as.factor(sub$spid)[i]])
    }
    legend(x=ifelse(which.max(abs(par()$usr[3:4])) == 2, "topleft",
                      "bottomleft"),
           legend=paste0(sub$spid, " (", sub$modl, ")",
                           " - ", sub$u_boxtrack),
           col=grns[as.factor(sub$spid)],
           bty="n",
           pch=1,
           cex=0.5)
    axis(side=1,
         at=axTicks(side=1),
         labels=signif(10^axTicks(side=1), digits=2),
         font=1,
         lwd=2,
         col="gray35")
    axis(side=2,
         at=axTicks(side=2),
         labels=axTicks(side=2),
         font=1,
         lwd=2,
         col="gray35")

}
