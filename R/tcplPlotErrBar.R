#####################################################################
## This program is distributed in the hope that it will be useful, ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of  ##
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    ##
## GNU General Public License for more details.                    ##
#####################################################################

#-------------------------------------------------------------------------------
# tcplPlotErrBar: Create error bar plots
#-------------------------------------------------------------------------------

#' @title Create error bar plots
#'
#' @description
#' \code{tcplPlotErrBar} creates the error bar plots.
#'
#' @param c1 Integer of length 1, the chid value for the first chemical
#' @param c2 Integer of length 1, the chid value for the first chemical
#' @param aeid Integer, the aeid value(s) to plot
#' @param ngrp Integer, the number of "slots" to draw; overridden if the
#' number of aeid values is greater than 'ngrp'
#'
#' @return None
#'
#' @importFrom stats sd
#' @importFrom RColorBrewer brewer.pal
#' @importFrom grDevices colorRampPalette
#' @import data.table
#' @importFrom graphics text abline points legend lines axis par strwidth
#' @export

tcplPlotErrBar <- function(c1, c2, aeid, ngrp=NULL) {

    ## Load necessary data
    cd <- tcplLoadChem(field="chid", val=c(c1, c2))
    dat <- tcplLoadData(
        lvl=3,
        fld=c("spid", "aeid"),
        val=list(cd$spid, aeid)
    )
    if (nrow(dat) == 0) {
        stop("No data for the given inputs.")
    }
    dat <- tcplPrepOtpt(dat)

    ## Get data type, and set default range
    data_type <- tcplLoadAeid("aeid", aeid, add.fld="normalized_data_type")
    data_type <- data_type[ , unique(normalized_data_type)]
    if (length(data_type) > 1) {
        stop(
            "This function does not currently support plotting multiple ",
            "scales on the same plot."
        )
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
                warning(
                    "Data scale not recognized. ",
                    "Default range set to -50:150."
                )
                y0 <- c(-50, 150)
                ylab <- "Activity"
            }
        }
    }

    ## Round logc to 3 sig figs
    dat[ , logc := signif(logc, 3)]

    ## Compress data by sample ID with mean
    m1 <- dat[ ,
        list(mean_resp=mean(resp)),
        by=list(aeid, aenm, chid, chnm, spid, logc)]
    m2 <-  m1[ ,
        list(
            mom=mean(mean_resp),
            sem=sd(mean_resp)/sqrt(.N)
        ),
        by=list(aeid, aenm, chid, chnm, logc)]

    ## Create the transformed concentration values
    m2[ , tc := (logc - min(logc))/max(logc - min(logc))*0.7 + 0.15]
    m2[ , tc := tc + .GRP - 1, by=list(aeid, aenm)]

    ## Define colors
    rf <- colorRampPalette(brewer.pal(n=9, name="Reds")[-1])
    bf <- colorRampPalette(brewer.pal(n=9, name="Blues")[-1])
    setkey(m2, logc)
    m2[chid == c1, col := rf(m2[chid == c1, lu(logc)])[.GRP], by=logc]
    m2[chid == c2, col := bf(m2[chid == c2, lu(logc)])[.GRP], by=logc]

    grps <- m2[ , .GRP, by=list(aeid, aenm)]

    p <- list(
        ylim=range((m2$mom + m2$sem)*1.2,
        (m2$mom - m2$sem)*1.2,
        y0, na.rm=TRUE),
        xlim=c(0, max(grps[ , max(GRP) + 0], ngrp)),
        font.lab=2,
        col="black",
        cex=2,
        xlab="",
        ylab=ylab,
        main="",
        bty="n",
        xaxt="n",
        yaxt="n",
        type="n"
    )
    par(mar=c(4, 4, 1, 1))
    do.call(what=plot, args=c(m2$mom ~ m2$tc, p), quote=TRUE)
    abline(v=grps[ , GRP], lty="dashed", col="gray50")

    xusr <- par("usr")
    xw <- max(strwidth(grps[ , aenm], cex=par("cex")))
    yh <- xw/(xusr[2] - xusr[1])*par("pin")[1]
    yh <- yh/par("pin")[2] * (xusr[4] - xusr[3])

    text(
        x=grps[ , GRP], y=mean(par()$usr[3:4]),
        labels=grps[ , aenm],
        adj=c(0.5, -0.5),
        srt=90,
        cex=min(diff(par("usr")[3:4])/yh*0.8, 0.75),
        font=2
    )
    points(m2$mom ~ m2$tc, col=m2$col)
    w <- strwidth("o")/2
    for (i in seq_len(nrow(m2))) {
        lines(
            x=rep(m2[i, tc], 2),
            y=m2[i, c(mom - sem, mom + sem)],
            col=m2[i, col]
        )
        lines(
            x=m2[i, c(tc - w, tc + w)],
            y=rep(m2[i, mom + sem], 2),
            col=m2[i, col]
        )
        lines(
            x=m2[i, c(tc - w, tc + w)],
            y=rep(m2[i, mom - sem], 2),
            col=m2[i, col]
        )
    }

    ltxt <- c(
        paste0(
            cd[chid == c1, unique(chnm)],
            " [",
            paste(
                m2[chid == c1, round(10^range(logc), 3)],
                collapse=", "
            ),
            "]"
        ),
        paste0(
            cd[chid == c2, unique(chnm)],
            " [",
            paste(
                m2[chid == c2, round(10^range(logc), 3)],
                collapse=", "
            ),
            "]"))
    
    legend(
        x=mean(par()$usr[seq_len(2)]),
        y=.line2user(1, 1),
        legend=ltxt,
        fill=c(rf(10)[5], bf(10)[5]),
        bty="n",
        horiz=TRUE,
        xjust=0.5,
        xpd=TRUE
    )
    axis(
        side=2,
        at=axTicks(side=2),
        labels=axTicks(side=2),
        font=1,
        lwd=2,
        col="gray35"
    )
    
}
