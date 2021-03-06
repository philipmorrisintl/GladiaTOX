#####################################################################
## This program is distributed in the hope that it will be useful, ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of  ##
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    ##
## GNU General Public License for more details.                    ##
#####################################################################

#-------------------------------------------------------------------------------
# plateHeat: create a plot showing the assay plate data
#-------------------------------------------------------------------------------

#' @title Plot plate heatmap
#'
#' @description Plot plate heatmap, to be used with gtoxPlotPlate
#'
#' @param vals Numeric, the well values
#' @param rowi Integer, the row index
#' @param coli Integer, the column index
#' @param wllt Character, the well type
#' @param wllq Logical, the well quality
#' @param rown Integer, the number of rows on the plate
#' @param coln Integer, the number of columns on the plate
#' @param main Character of length 1, the title/main
#' @param arng Numeric of length 2, the minimum and maximum values to constrain
#' the color scale
#'
#' @note
#' Optimized for an output with height = 8, width = 12, and
#' pointsize = 12
#'
#' @importFrom RColorBrewer brewer.pal
#' @importFrom graphics par layout plot.new plot.window text box abline axis
#' @importFrom graphics lines rect
#' @importFrom grDevices colorRampPalette
#' @importFrom stats quantile
#' 
#' @keywords internal
#' 
#' @return None

.plateHeat <- function(vals, rowi, coli, wllt, wllq, rown, coln, main, arng) {

    opar <- par()[c("pty", "mar", "mai", "family")]
    on.exit(par(opar))
    par(pty="m", family="mono")

    myPal <- colorRampPalette(rev(brewer.pal(11, "Spectral")), space="Lab")

    outside <- vals < arng[1] | vals > arng[2]
    outside[is.na(outside)] <- TRUE
    vals[vals < arng[1]] <- arng[1]
    vals[vals > arng[2]] <- arng[2]
    badwlls <- cbind(rowi[!wllq], coli[!wllq])

    layout(
        matrix(c(1, 1, 2, 3), ncol=2, byrow=TRUE),
        widths=c(9, 1.5),
        heights=c(1, 9)
    )

    par(mar=c(0, 0, 0, 0), mai=c(0, 0, 0, 0), family="mono")
    plot.new()
    plot.window(
        xlim=0:1,
        ylim=0:1
    )
    text(0.5, 0.5, main, font=2, cex=1.5)

    plot.new()
    par(mar=c(2, 3, 2, 0) + 0.1, family="mono")
    plot.window(
        xlim=c(0, coln) + 0.5,
        ylim=rev(c(0, rown) + 0.5),
        xaxs="i",
        yaxs="i"
    )
    box(which="plot")
    abline(v=seq_len((coln - 1)) + 0.5)
    abline(h=seq_len((rown - 1)) + 0.5)
    axis(
        side=3,
        at=seq_len(coln),
        tick=FALSE,
        labels=sprintf("%02d", seq_len(coln)),
        cex=0.75
    )
    axis(
        side=2,
        at=seq_len(rown),
        tick=FALSE,
        labels=sprintf("%02d", seq_len(rown)),
        cex=0.75,
        las=2
    )
    allwells <- expand.grid(seq_len(coln), seq_len(rown))
    .drawCircles(
        x=allwells[ , 1],
        y=allwells[ , 2],
        r=0.4,
        border="gray30"
    )
    wcol <- myPal(500)[as.numeric(cut(c(vals, arng), breaks=500))]
    wcol <- wcol[seq_len(length(vals))]
    .drawCircles(
        x=coli,
        y=rowi,
        r=0.4,
        border="gray30",
        col=wcol,
        lwd=1 + 3*outside
    )
    invisible(
        apply(
            badwlls,
            1,
            function(x) {
                lines(
                    x=c(x[2] - 0.5, x[2] + 0.5),
                    y=c(x[1] - 0.5, x[1] + 0.5)
                )
                lines(
                    x=c(x[2] - 0.5, x[2] + 0.5),
                    y=c(x[1] + 0.5, x[1] - 0.5)
                )
            }
        )
    )
    .drawCircles(
        x=coli,
        y=rowi,
        r=0.2,
        border="gray30",
        col="white"
    )
    text(
        x=coli,
        y=rowi,
        label=wllt,
        font=2,
        col="black",
        cex=0.65
    )

    lgnd <- try(plot.new(), silent=TRUE)
    if (is(lgnd, "try-error")) {
        par(mar=rep(0, 4), family="mono", new=FALSE)
        plot.new()
        warning("Could not add the legend. Please adjust device size. See",
                " ?gtoxPlotPlate for more information.")
    } else {
        par(mar=c(2.1, 0, 2.1, 6.1), family="mono")
        plot.window(
            xlim=c(0, 1),
            ylim=arng,
            xaxs="i",
            yaxs="i"
        )
        rect(
            0,
            seq(arng[1], arng[2], length.out=101)[-101],
            1,
            seq(arng[1], arng[2], length.out=101)[-1],
            col=myPal(100),
            border=myPal(100)
        )
        apts <- quantile(arng, probs=seq(0, 1, length.out=10))
        axis(
            side=4,
            at=apts,
            las=2,
            cex=0.5,
            labels=sprintf("%5.1f", apts)
        )
        axis(
            side=4,
            at=range(vals, na.rm=TRUE),
            labels=c("", ""),
            lwd=0,
            lwd.ticks=2,
            tcl=-0.5
        )
        axis(
            side=4,
            at=range(vals, na.rm=TRUE),
            labels=c("", ""),
            lwd=0,
            lwd.ticks=2,
            tcl=0.5
        )
    }

}

#-------------------------------------------------------------------------------
