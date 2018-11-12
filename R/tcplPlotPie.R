#####################################################################
## This program is distributed in the hope that it will be useful, ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of  ##
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    ##
## GNU General Public License for more details.                    ##
#####################################################################

#-------------------------------------------------------------------------------
# tcplPlotPie: Create piechart plots
#-------------------------------------------------------------------------------

#' @title Create piechart plots
#'
#' @description
#' \code{tcplPlotPie} creates the piechart plots.
#'
#' @param chid Integer of length 1, the chid value
#' @param mrks Numeric, the values for concentration label rings
#' @param aeid Integer, the aeid values to plot
#' @param col Vector of colors
#' @param lbl  Vector with pie labels (optional)
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

tcplPlotPie <- function(chid, mrks, aeid, col=NULL, lbl=NULL) {

    mrks <- -log10(mrks/1e6)

    cdat <- tcplLoadChem("chid", chid)

    dat <- tcplLoadData(5, c("aeid", "spid"), list(aeid, cdat$spid))
    dat <- tcplPrepOtpt(dat)

    dat[ , pot := 10^modl_acb]
    dat[is.na(pot), pot := 1e6]
    dat[ , pot := -log10(pot/1e6)]

    mdat <-  dat[ ,
                 list(
                     ptmn=ifelse(lw(pot > 0) > 0, mean(pot[pot > 0]), 0),
                     ptsd=ifelse(lw(pot > 0) > 2, sd(pot[pot > 0]), NA_real_),
                     nsmp=lw(pot > 0)),
                 by=list(aeid, aenm, chid, chnm)]

    setkey(mdat, aeid)
    ae <- aeid
    mdat <- mdat[J(ae)]

    if (lw(mdat$ptmn > 0)) {
        rng <- range(mdat[ptmn > 0, c(ptmn, ptmn + ptsd)], na.rm=TRUE)
        if (rng[1] < min(mrks) | rng[2] > max(mrks)) {
            warning("Data range outside the given 'mrks' values.")
        }
    }

    trans <- function(x) {(x - min(mrks))/diff(range(mrks))*0.9 + 0.1}
    mdat[ , c("tmn", "up") := list(trans(ptmn), trans(ptmn + ptsd))]
    tmrk <- trans(mrks)

    colfunc <- colorRampPalette(brewer.pal(10, "Spectral")[3:10])
    colvec <- if (is.null(col)) colfunc(nrow(mdat)) else col
    mdat[ , col := colvec]
    mdat[is.na(tmn), col := "gray80"]

    angls <- seq(0, 2*pi, 2*pi/length(aeid))

    x1 <- vector(mode="numeric")
    x2 <- vector(mode="numeric")
    x3 <- vector(mode="numeric")
    x4 <- vector(mode="numeric")
    y1 <- vector(mode="numeric")
    y2 <- vector(mode="numeric")
    y3 <- vector(mode="numeric")
    y4 <- vector(mode="numeric")

    for (j in seq_len(length(aeid))) {

        theta <- seq(angls[j],
                     angls[j + 1],
                     length.out=360/(2*pi/(angls[j + 1] - angls[j])))
        thrd <- floor(length(theta)/3)
        er <- theta[(thrd + 1):(thrd*2)]
        thrd <- diff(range(theta))/3
        er <- seq(theta[1] + thrd, theta[1] + thrd*2, length.out=12)

        ## Draw a slice if the value is not NA or < 0.1
        if (!is.na(mdat[j, tmn]) && mdat[j, tmn] > 0.1) {
            x1 <- c(x1, 0.1*cos(angls[j]), mdat[j, tmn]*cos(theta),
                    0.1*cos(angls[j + 1]), 0.1*cos(rev(theta)), NA)
            y1 <- c(y1, 0.1*sin(angls[j]), mdat[j, tmn]*sin(theta),
                    0.1*sin(angls[j + 1]), 0.1*sin(rev(theta)), NA)
            x2 <- c(x2, 0.1*cos(mean(angls[c(j, j + 1)])),
                    mdat[j, up]*cos(mean(angls[c(j, j + 1)])), NA,
                    mdat[j, up]*cos(er), NA)
            y2 <- c(y2, 0.1*sin(mean(angls[c(j, j + 1)])),
                    mdat[j, up]*sin(mean(angls[c(j, j + 1)])), NA,
                    mdat[j, up]*sin(er), NA)
        } else { ## Else draw a slice in the center
            x1 <- c(x1, 0, 0.1*cos(theta), 0, NA)
            y1 <- c(y1, 0, 0.1*sin(theta), 0, NA)
        }
        x3 <- c(x3, 0.1*cos(mean(angls[c(j, j + 1)])),
                1.05*cos(mean(angls[c(j, j + 1)])), NA)
        y3 <- c(y3, 0.1*sin(mean(angls[c(j, j + 1)])),
                1.05*sin(mean(angls[c(j, j + 1)])), NA)
        x4 <- c(x4, 0.1*cos(mean(angls[c(j, j + 1)])),
                1.1*cos(mean(angls[c(j, j + 1)])), NA)
        y4 <- c(y4, 0.1*sin(mean(angls[c(j, j + 1)])),
                1.1*sin(mean(angls[c(j, j + 1)])), NA)
    }

    mx <- vector(mode="numeric")
    my <- vector(mode="numeric")

    for (mrk in tmrk) {

        crcl <- seq(0, 2*pi, length.out=360)
        mx <- c(mx, mrk, mrk*cos(crcl), mrk, NA)
        my <- c(my, 0, mrk*sin(crcl), 0, NA)

    }

    par(pty="s", mar=rep(0, 4))
    plot.new()
    plot.window(xlim=c(-1.05, 1.05), ylim=c(-1.05, 1.05))

    polygon(x=mx, y=my, lty="dashed", border="grey80")
    if (!is.null(lbl)) {
        lines(x=x3, y=y3, lty="dotted", col="grey60")
        to26 <- function(x) {
            x <- x - 26*floor(x/26)
            x[x == 0] <- 26
            x
        }
        text(x=x4[seq(2, length(x3), by=3)],
             y=y4[seq(2, length(y3), by=3)],
             lbl, col="grey80")
    }
    lines(x=x2, y=y2)
    polygon(x=x1, y=y1, col=mdat[ , col])
    text(x=0, y=tmrk, labels=signif(10^-mrks*1e6, 3), pos=3,
         offset=0.1)
    mdat[!is.na(chnm), list(aeid, aenm, chid, chnm, ptmn, ptsd, nsmp)]

}
