#####################################################################
## This program is distributed in the hope that it will be useful, ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of  ##
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    ##
## GNU General Public License for more details.                    ##
#####################################################################

#-------------------------------------------------------------------------------
# tcplPlotPieLgnd: Create piechart plot legend
#-------------------------------------------------------------------------------

#' @title Create piechart plot legend
#'
#' @description
#' \code{tcplPlotPieLgnd} creates the piechart plots.
#'
#' @param aenm Character, the assay endpoint names
#' @param ncol Interger, the number of columns for the legend
#' @param col Vector of colors
#' @param fit.labels Boolean, if TRUE, scale the text to fit
#'
#'
#' @return None
#'
#' @importFrom stats sd
#' @importFrom RColorBrewer brewer.pal
#' @importFrom grDevices colorRampPalette
#' @importFrom graphics text par plot.new points plot.window
#' @export

tcplPlotPieLgnd <- function(aenm, ncol=2, col=NULL, fit.labels=TRUE) {

    n <- length(aenm)
    nc <- ceiling(n/ncol) ## Number of aenm per column
    grps <- split(aenm, ceiling(seq_along(aenm)/nc))

    colfunc <- colorRampPalette(brewer.pal(10, "Spectral")[3:10])
    colvec <- if (is.null(col)) colfunc(n) else col

    par(mar=rep(0, 4))
    plot.new()
    plot.window(ylim=c(0, nc + 1), xlim=c(0, 10*ncol), asp=NA)

    yval <- rep(nc:1, ncol)[seq_len(n)]
    xval <- rep(seq(0, (ncol - 1)*10, 10), each=nc)[seq_len(n)]
    points(xval, yval, pch=22, bg=colvec, cex=2)
    scl <- ifelse(fit.labels, 9/max(strwidth(aenm)), 1)
    text(xval + 0.5, yval, aenm, adj=c(0, 0.5), cex=scl)

}
