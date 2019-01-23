#####################################################################
## This program is distributed in the hope that it will be useful, ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of  ##
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    ##
## GNU General Public License for more details.                    ##
#####################################################################

#-------------------------------------------------------------------------------
# line2user: Convert margin line to usr coordinates
#-------------------------------------------------------------------------------

#' @title Convert margin line to usr coordinates
#'
#' @param line Line to convert
#' @param side Line side
#'
#' @importFrom graphics grconvertX grconvertY
#' 
#' @return none
#' 

.line2user <- function(line, side) {
    lh <- par('cin')[2] * par('cex') * par('lheight')
    x_off <- diff(grconvertX(c(0, lh), 'inches', 'npc'))
    y_off <- diff(grconvertY(c(0, lh), 'inches', 'npc'))
    switch(
        side,
        `1`=grconvertY(-line * y_off, 'npc', 'user'),
        `2`=grconvertX(-line * x_off, 'npc', 'user'),
        `3`=grconvertY(1 + line * y_off, 'npc', 'user'),
        `4`=grconvertX(1 + line * x_off, 'npc', 'user'),
        stop("Side must be 1, 2, 3, or 4", call.=FALSE)
    )
}
