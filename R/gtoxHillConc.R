#####################################################################
## This program is distributed in the hope that it will be useful, ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of  ##
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    ##
## GNU General Public License for more details.                    ##
#####################################################################

#-------------------------------------------------------------------------------
# gtoxHillConc: Calculate the concentration for a given value
#-------------------------------------------------------------------------------

#' @rdname hill_utils
#' @export

gtoxHillConc <- function(val, tp, ga, gw, bt=0) {

    ga - log10((tp - bt)/(val - bt) - 1)/gw

}

#-------------------------------------------------------------------------------
