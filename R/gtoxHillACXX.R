#####################################################################
## This program is distributed in the hope that it will be useful, ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of  ##
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    ##
## GNU General Public License for more details.                    ##
#####################################################################

#-------------------------------------------------------------------------------
# gtoxHillACXX: Calculate the concentration for a given activity level
#-------------------------------------------------------------------------------

#' @rdname hill_utils
#' @export

gtoxHillACXX <- function(XX, tp, ga, gw, bt=0) {

    y <- tp * XX/100
    ga - log10((tp - bt)/(y - bt) - 1)/gw

}

#-------------------------------------------------------------------------------
