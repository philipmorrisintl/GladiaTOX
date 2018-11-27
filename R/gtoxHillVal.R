#####################################################################
## This program is distributed in the hope that it will be useful, ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of  ##
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    ##
## GNU General Public License for more details.                    ##
#####################################################################

#-------------------------------------------------------------------------------
# gtoxHillVal: Calculate the value for a given concentration
#-------------------------------------------------------------------------------

#' @rdname hill_utils
#' @export

gtoxHillVal <- function(logc, tp, ga, gw, bt=0) {

    bt + (tp - bt)/(1 + 10^((ga - logc)*gw))

}

#-------------------------------------------------------------------------------
