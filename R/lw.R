#####################################################################
## This program is distributed in the hope that it will be useful, ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of  ##
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    ##
## GNU General Public License for more details.                    ##
#####################################################################

#-------------------------------------------------------------------------------
# lw: Length of which is true
#-------------------------------------------------------------------------------

#' @title Abbreviation for \code{length(which(x))}
#' 
#' @description 
#' \code{lw} takes a logical vector, \code{x}, and returns 
#' \code{length(which(x))}.
#' 
#' @param x A logical
#' 
#' @examples 
#' lw(c(TRUE, FALSE, TRUE))
#' 
#' @return The length of the \code{TRUE} values in \code{x}
#' 
#' @family gtox abbreviations
#' @seealso \code{\link{length}}, \code{\link{which}}
#' @export

lw <- function(x) length(which(x))

#-------------------------------------------------------------------------------
