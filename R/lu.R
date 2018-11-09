#####################################################################
## This program is distributed in the hope that it will be useful, ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of  ##
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    ##
## GNU General Public License for more details.                    ##
#####################################################################

#-------------------------------------------------------------------------------
# lu: Length of unique
#-------------------------------------------------------------------------------

#' @title Abbreviation for \code{length(unique(x))}
#' 
#' @description 
#' \code{lu} takes a logical vector, \code{x}, and returns 
#' \code{length(unique(x))}.
#' 
#' @param x A logical
#' 
#' @examples 
#' lu(c(1, 1, 2, 3))
#' 
#' @return The unique of the \code{TRUE} values in \code{x}
#' 
#' @family tcpl abbreviations
#' @seealso \code{\link{unique}}, \code{\link{which}}
#' @export

lu <- function(x) length(unique(x))

#-------------------------------------------------------------------------------
