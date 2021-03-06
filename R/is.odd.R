#####################################################################
## This program is distributed in the hope that it will be useful, ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of  ##
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    ##
## GNU General Public License for more details.                    ##
#####################################################################

#-------------------------------------------------------------------------------
# is.odd: tests whether a value is odd or even, returning TRUE for odd
#-------------------------------------------------------------------------------

#' @title Check for odd numbers
#'
#' @description
#' \code{is.odd} takes an integer vector, \code{x}, and returns TRUE for odd
#' integers.
#'
#' @param x An integer
#'
#' @keywords internal
#' 
#' @return \code{TRUE} for odd integers and \code{FALSE} for even integers.
#'
#' @family gtox abbreviations

is.odd <- function(x) {

    if (!is.integer(x)) x <- as.integer(x)
    x %% 2 != 0

}

#-------------------------------------------------------------------------------
