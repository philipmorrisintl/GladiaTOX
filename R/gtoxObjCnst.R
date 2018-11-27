#####################################################################
## This program is distributed in the hope that it will be useful, ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of  ##
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    ##
## GNU General Public License for more details.                    ##
#####################################################################

#-------------------------------------------------------------------------------
# gtoxObjCnst: Generate a constant model objective function to optimize
#-------------------------------------------------------------------------------

#' @rdname Models
#'
#' @section Constant Model (cnst):
#' \code{gtoxObjCnst} calculates the likelyhood for a constant model at 0. The
#' only parameter passed to \code{gtoxObjCnst} by \code{p} is the scale term
#' \eqn{\sigma}. The constant model value \eqn{\mu_{i}}{\mu[i]} for the
#' \eqn{i^{th}}{ith} observation is given by:
#' \deqn{\mu_{i} = 0}{\mu[i] = 0}
#'
#' @importFrom stats dt
#' @export

gtoxObjCnst <- function(p, resp) {

    ## This function takes creates an objective function to be optimized using
    ## the starting constant model parameter, and response.
    ##
    ## Arguments:
    ##   p:     a numeric vector of length 1 containg the starting values for
    ##          the constant model, in order: log error term
    ##   lresp: a numeric vector containing the response values to produce the
    ##          objective function
    ##
    ## Value:
    ##   An objective function for the constant model and the given resp data

    mu <- 0
    sum(dt((resp - mu)/exp(p[1]), df=4, log=TRUE) - p[1])

}

#-------------------------------------------------------------------------------
