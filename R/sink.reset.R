#####################################################################
## This program is distributed in the hope that it will be useful, ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of  ##
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    ##
## GNU General Public License for more details.                    ##
#####################################################################

#-------------------------------------------------------------------------------
# sink.reset: Reset all sink connections, returning all output to console
#-------------------------------------------------------------------------------

#' @title Reset all sinks
#'
#' @description
#' \code{sink.reset} resets all sinks and returns all output to the console.
#'
#' @details
#' \code{sink.reset} identifies all sinks with \code{sink.number} then returns
#' all output and messages back to the console.
#' 
#' @examples 
#' sink.reset()
#' 
#' @return None
#'
#' @family tcpl abbreviations
#' @seealso \code{\link{sink}}, \code{\link{sink.number}}
#' @export

sink.reset <- function() {

    for (i in seq_len(sink.number())) {
        sink(NULL)
        sink(NULL, type="message")
    }

}

#-------------------------------------------------------------------------------
