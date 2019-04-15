#####################################################################
## This program is distributed in the hope that it will be useful, ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of  ##
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    ##
## GNU General Public License for more details.                    ##
#####################################################################

#-------------------------------------------------------------------------------
# gtoxConfList:
#-------------------------------------------------------------------------------

#' @rdname config_funcs
#' 
#' @examples
#' 
#' ## List configuration of database parameters
#' gtoxConfList()
#' 
#' @export

gtoxConfList <- function(show.pass=FALSE) {

    opts <- list("TCPL_DB", "TCPL_USER", "TCPL_HOST", "TCPL_DRVR")
    if (show.pass) opts <- c(opts, "TCPL_PASS")
    do.call(options, opts)

}

#-------------------------------------------------------------------------------
