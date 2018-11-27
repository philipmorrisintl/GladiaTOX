#####################################################################
## This program is distributed in the hope that it will be useful, ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of  ##
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    ##
## GNU General Public License for more details.                    ##
#####################################################################

#' @title gtoxConfLoad
#' @description Load the current configuration file
#' @rdname config_funcs
#' @param list.new Logical of length 1, should the new settings be printed?
#' @export
gtoxConfLoad <- function (list.new = TRUE) {
    
    stopifnot(is.logical(list.new) && length(list.new) == 1)
    
    conf_file <- .getConfFile()

    ## Variable-binding to pass R CMD Check
    DRVR <- USER <- PASS <- HOST <- DB <- INT <- NULL

    source(
        conf_file,
        local=TRUE
    )

    gtoxConf(DRVR, USER, PASS, HOST, DB, INT)
    gtoxConfList()
}
