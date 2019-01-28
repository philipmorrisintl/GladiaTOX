#####################################################################
## This program is distributed in the hope that it will be useful, ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of  ##
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    ##
## GNU General Public License for more details.                    ##
#####################################################################

#-------------------------------------------------------------------------------
# tcplConfDefault: Generate default config file
#-------------------------------------------------------------------------------

#' @title Functions for configuring the gtox package
#'
#' @description
#' This function is used to set default configuration of the gtox sqlite DB.
#' 
#' @examples
#' 
#' tcplConfDefault()
#' 
#' @importFrom tcpl tcplConf
#' @export
#' 
#' @return none

tcplConfDefault <- function () {

    sqlite <- file.path(
        system.file(package="GladiaTOX"),
        "sql",
        "gladiatoxdb.sqlite"
    )
    tcplConf(db=sqlite, user=NA, host=NA, drvr="SQLite")

}

#-------------------------------------------------------------------------------
