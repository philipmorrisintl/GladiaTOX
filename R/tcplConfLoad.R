#####################################################################
## This program is distributed in the hope that it will be useful, ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of  ##
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    ##
## GNU General Public License for more details.                    ##
#####################################################################

#-------------------------------------------------------------------------------
# tcplConfLoad: Load the current configuration file
#-------------------------------------------------------------------------------

#' @rdname config_funcs
#' @export

tcplConfLoad <- function () {

    ## Variable-binding to pass R CMD Check
    DRVR <- USER <- PASS <- HOST <- DB <- INT <- NULL

    source(
        file.path(
            system.file(package="GladiaTOX"),
            "TCPL.config"
        ),
        local=TRUE
    )

    tcplConf(DRVR, USER, PASS, HOST, DB, INT)

}

#-------------------------------------------------------------------------------
