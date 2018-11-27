#####################################################################
## This program is distributed in the hope that it will be useful, ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of  ##
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    ##
## GNU General Public License for more details.                    ##
#####################################################################

#-------------------------------------------------------------------------------
# gtoxConfDefault: Generate default config file
#-------------------------------------------------------------------------------

#' @rdname config_funcs
#' @export

gtoxConfDefault <- function () {

    sqlite <- file.path(
        system.file(package="GladiaTOX"),
        "sql",
        "gladiatoxdb.sqlite"
    )
    gtoxConf(db=sqlite, user=NA, host=NA, drvr="SQLite")

}

#-------------------------------------------------------------------------------
