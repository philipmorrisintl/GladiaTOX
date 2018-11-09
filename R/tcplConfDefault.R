#####################################################################
## This program is distributed in the hope that it will be useful, ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of  ##
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    ##
## GNU General Public License for more details.                    ##
#####################################################################

#-------------------------------------------------------------------------------
# tcplConfDefault: Generate default config file
#-------------------------------------------------------------------------------

#' @rdname config_funcs
#' @export

tcplConfDefault <- function () {

    sqlite <- file.path(
        system.file(package="GladiaTOX"),
        "sql",
        "gladiatoxdb.sqlite"
    )
    tcplConf(db=sqlite, user=NA, host=NA, drvr="SQLite")

}

#-------------------------------------------------------------------------------
