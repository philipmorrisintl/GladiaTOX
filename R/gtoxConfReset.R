#####################################################################
## This program is distributed in the hope that it will be useful, ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of  ##
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    ##
## GNU General Public License for more details.                    ##
#####################################################################

#-------------------------------------------------------------------------------
# gtoxConfReset: Generate default config file
#-------------------------------------------------------------------------------

#' @rdname config_funcs
#' 
#' @examples
#' 
#' ## Set the environment variable pointing to the configuration file
#' Sys.setenv(TCPL_CONF=file.path(system.file(package="GladiaTOX"),"gtoxConf"))
#' 
#' ## Configure database
#' gtoxConfReset()
#'
#' @export

gtoxConfReset <- function () {

    conf_file <- .getConfFile()
    
    message(
        "###################################################################",
        "",
        "## Detailed information about this file available in the help file",
        "## for gtoxConf (?gtoxConf).",
        "",
        "DRVR <- \"SQLite\"",
        "HOST <- NA_character_",
        "USER <- NA_character_",
        "PASS <- NA_character_",
        "DB   <- file.path(system.file(package=\"GladiaTOX\"),",
        "                             \"sql\",",
        "                             \"gladiatoxdb.sqlite\")",
        "",
        "###################################################################",
        sep="\n",
        file=file.path(conf_file),
        append=FALSE
    )

}

#-------------------------------------------------------------------------------
