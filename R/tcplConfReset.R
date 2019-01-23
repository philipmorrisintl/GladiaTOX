#####################################################################
## This program is distributed in the hope that it will be useful, ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of  ##
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    ##
## GNU General Public License for more details.                    ##
#####################################################################

#-------------------------------------------------------------------------------
# tcplConfReset: Generate default config file
#-------------------------------------------------------------------------------

#' @title Functions for configuring the gtox package
#'
#' @description
#' This function is used to reset gtox sqlite DB configuration.
#' 
#' @export
#' 
#' @return none

tcplConfReset <- function () {

    conf_file <- tcpl:::.getConfFile()
    
    message(
        "###################################################################",
        "",
        "## Detailed information about this file available in the help file",
        "## for tcplConf (?tcplConf).",
        "",
        "DRVR <- \"SQLite\"",
        "HOST <- NA_character_",
        "USER <- NA_character_",
        "PASS <- NA_character_",
        "DB   <- file.path(system.file(package=\"GladiaTOX\"),",
        "                             \"sql\",",
        "                             \"gladiatoxdb.sqlite\")",
        "INT  <- FALSE",
        "",
        "###################################################################",
        sep="\n",
        file=file.path(conf_file),
        append=FALSE
    )

}

#-------------------------------------------------------------------------------
