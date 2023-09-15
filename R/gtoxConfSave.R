#####################################################################
## This program is distributed in the hope that it will be useful, ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of  ##
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    ##
## GNU General Public License for more details.                    ##
#####################################################################

#-------------------------------------------------------------------------------
# gtoxConfSave: Save current gtox settings to config file
#-------------------------------------------------------------------------------

#' @rdname config_funcs
#' 
#' @examples
#' 
#' ## Set the environment variable pointing to the configuration file
#' Sys.setenv(TCPL_CONF=file.path(system.file(package="GladiaTOX"),"gtoxConf"))
#' 
#' ## Configure database
#' gtoxConfSave()
#'
#' @export

gtoxConfSave <- function () {

    conf_file <- .getConfFile()
    
    if(any(vapply(gtoxConfList(), is.null, logical(1)))) {
        stop(
            "One of the gtox settings is NULL. Saving the configuration file ",
            "with a NULL setting\nwill keep the package from loading in ",
            "future sessions."
        )
    }

    drvr <- options()$TCPL_DRVR
    if (!drvr %in% c("SQLite", "MariaDB")) {
        stop(
            drvr, " is not a supported database driver. Must be 'SQLite' or ",
            "'MariaDB'."
        )
    }
    drvr <- shQuote(drvr)

    host <- options()$TCPL_HOST
    host <- if(is.na(host)) host else shQuote(host)
    user <- options()$TCPL_USER
    user <- if(is.na(user)) user else shQuote(user)
    pass <- options()$TCPL_PASS
    if(is.null(pass)) pass <- NA
    pass <- if(is.na(pass)) pass else shQuote(pass)
    db   <- options()$TCPL_DB
    db   <- if(is.na(db))   db   else shQuote(db)

    message(
        "###################################################################\n",
        "\n",
        "## Detailed information about this file available in the help file",
        "## for gtoxConf (?gtoxConf).\n",
        "\n",
        "DRVR <-", drvr, "\n",
        "HOST <-", host, "\n",
        "USER <-", user, "\n",
        "PASS <-", pass, "\n",
        "DB   <-", db, "\n",
        "\n",
        "###################################################################\n",
        sep=" ",
        file=file.path(conf_file),
        append=FALSE
    )

}

#-------------------------------------------------------------------------------
