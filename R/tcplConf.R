#####################################################################
## This program is distributed in the hope that it will be useful, ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of  ##
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    ##
## GNU General Public License for more details.                    ##
#####################################################################

#-------------------------------------------------------------------------------
# tcplConf: Configure the tcpl options
#-------------------------------------------------------------------------------

#' @rdname config_funcs
#' @export

tcplConf <- function (drvr=NULL, user=NULL, pass=NULL, host=NULL,
    db=NULL, int=NULL) {

    ## Variable-binding to pass R CMD Check
    Value <- NULL

    if (!is.null(user)) options("TCPL_USER"=user)
    if (!is.null(pass)) options("TCPL_PASS"=pass)
    if (!is.null(host)) options("TCPL_HOST"=host)
    if (!is.null(db))   options("TCPL_DB"=db)
    if (!is.null(int) && !is.logical(int)) {
        stop("The 'int' setting must be logical.")
    }
    if (!is.null(int))  options("TCPL_INT" =int)

    if (!is.null(drvr)) {

        if (!drvr %in% c("SQLite", "MySQL")) {
            stop(
                drvr,
                " is not a supported database driver. Must be 'SQLite' or ",
                "'MySQL'."
            )
        }

        if (drvr == "SQLite") {
            options("TCPL_DRVR"="SQLite")
        }

        if (drvr == "MySQL") {
            options("TCPL_DRVR"="MySQL")
            mxp <- tcplQuery(
                "SHOW VARIABLES LIKE 'max_allowed_packet'")[ , Value]
            mxp <- as.numeric(mxp)
            if (mxp < 1073741824) {
                warning(
                    "The 'max_allowed_packet' MySQL server setting is set to ",
                    mxp, " bytes. It is recommended that you increase it to ",
                    "1073741824 bytes to ensure larger queries run without ",
                    "error."
                )
            }
        }

    }

}

#-------------------------------------------------------------------------------
