#####################################################################
## This program is distributed in the hope that it will be useful, ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of  ##
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    ##
## GNU General Public License for more details.                    ##
#####################################################################

#-------------------------------------------------------------------------------
# gtoxListFlds: Load the column/field names from the given table and database
#-------------------------------------------------------------------------------

#' @title Load the field names for a table
#' @description
#' \code{gtoxListFlds} loads the column names for the given table and database.
#'
#' @param tbl Character of length 1, the gtox database table
#' @param db Character of length 1, the gtox database
#'
#' @details
#' This function can be particularly useful in defining the 'fld' param in the
#' gtoxLoad- functions.
#'
#' @examples
#' ## Gives the fields in the mc1 table
#' gtoxListFlds("mc1")
#'
#' @return A string of field names for the given table.
#'
#' @import data.table
#' @export


gtoxListFlds <- function(tbl, db=getOption("TCPL_DB")) {

    ## Variable-binding to pass R CMD Check
    name <- COLUMN_NAME <- NULL

    if (length(tbl) > 1 | length(db) > 1) {
        stop("tbl and db must both be of length 1.")
    }

    if (getOption("TCPL_DRVR") == "SQLite") {

        qformat <- "PRAGMA table_info(%s);"
        qstring <- sprintf(qformat, tbl)
        return(gtoxQuery(qstring, db)[ , name])

    }

    if (getOption("TCPL_DRVR") == "MySQL") {

        qformat <-
            "
            SELECT
                `COLUMN_NAME`
            FROM
                `INFORMATION_SCHEMA`.`COLUMNS`
            WHERE
                `TABLE_SCHEMA`='%s'
            AND
                `TABLE_NAME`='%s';
            "

        return(gtoxQuery(sprintf(qformat, db, tbl), db)[ , COLUMN_NAME])

    }

    stop(
        getOption("TCPL_DRVR"),
        " is not a supported database system. See ",
        "?gtoxConf for more details."
    )

}

#-------------------------------------------------------------------------------
