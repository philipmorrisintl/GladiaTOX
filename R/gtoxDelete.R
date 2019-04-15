#####################################################################
## This program is distributed in the hope that it will be useful, ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of  ##
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    ##
## GNU General Public License for more details.                    ##
#####################################################################

#-------------------------------------------------------------------------------
# gtoxDelete: Delete rows from gtox databases
#-------------------------------------------------------------------------------

#' @title Delete rows from gtox databases
#'
#' @description
#' \code{gtoxDelete} deletes rows from the given table and database.
#'
#' @param tbl Character, length 1, the table to delete from
#' @param fld Character, the field(s) to query on
#' @param val   List, vectors of values for each field to query on. Must be in
#'              the same order as 'fld'.
#' @param db Character, the database containing the table
#'
#' @keywords internal
#'
#' @seealso \code{\link{gtoxSendQuery}}
#' 
#' @return None
#'
#' @import data.table

gtoxDelete <- function(tbl, fld, val, db) {

                                        # Check for valid inputs
    if (length(tbl) != 1 | !is(tbl, "character")) {
        stop("The input 'tbl' must be a character of length one.")
    }

    qformat <- paste("DELETE FROM", tbl, "WHERE")

    qformat <- paste0(qformat, "  ", paste(fld, "IN (%s)", collapse=" AND "))
    qformat <- paste0(qformat, ";")

    if (!is.list(val)) val <- list(val)
    val <- lapply(val, function(x) paste0("\"", x, "\"", collapse=","))

    qstring <- do.call(sprintf, args=c(qformat, val))

    gtoxSendQuery(query=qstring, db=db)

}

#-------------------------------------------------------------------------------
