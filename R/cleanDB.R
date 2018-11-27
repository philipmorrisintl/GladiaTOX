#####################################################################
## This program is distributed in the hope that it will be useful, ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of  ##
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    ##
## GNU General Public License for more details.                    ##
#####################################################################

#-------------------------------------------------------------------------------
# .cleanDB: Remove all data from database for testing purposes
#-------------------------------------------------------------------------------

#' @title Remove all data from database for testing purposes
#' @description This function deletes all data from the database and preserves 
#' the method tables. 
#' 
#' @note PMI-specific

.cleanDB <- function() {

    v <- gtoxConfList()
    p <- names(v)
    pn <- vapply(p, nchar, integer(1))
    sep <- vapply(
        pn, function(x) paste(rep(" ", 11 - x), collapse=""),
        character(1)
    )
    sep <- paste0(":", sep)
    cs <- vapply(
        seq_along(v),
        function(x) paste(p[x], v[[x]], sep=sep[x]),
        "character"
    )

    packageStartupMessage(
        "Current database settings:\n  ",
        paste(cs, collapse="\n  "),
        "\nAre you sure you want to erase all content?"
    )

    x <- readline("Type 'yes' to proceed or anything else to cancel. ")

    if (x == "yes") {
        tbls <- unname(unlist(gtoxQuery("SHOW TABLES;")))
        keep <- c(grep("_methods", tbls, value=TRUE), "mc5_fit_categories")
        reset_qs <- paste0("TRUNCATE TABLE ", tbls[!tbls %in% keep], ";")
        vapply(reset_qs, gtoxSendQuery, "")
    }

}
