#####################################################################
## This program is distributed in the hope that it will be useful, ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of  ##
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    ##
## GNU General Public License for more details.                    ##
#####################################################################

#-------------------------------------------------------------------------------
# tcplLoadApid: Load assay plate information
#-------------------------------------------------------------------------------

#' @title Load assay plate information
#'
#' @description
#' \code{tcplLoadApid} queries the tcpl database and returns the assay plate
#' information for the given field and values.
#'
#' @param fld Character, the field(s) to query on
#' @param val List, vectors of values for each field to query on. Must be in
#' the same order as 'fld'.
#'
#' @examples
#' ## Store the current config settings, so they can be reloaded at the end 
#' ## of the examples
#' conf_store <- tcplConfList()
#' tcplConfDefault()
#' 
#' ## Prepare for analysis before QC + process data
#' tcplLoadApid()
#' 
#' ## Reset configuration
#' options(conf_store)
#' 
#' @return A data.table with the assay plate information for the given
#' parameters
#'
#' @import data.table
#' @export

tcplLoadApid <- function(fld=NULL, val=NULL) {

    if (!is.null(fld)) {
        vfield <- c("apid", "aid", "date_plate", "date_treat", "date_harvest",
                    "cell_passage", "cell_lot")
        if (!all(fld %in% vfield)) stop("Invalid 'fld' value(s).")
    }

    qformat <-
        "
    SELECT
      assay_plate.apid,
      aid,
      old_name,
      date_plate,
      date_treat,
      date_harvest,
      cell_passage,
      cell_lot,
      hr_barcode,
      u_boxtrack
    FROM assay_plate
      LEFT JOIN bb_apid_map
      ON assay_plate.apid=bb_apid_map.apid
    "

    if (!is.null(fld)) {

        fld <- .prepField(fld=fld,
                          tbl=c("bb_apid_map", "assay_plate"),
                          db=options()$TCPL_DB)

        qformat <- paste(qformat, "WHERE")
        qformat <- paste0(qformat,
                          "  ",
                          paste(fld, "IN (%s)", collapse=" AND "))
        qformat <- paste0(qformat, ";")

        if (!is.list(val)) val <- list(val)
        val <- lapply(val, function(x) paste0("\"", x, "\"", collapse=","))

        qstring <- do.call(sprintf, args=c(qformat, val))

    } else {

        qstring <- qformat

    }

    dat <- suppressWarnings(tcplQuery(query=qstring))

    if (nrow(dat) == 0) {
        warning("The given ", fld,"(s) are not in the tcpl database.")
        return(dat)
    }

    dat[]

}

#-------------------------------------------------------------------------------
