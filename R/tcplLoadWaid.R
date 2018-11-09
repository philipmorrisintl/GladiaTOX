#####################################################################
## This program is distributed in the hope that it will be useful, ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of  ##
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    ##
## GNU General Public License for more details.                    ##
#####################################################################

#-------------------------------------------------------------------------------
# tcplLoadWaid: Load well annotation information
#-------------------------------------------------------------------------------

#' @title Load well annotation information
#'
#' @description
#' \code{tcplLoadWaid} queries the tcpl database and returns the well annotation
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
#' tcplLoadWaid()
#' 
#' ## Reset configuration
#' options(conf_store)
#' 
#' @return A data.table with the well annotation information for the given
#' parameters
#'
#' @import data.table
#' @export

tcplLoadWaid <- function(fld=NULL, val=NULL) {

    if (!is.null(fld)) {
        vfield <- c("apid", "aid", "date_plate", "date_treat", "date_harvest",
                    "cell_passage", "cell_lot", "waid", "spid", "rowi", "coli",
                    "wllt", "vhid", "conc", "gui_sample_id")
        if (!all(fld %in% vfield)) stop("Invalid 'fld' value(s).")
    }

    qformat <-
        "
    SELECT
      assay_plate_well.waid,
      assay_plate_well.apid,
      aid,
      spid,
      rowi,
      coli,
      wllt,
      vhid,
      conc,
      date_plate,
      date_treat,
      date_harvest,
      cell_passage,
      cell_lot,
      gui_sample_id,
      hr_barcode,
      old_name,
      s_sampleid,
      u_boxtrack
    FROM assay_plate_well
      LEFT JOIN assay_plate
      ON assay_plate_well.apid=assay_plate.apid
        LEFT JOIN bb_waid_map
        ON assay_plate_well.waid=bb_waid_map.waid
          LEFT JOIN bb_apid_map
          ON assay_plate_well.apid=bb_apid_map.apid
    "

    if (!is.null(fld)) {

        fld <- .prepField(fld=fld,
                          tbl=c("assay_plate", "assay_plate_well"),
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
