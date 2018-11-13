#####################################################################
## This program is distributed in the hope that it will be useful, ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of  ##
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    ##
## GNU General Public License for more details.                    ##
#####################################################################

#-------------------------------------------------------------------------------
# tcplUpdate: Update assay or chemical information
#-------------------------------------------------------------------------------

#' @rdname rgstr_funcs
#'
#' @import data.table
#' @export


tcplUpdate <- function(what, id, flds) {

    lens <- c(length(id), vapply(flds, length, integer(1)))
    if (!identical(max(lens), min(lens))) {
        stop(
            "The length of 'id' and the lengths of each list element in 'flds'",
            " must be equal.")
    }

    i <- switch(what,
                asid="assay_source",
                aid ="assay",
                acid="assay_component",
                aeid="assay_component_endpoint",
                spid="sample",
                chid="chemical",
                clib="chemical_library",
                waid="assay_plate_well",
                apid="assay_plate",
                vehicle="vehicle")

    if (is.null(i)) stop("Not a valid 'what' input.")

    pot_flds <- tcplListFlds(tbl=i[[1]], db=getOption("TCPL_DB"))
    flds <- as.data.table(flds)
    setnames(flds, .convertNames(names(flds)))

    if (any(!names(flds) %in% pot_flds)) {
        warning("Some of the given fields are not in the ", i[[1]], " table. ",
                "Extra fields will be ignored.")
        xtra <- names(flds)[!names(flds) %in% pot_flds]
        flds[ , c(xtra) := NULL]
    }

    qf <- paste("UPDATE", i, "%s", "WHERE", what, "=", id)

    for (i in seq_len(nrow(flds))) {
        inst <- paste0(names(flds), "=", "\"", flds[i], "\"", collapse=", ")
        inst <- paste("SET", inst)
        qf[i] <- sprintf(qf[i], inst)
    }

    res <- lapply(qf, tcplSendQuery)

    test <- !vapply(res, isTRUE, logical(1))
    if (any(test)) {
        warning("Error updating the following ids: ",
                paste(id[test], collapse=", "))
    }

    TRUE

}
