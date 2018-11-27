#####################################################################
## This program is distributed in the hope that it will be useful, ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of  ##
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    ##
## GNU General Public License for more details.                    ##
#####################################################################

#-------------------------------------------------------------------------------
# gtoxRegister: Register new assay or chemical information
#-------------------------------------------------------------------------------

#' @rdname rgstr_funcs
#'
#' @import data.table
#' @export

gtoxRegister <- function(what, flds) {

    valid_what <- c(
        "asid",  "aid", "acid", "aeid", "spid",
        "chid", "clib", "vehicle", "apid", "waid",
        "bb_waid", "bb_apid"
    )

    if (!what %in% valid_what) {
        stop("Not a valid 'what' input.")
    }

    i <- switch(
        what,
        asid=list(
            "assay_source",
            c("assay_source_name", "assay_source_phase")
        ),
        aid =list(
            "assay",
            c("asid", "assay_name", "assay_footprint")
        ),
        acid=list(
            "assay_component",
            c("aid", "assay_component_name", "machine_name")
        ),
        aeid=list(
            "assay_component_endpoint",
            c("acid", "assay_component_endpoint_name")
        ),
        spid=list(
            "sample",
            c("spid", "chid")
        ),
        chid=list(
            "chemical",
            c("chnm")
        ),
        clib=list(
            "chemical_library",
            c("chid", "clib")
        ),
        vehicle=list(
            "vehicle",
            c("vehicle_name")
        ),
        apid=list(
            "assay_plate",
            c("aid")),
        waid=list(
            "assay_plate_well",
            c("apid", "spid", "rowi", "coli", "wllt", "vhid", "conc")),
        bb_waid=list(
            "bb_waid_map",
            c("waid", "s_sampleid")),
        bb_apid=list(
            "bb_apid_map",
            c("apid", "u_boxtrack"))
    )

    pot_flds <- gtoxListFlds(tbl=i[[1]], db=options()$TCPL_DB)
    flds <- as.data.table(flds)
    setnames(flds, .convertNames(names(flds)))

    if (any(!i[[2]] %in% names(flds))) {
        stop(
            "Missing required fields for registering a(n) ", what,
            ". See ?gtoxRegister"
        )
    }

    if (any(!names(flds) %in% pot_flds)) {
        warning(
            "Some of the given fields are not in the ", i[[1]], " table. ",
            "Extra fields will be ignored."
        )
        xtra <- names(flds)[!names(flds) %in% pot_flds]
        flds[ , c(xtra) := NULL]
    }

    gtoxAppend(dat=flds, tbl=i[[1]], db=options()$TCPL_DB)

    TRUE

}
