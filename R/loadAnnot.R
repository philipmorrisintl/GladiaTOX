#####################################################################
## This program is distributed in the hope that it will be useful, ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of  ##
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    ##
## GNU General Public License for more details.                    ##
#####################################################################

#-------------------------------------------------------------------------------
# loadAnnot: register the annotations provided by GUI
#-------------------------------------------------------------------------------

#' @title Register the annotations provided by GUI
#' @description This function parses the output from the GUI and registers the
#' appropriate data within the GladiaTOX database.
#'
#' @param plate path to 'plate' JSON file produced by the GUI
#' @param assay path to 'assay' JSON file produced by the GUI
#' @param outFile character of length 1, name of the output file
#'
#' @details
#' If loading legacy data, 'outFile' should be set to NULL and no JSON file
#' will be written. 
#' 
#' @examples
#' 
#' \dontrun{
#' ## Load annotation files in database
#' loadAnnot(plate, assay, NULL)
#' }
#' 
#' @return Logical value
#' 
#' @importFrom RJSONIO toJSON
#' @importFrom stringr str_extract
#' @import data.table
#' @importFrom tcpl tcplLoadAid tcplLoadAcid
#' @export
#' 

loadAnnot <- function(plate, assay, outFile="out.json") {

    ## Read plate data
    plate <- .readInput(plate)
    setnames(
        plate,
        old=c(
            "stimulus", "stimulus concentration",
            "exposure duration", "tube", "Biological Replicate",
            "well_type", "study", "study.phase", "cell type",
            "exposure date", "well format", "Date", "assay"
        ),
        new=c(
            "chnm", "conc", "timepoint_hr", "well", "bior", "wllt",
            "asnm", "asph", "cell_type", "date_treat",
            "assay_footprint", "date_harvest", "anm"
        )
    )
    plate[ , date_treat   := as.Date(date_treat,   '%Y-%m-%d')]
    plate[ , date_harvest := as.Date(date_harvest, '%Y-%m-%d')]

    ## Track input order
    plate[ , inOrdr := .I]

    ## define timestamp
    tstamp <- as.integer(Sys.time())

    ## Add Human Readable Barcode if not in input
    if (is.null(plate$hr_barcode)) {
        plate[ , hr_barcode := paste(
            substr(asnm, 1, 7), "_",
            substr(cell_type, 1, 5), "_",
            substr(endpoint, 1, 6), "_",
            substr(timepoint_hr, 1, 3), "_",
            format(date_treat, '%d%b'),
            paste0(
                ## 'S', sprintf('%02d', plate_set),
                'P', sprintf('%02d', plate),
                'R', sprintf('%02d', bior)),
            sep=""
            )]
        plate[ , hr_barcode := paste(
            gsub(" ", "", hr_barcode), tstamp, sep="_"
        )]
    }

    ## Register chemicals/stimulants
    chnmloaded <- unique(tcplLoadChem()$chnm)
    chnm <- unique(plate[wllt != "n", chnm])
    tcplRegister("chid", flds=list(chnm=chnm[!chnm%in%chnmloaded]))
    plate <- merge(
        plate,
        unique(tcplLoadChem()[ , list(chid, chnm)]),
        by="chnm", all.x=TRUE
    )

    ## Register sample IDs
    plate[ , spid := paste0(chnm, "_Pl", plate, "_", tstamp)]
    tcplRegister("spid", flds=unique(plate[wllt == "t", list(spid, chid)]))

    ## Register vehicles
    vhloaded <- unique(gtoxLoadVehicle()$vehicle_name)
    vhl <- unique(plate[ , list(vehicle_name)])$vehicle_name
    tcplRegister("vehicle", list(vehicle_name=vhl[!vhl%in%vhloaded]))
    plate <- merge(plate, gtoxLoadVehicle(), by="vehicle_name", all.x=TRUE)

    ## Register the study
    tcplRegister("asid", unique(plate[ , list(asnm, asph)]))

    ## Read assay data
    assay <- .readInput(assay)
    setnames(
        assay,
        old=c("assay", "component", "endpoint", "channel"),
        new=c("anm", "acnm", "aenm", "machine_name")
    )

    ## Adjust the timepoint to hours
    assay[ , timepoint_hr := str_extract(timepoint, pattern='^[0-9]+')]
    assay[ , tu := str_extract(timepoint, pattern='[aA-zZ]+$')]
    assay <- merge(
        assay,
        data.table(
            tu=c("m", "h", "d", "min", "w", "b", "y"),
            tmult=c(1/60, 1, 24, 1/60, 120, 3600, 1314000)
        ),
        by="tu", all.x=TRUE
    )
    assay[ , timepoint_hr := as.numeric(timepoint_hr)*tmult]

    ## Register the assays
    assay <- merge(
        unique(plate[ , list(asnm, asph, anm, assay_footprint)]),
        assay,
        by="anm"
    )
    assay <- merge(
        assay,
        tcplLoadAsid(
            fld=c("asnm","asph"),
            val=list(assay$asnm, assay$asph)
        ),
        by=c("asnm", "asph")
    )
    ##  assay <- merge(assay, tcplLoadAsid("asnm", assay$asnm), by="asnm")
    assay <- assay[order(gsub("_[0-9]+h", "", aenm), timepoint_hr)]
    tcplRegister(
        "aid",
        unique(
            assay[ , list(asid, anm, timepoint_hr, assay_footprint)]
        )
    )
    assay <- merge(
        assay,
        unique(tcplLoadAid("asid", assay$asid)[ , list(anm, aid)]),
        by="anm"
    )
    assay <- assay[order(gsub("_[0-9]+h", "", aenm), timepoint_hr)]

    ## Add components & endpoints for the assays
    tcplRegister(
        "acid", unique(assay[ , list(aid, acnm, machine_name)])
    )
    assay <- merge(
        assay,
        unique(
            tcplLoadAcid(
                fld="asid",
                assay$asid
            )[ , list(acid, acnm)]
        ),
        by="acnm"
    )
    assay <- assay[order(gsub("_[0-9]+h", "", aenm), timepoint_hr)]
    assay[ , analysis_direction := ifelse(grepl('_dn$', aenm), "down", "up")]
    assay[ , c("burst_assay", "fit_all") := list(0, 0)]
    tcplRegister(
        "aeid",
        unique(
            assay[ , list(
                acid, aenm, burst_assay, fit_all,
                analysis_direction
            )]
        )
    )

    ## Add a new plate to the study
    plate <- merge(plate, unique(assay[ , list(aid, anm)]), by="anm")
    tcplRegister(
        "apid",
        unique(
            plate[ , list(aid, date_treat, date_harvest, hr_barcode)]
        )
    )
    plate <- merge(
        plate,
        gtoxLoadApid()[ , list(apid, hr_barcode)],
        by="hr_barcode"
    )

    ## Add wells to the plate
    plate[ , gui_sample_id := paste0("Pl", plate, "_", well, "_", tstamp)]
    plate[ , rowi := match(toupper(str_extract(well, '[A-Z]+')), LETTERS)]
    plate[ , coli := as.numeric(str_extract(well, '[0-9]+'))]
    plate[ , conc := as.numeric(str_extract(conc, '\\d+\\.*\\d*'))]
    tcplRegister(
        "waid",
        unique(
            plate[ , list(
                apid, rowi, coli, spid, wllt, vhid,
                conc, gui_sample_id
            )]
        )
    )
    waid <- gtoxLoadWaid(fld="apid", plate$apid)

    ## Register s_sampleid and u_boxtrack when in the table.
    if (!is.null(plate$u_boxtrack)) {
        setkey(plate, apid, rowi, coli)
        setkey(waid,  apid, rowi, coli)
        plate <- merge(plate, waid[ , list(waid,  apid, rowi, coli)])
        tcplRegister(
            "bb_apid",
            unique(plate[ , list(apid, u_boxtrack)])
        )
        if (!is.null(plate$s_sampleid)) {
            tcplRegister(
                "bb_waid",
                unique(plate[ , list(waid, s_sampleid)])
            )
        }
    }

    if (!is.null(outFile)) {
        setkey(waid, gui_sample_id)
        outTbl <- waid[J(plate[order(inOrdr), gui_sample_id]),
            list(waid, apid, hr_barcode)]
        outTbl[ , gtoxIDs := paste(
            sprintf('%09d', waid), sprintf('%09d', apid))]
        outTbl[ , comment := sub(paste0('_', tstamp), "", hr_barcode)]
        cat(toJSON(outTbl[ , list(comment, gtoxIDs)]), file=outFile)
    }

    return(TRUE)

}
