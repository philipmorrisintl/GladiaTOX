#####################################################################
## This program is distributed in the hope that it will be useful, ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of  ##
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    ##
## GNU General Public License for more details.                    ##
#####################################################################

#-------------------------------------------------------------------------------
# glLoadInput: load input file
#-------------------------------------------------------------------------------

#' @title Check validity of input file
#' @description This function check the structure and content of an input file.
#'
#' @param file file URL
#' @param studyname Name of the study
#' @param phasename Name of the phase
#' @param tab input table is file URL is not provided
#'
#' @details
#' This function is useful to load an input file in the GladiaTOX GUI
#'
#' @return List of error messages in JSON format
#'
#' @importFrom RJSONIO toJSON
#' @export
#'

glLoadInput <- function(file=NULL, studyname = "samplestudy", 
                        phasename = "samplephase", tab = NULL){
    
    if(!is.null(file)){
        if(!file.exists(file))
            return(toJSON(list('status'='error', 'results'=list('File does not exists.'))))
        
        tab = try(read.csv(file), silent=TRUE)
        if(inherits(tab, "try-error"))
            return(toJSON(list('status'='error', 'results'=list('File is not in csv format.'))))
    }else
        if(is.null(tab))
            return(toJSON(list('status'='error', 'results'=list('No File and input tab provided.'))))

    assaytab = data.frame(
        assay = paste(tab$assay, tab$duration, sep="_"),
        timepoint = tab$duration,
        component = paste(tab$assay, tab$endpoint, tab$duration, sep="_"),
        endpoint = paste(tab$assay, tab$endpoint, tab$duration, sep="_"),
        channel = tab$channel
    )
    
    assaytab = unique(assaytab)
    assaytab_up = assaytab 
    assaytab_up$endpoint = paste(assaytab_up$endpoint, "up", sep="_")
    assaytab_dn = assaytab 
    assaytab_dn$endpoint = paste(assaytab_dn$endpoint, "dn", sep="_")
    assaytab = rbind(assaytab_up, assaytab_dn)
    
    platetab = data.frame(
        "stimulus" = as.character(tab$stimulus),
        "stimulus concentration" = tab$concentration,
        "exposure duration" = tab$duration,
        "plate" = tab$plate,
        "tube" = tab$tube,
        "well_type" = tab$tubetype,
        "vehicle_name" = tab$vehicle,
        "study" = studyname,
        "study.phase" = phasename,
        "cell type" = "samplecelltype",
        "endpoint" = tab$assay,
        "exposure date" = tab$exposuredate,
        "plate_set" = 0,
        "Biological Replicate" = 1,
        "smkid" = "",
        "well format" = tab$plateformat,
        "assay" = paste(tab$assay, tab$duration, sep="_"),
        "Date" = format(Sys.time(), format="%Y-%m-%d"),
        "u_boxtrack" = as.character(tab$plate),
        check.names=FALSE, stringsAsFactors=FALSE)
    
    loadAnnot(platetab, assaytab, NULL)
    
    dat = data.table(
        measure_val = tab$val,
        rowi = match(toupper(str_extract(tab$tube, '[A-Z]+')), LETTERS),
        coli = as.numeric(str_extract(tab$tube, '[0-9]+')),
        machine_name = tab$channel,
        u_boxtrack = as.character(tab$plate)
    )
    
    asid = gtoxLoadAsid(fld = c("asnm", "asph"), val = list(studyname, phasename))$asid
    dat <- prepareDatForDB(asid=asid, dat)
    gtoxWriteData(dat[ , list(acid, waid, wllq, rval)], lvl = 0, type = "mc")
    
    return(toJSON(list('status'='ok', 'results'=list('Data loaded in DB.'))))
}






