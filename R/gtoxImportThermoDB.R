#####################################################################
## This program is distributed in the hope that it will be useful, ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of  ##
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    ##
## GNU General Public License for more details.                    ##
#####################################################################

#-------------------------------------------------------------------------------
# gtoxImportThermoDB: Import data from ThermoDB by study ID
#-------------------------------------------------------------------------------

#' @title Import data from ThermoDB by study ID
#'
#' @description This function accesses the ThermoDB webservices and imports
#' data from ThermoDB to the gtox database.
#'
#' @param asid Integer, the assay study/source ID to import data for
#' @param verbose Logical, should the output from the curl be displayed?
#' @param write Logical, should the data be written to the database, or just
#' returned?
#' @param store Character, the name of the store on ThermoDB to query
#' @param type Character, the data type: 'mc' or 'sc'
#' @param curlurl URL of the webservice
#'
#' @examples
#' ## Fetches data from ThermoDB to load in GladiaTOX DB prior processing
#' conf_store <- gtoxConfList()
#' gtoxConfDefault()
#' 
#' \dontrun{
#' ## Fetch data from ThermoDB
#' dat <- gtoxImportThermoDB(asid=1L)
#' }
#' 
#' ## Reset configuration
#' options(conf_store)
#' 
#' @return Data table with content fetched from Thermo DB.
#' 
#' @import data.table
#' @export

gtoxImportThermoDB <- function(asid, verbose=TRUE, write=FALSE, store="STORE",
            type="mc",
            curlurl="https://YOUR_THERMODB_SERVER_HOSTNAME/HTTPHCSConnect") {

    ## Check if the service is available
    temp <- try(curlPerform(url=curlurl), silent=TRUE)
    if (is(temp, "try-error"))
        stop("Error: you have to set curlurl to an available service.")

    acid_map <- gtoxLoadAcid("asid", asid, c("aid", "machine_name"))
    well_dat <- gtoxLoadWaid("aid", acid_map[ , unique(aid)])

    scans <- .ListsWrapper(store=store, verbose=verbose,
                curlurl=curlurl)

    upds <- scans[scans$barcode %in% unique(well_dat$u_boxtrack), unique(upd)]

    dat <- lapply(upds, .getScanData, verbose=verbose, store=store,
                    curlurl=curlurl)
    dat <- rbindlist(dat)

    dat$coli<- dat$coli + 1 #to be compatible with the database sample indexing
    dat$rowi<- dat$rowi + 1 #to be compatible with the database sample indexing

    setkey(well_dat, u_boxtrack, coli, rowi)
    setkey(dat, u_boxtrack, coli, rowi)

    dat <- well_dat[dat]

    setkey(acid_map, aid, machine_name)
    setkey(dat, aid, machine_name)

    dat <- acid_map[dat, nomatch=0]

    dat[ , rval := measure_val]
    dat[ , wllq := 1]

    if (write) {
        gtoxWriteData(dat[ , list(acid, waid, wllq, rval)],
                        lvl=0, type=type)
    }

    return(dat[])

}

#-------------------------------------------------------------------------------
