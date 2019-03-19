#####################################################################
## This program is distributed in the hope that it will be useful, ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of  ##
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    ##
## GNU General Public License for more details.                    ##
#####################################################################

#-------------------------------------------------------------------------------
# .parseListProtocols: parse output in .listProtocols
#-------------------------------------------------------------------------------

#' @title Parse output in .listScans
#' @description This function parses the output from the .listScans function
#' in the ThermoDB webservices.
#' 
#' @param xml_file XML formatted content from Thermo database
#' 
#' @note PMI-specific
#' @importFrom XML xmlToList
#' @import data.table

.parseListProtocols <- function(xml_file) {

    ## Convert the xml file to a list object and extract the list of scans
    slst <- xmlToList(xml_file)
    slst <- slst$Body$ListProtocolsResponse$ListProtocolsResult

    ## Parse out the ID, Name, & Version columns into a data.table object
    slst <- lapply(slst, "[", c("ID", "Name", "Version"))
    slst <- data.table(prcl=sapply(slst, "[[", 1),
                       name=sapply(slst, "[[", 2),
                       vrs =sapply(slst, "[[", 3))

    return(slst[])

}

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# .parseListScans: parse output in .listScans
#-------------------------------------------------------------------------------

#' @title Parse output in .listScans
#' @description This function parses the output from the .listScans function
#' in the ThermoDB webservices.
#' 
#' @param xml_file XML formatted content from Thermo database
#' 
#' @note PMI-specific
#' @importFrom XML xmlToList
#' @import data.table

.parseListScans <- function(xml_file) {

    ## Convert the xml file to a list object and extract the list of scans
    slst <- xmlToList(xml_file)
    slst <- slst$Body$ListScansResponse$ListScansResult

    ## Parse out the UPD, Barcode, Name, ScanFinish, & ProtocolID columns into a
    ## ProtocolID object
    slst <- lapply(slst, "[",
                   c("UPD", "Barcode", "Name", "ScanFinish", "ProtocolID"))
    slst <- data.table(upd  =sapply(slst, "[[", 1),
                       b_tmp=sapply(slst, "[[", 2),
                       n_tmp=sapply(slst, "[[", 3),
                       endt =sapply(slst, "[[", 4),
                       prcl =sapply(slst, "[[", 5))

    ## For NULL Barcode values, make the Barcode & Name "NULL" character strings
    bmiss <- sapply(slst$b_tmp, is.null)
    slst[bmiss,  barcode := "NULL"]
    slst[!bmiss, barcode := unlist(b_tmp)]
    slst[ , b_tmp := NULL]
    nmiss <- sapply(slst$n_tmp, is.null)
    slst[nmiss,  name := "NULL"]
    slst[!nmiss, name := unlist(n_tmp)]
    slst[ , n_tmp := NULL]

    return(slst[])

}

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# .listScans: List the scans stored on thermoDB
#-------------------------------------------------------------------------------

#' @title List the scans stored on the thermoDB server
#' @description This function queries the thermoDB webservices and returns a
#' data.table with the available scans stored on the server.
#'
#' @param store The ThermoDB store to pull data from
#' @param verbose Logical, should curl return the messages to the console?
#' @param curlurl webservice URL
#'
#' @note PMI-specific
#' @importFrom RCurl basicTextGatherer curlOptions curlPerform
#' @importFrom stringr str_locate
#' @import data.table

.listScans <- function(store="STORE", verbose=TRUE, curlurl=curlurl) {

    ## The query body format to send to the ThermoDB
    body_frmt <-
        "
        <soapenv:Envelope
        xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\"
        xmlns:ther=\"http://Thermo.Connect\"
        xmlns:ther1=\"http://schemas.datacontract.org/2004/07/Thermo.Connect\"
        xmlns:sys=\"http://schemas.datacontract.org/2004/07/System\">
        <soapenv:Header/>
        <soapenv:Body>
        <ther:ListScans>
            <ther:scanListRequest>
            <ther1:Store>
                <ther1:Alias>%s</ther1:Alias>
                <ther1:Name>%s</ther1:Name>
            </ther1:Store>
            </ther:scanListRequest>
        </ther:ListScans>
        </soapenv:Body>
        </soapenv:Envelope>
        "

    ## Insert the store into the query body format
    body <- sprintf(body_frmt, store, store)

    ## The XML query header
    header <- c(Accept="text/xml",
                Accept="multipart/*",
                'Content-Type'="text/xml; charset=utf-8",
                SOAPAction="http://Thermo.Connect/IHCSConnect/ListScans")

    ## Initiate a reader that will gather the query results from the curl
    ## operation; reset the reader, to ensure a clean slate
    reader=basicTextGatherer()
    reader$reset()

    ## Convigure the curl options, then perform the curl
    myOpts=curlOptions(verbose=verbose,
                         writefunc=reader$update,
                         header=FALSE)
    curlPerform(url=curlurl,
                httpheader=header,
                postfields=body,
                .opts=myOpts)

    ## Extract the XML file from the reader; reset the reader
    xml_file <- reader$value()
    reader$reset()

    ## Extract the body of the XML file
    str_1 <- str_locate(xml_file, "<s:Envelope")[1]
    str_2 <- str_locate(xml_file, "</s:Envelope>")[2]
    list_scan <- substr(xml_file, str_1, str_2)

    ## avoid special characters
    list_scan <- gsub("&#", "#", list_scan)
    ##  list_scan <- c('<?xml version="1.1" encoding="UTF-8"?>', list_scan)

    ## Return the paresed XML file, completed by .parseListScans
    return(list_scan)

}

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
## .listPrcls: List the protocols stored on thermoDB
#-------------------------------------------------------------------------------

#' @title List the protocols stored on the thermoDB server
#' @description This function queries the thermoDB webservices and returns a
#' data.table with the available protocols stored on the server.
#'
#' @param store The ThermoDB store to pull data from
#' @param verbose Logical, should curl return the messages to the console?
#' @param curlurl webservice URL
#'
#' @note PMI-specific
#' @importFrom RCurl basicTextGatherer curlOptions curlPerform
#' @importFrom stringr str_locate
#' @import data.table

.listPrcls <- function(store="STORE", verbose=TRUE, curlurl=curlurl) {

    ## The query body format to send to the ThermoDB
    body_frmt <-
        "
        <soapenv:Envelope
        xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\"
        xmlns:ther=\"http://Thermo.Connect\"
        xmlns:ther1=\"http://schemas.datacontract.org/2004/07/Thermo.Connect\"
        xmlns:sys=\"http://schemas.datacontract.org/2004/07/System\">
        <soapenv:Header/>
        <soapenv:Body>
        <ther:ListProtocols>
        <ther:protocolListRequest>
        <ther1:Store>
        <ther1:Alias>%s</ther1:Alias>
        <ther1:Name>%s</ther1:Name>
        <ther1:Port>0</ther1:Port>
        <ther1:Store>%s</ther1:Store>
        </ther1:Store>
        </ther:protocolListRequest>
        </ther:ListProtocols>
        </soapenv:Body>
        </soapenv:Envelope>
        "

    ## Insert the store into the query body format
    body <- sprintf(body_frmt, store, store, store)

    ## The XML query header
    header <- c(Accept="text/xml",
                Accept="multipart/*",
                'Content-Type'="text/xml; charset=utf-8",
                SOAPAction="http://Thermo.Connect/IHCSConnect/ListProtocols")

    ## Initiate a reader that will gather the query results from the curl
    ## operation; reset the reader, to ensure a clean slate
    reader=basicTextGatherer()
    reader$reset()

    ## Convigure the curl options, then perform the curl
    myOpts=curlOptions(verbose=verbose,
                         writefunc=reader$update,
                         header=FALSE)
    curlPerform(url=curlurl,
                httpheader=header,
                postfields=body,
                .opts=myOpts)

    ## Extract the XML file from the reader; reset the reader
    xml_file <- reader$value()
    reader$reset()

    ## Extract the body of the XML file
    str_1 <- str_locate(xml_file, "<s:Envelope")[1]
    str_2 <- str_locate(xml_file, "</s:Envelope>")[2]
    list_prcl <- substr(xml_file, str_1, str_2)

    ## avoid special characters
    list_prcl <- gsub("&#", "#", list_prcl)
    ##  list_scan <- c('<?xml version="1.1" encoding="UTF-8"?>', list_scan)

    ## Return the paresed XML file, completed by .parseListScans
    return(list_prcl)

}

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# .ListsWrapper: manage listScans and listProtocols
#-------------------------------------------------------------------------------

#' @title List scans and protocols
#' @description This function fetched and merges scan list and protocols
#' 
#' @param store The ThermoDB store to pull data from
#' @param verbose Logical, should curl return the messages to the console?
#' @param curlurl webservice URL
#' 
#' @note PMI-specific
#' @import data.table

.ListsWrapper <- function(store="STORE", verbose=TRUE, 
              curlurl=curlurl){

    ## List scans
    list_scan <- .parseListScans(.listScans(store=store, verbose=verbose,
                curlurl=curlurl))

    ## List protocols
    list_prcl <- .parseListProtocols(
        .listPrcls(store=store, verbose=verbose, curlurl=curlurl)
    )

    ## Merge lists
    slst <- merge(list_scan, list_prcl, by="prcl")

    ## Remove Cell Cycle barcodes
    slst <- slst[!grepl("Cell Cycle", slst$name.y, fixed=TRUE),]

    ## Remove duplicated barcodes
    slst[ , endt := as.POSIXct(endt, format="%Y-%m-%dT%H:%M:%S")]
    slst<- slst[order(endt, decreasing=TRUE), ]
    slst<- slst[!duplicated(slst$barcode), ]

    return(slst[])

}

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# .parseGetScanData: parse output in .getScanData
#-------------------------------------------------------------------------------

#' @title Parse output in .getScanData
#' @description This function parses the output from the .getScanData function
#' in the ThermoDB webservices.
#' 
#' @param xml_file XML formatted content from Thermo database
#' 
#' @note PMI-specific
#' 
#' @importFrom XML xmlToList
#' @importFrom utils type.convert
#' @import data.table

.parseGetScanData <- function(xml_file) {

    ## Convert XML file to list and extract the scan data
    sdat <- xmlToList(xml_file)
    sdat <- sdat$Body$GetScanDataResponse$GetScanDataResult$ScanData

    ## Extact the Wells list and parse it to a data.table object using the
    ## .parseWellScan function, then convert the columns to the appropriate
    ## class
    wdat <- rbindlist(lapply(sdat$Wells, .parseWellScan))
    wdat <- wdat[ , lapply(.SD, type.convert)]

    ## Parse and reorganize the annotation data for the wells into a data.table
    mid_map <- as.data.table(sdat$MeasureSpecs)
    mid_map <- as.data.table(t(mid_map))
    setnames(mid_map, c("measure_id", "temp","machine_name", "measure_tp"))
    null_names <- mid_map[ , sapply(machine_name, is.null)]
    mid_map[null_names, machine_name := temp]
    mid_map[ , temp := NULL]
    mid_map <- mid_map[ , lapply(.SD, function(x) type.convert(unlist(x)))]

    ## Map the annotation information to the well values
    setkey(wdat, measure_id)
    setkey(mid_map, measure_id)
    wdat <- merge(wdat, mid_map)

    ## Remove calibration data
    wdat <- wdat[measure_tp != "CalibrationPlate"]

    ## Add the u_boxtrack and upd fields
    wdat[ , u_boxtrack := sdat$ScanSpec$Barcode]
    wdat[ , upd  := sdat$ScanSpec$UPD]

    return(wdat[])

}

#-------------------------------------------------------------------------------
# .getScanData: Load data from a scan stored on thermoDB
#-------------------------------------------------------------------------------

#' @title Load data from a scan stored on thermoDB
#' @description This function queries the thermoDB webservices and returns a
#' data.table with the data for the given scan
#'
#' @param upd The plate/scan identifier used by ThermoDB
#' @param store The ThermoDB store to pull data from
#' @param verbose Logical, should curl return the messages to the console?
#' @param curlurl webservice URL
#'
#' @note PMI-specific
#' @importFrom RCurl basicTextGatherer curlOptions curlPerform
#' @importFrom stringr str_locate
#' @import data.table

.getScanData <- function(upd, store="STORE", verbose=TRUE, curlurl=curlurl) {

    ## The query body format to send to ThermoDB
    body_frmt <-
        "
        <soapenv:Envelope
        xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\"
        xmlns:ther=\"http://Thermo.Connect\"
        xmlns:ther1=\"http://schemas.datacontract.org/2004/07/Thermo.Connect\">
        <soapenv:Header/>
        <soapenv:Body>
        <ther:GetScanData>
            <ther:scanDataRequest>
            <ther1:Store>
            <ther1:Alias>%s</ther1:Alias>
            <ther1:Store>%s</ther1:Store>
            </ther1:Store>
            <ther1:Scan>
              <ther1:UPD>%s</ther1:UPD>
            </ther1:Scan>
            <ther1:IncludePasses>1</ther1:IncludePasses>
            <ther1:IncludeFields>0</ther1:IncludeFields>
            <ther1:IncludeObjects>0</ther1:IncludeObjects>
            <ther1:IncludeProtocol>0</ther1:IncludeProtocol>
            <ther1:IncludeMeasures>1</ther1:IncludeMeasures>
            <ther1:IncludeAnnotations>0</ther1:IncludeAnnotations>
            </ther:scanDataRequest>
        </ther:GetScanData>
        </soapenv:Body>
        </soapenv:Envelope>
        "

    ## Insert the store and upd into the query format
    body <- sprintf(body_frmt, store, store, upd)

    ## The query header to send to ThermoDB
    header <- c(Accept="text/xml",
                Accept="multipart/*",
                'Content-Type'="text/xml; charset=utf-8",
                SOAPAction="http://Thermo.Connect/IHCSConnect/GetScanData")

    ## Initiate a reader that will gather the query results from the curl
    ## operation; reset the reader, to ensure a clean slate
    reader=basicTextGatherer()
    reader$reset()

    ## Convigure the curl options, then perform the curl
    myOpts=curlOptions(verbose=verbose,
                         writefunc=reader$update,
                         header=FALSE)
    curlPerform(url=curlurl,
                httpheader=header,
                postfields=body,
                .opts=myOpts)

    ## Extract the XML file from the reader; reset the reader
    xml_file <- reader$value()
    reader$reset()

    ## Extract the body of the XML file
    str_1 <- str_locate(xml_file, "<s:Envelope")[1]
    str_2 <- str_locate(xml_file, "</s:Envelope>")[2]
    list_scan <- substr(xml_file, str_1, str_2)

    ## Return the paresed XML file, completed by .parseGetScanData
    return(.parseGetScanData(list_scan))

}

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# .parseWellScan: helper function for .parseGetScanData
#-------------------------------------------------------------------------------

#' @title helper function for .parseGetScanData
#' @description This function parses the individual wells within the
#' .parseGetScanData function
#' 
#' @param l Content of a single well
#' 
#' @note PMI-specific
#' @import data.table

.parseWellScan <- function(l) {

    ## Return NULL if the data did not pass quality measures
    if(is.null(l$Passes$PassData$PassMeasures))
        return(NULL)

    ## Extract the measure_id and measure_val from the provided list
    well_info <- l$WellSpec
    dat <- as.data.table(l$Passes$PassData$PassMeasures)
    dat <- data.table(measure_id =unlist(dat[1]),
                      measure_val=unlist(dat[2]))
    ## Extract the row and column indices from the provided list
    dat[ , c("rowi", "coli") := list(well_info$Row, well_info$Col)]

    return(dat[])

}

#-------------------------------------------------------------------------------
