#####################################################################
## This program is distributed in the hope that it will be useful, ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of  ##
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    ##
## GNU General Public License for more details.                    ##
#####################################################################

#-------------------------------------------------------------------------------
# .bldAnnPlt: prepare annotation file for legacy data - plate based
#-------------------------------------------------------------------------------

#' @title Prepare annotation file for legacy data - Plate based
#' @description This function parses plate based legacy data annotation files,
#' channel, positive control, and assay mapping to build a data.frame with 
#' compatible format for GladiaTOX loading
#' 
#' @param study.data Legacy study annotation file from biobanking
#' @param vehicle.mapping List of vehicles to treatments mapping
#' @param posctr.doses Vector of positive control doses
#' @param phase Study phase
#' 
#' @details
#' Function used only when processing historical data
#' 
#' @keywords internal
#' @return Data table with annotation info

.bldAnnPlt <- function(study.data, vehicle.mapping, posctr.doses, phase) {

    ## auxiliary function to split strings
    splitString <- function(string) {
        return(unlist(lapply(
            as.character(string),
            function(xx){ xxx<- strsplit(xx, ",")[[1]]
                if(length(xxx)==3) {
                    return(c(xxx, ""))
                } else {
                    return(xxx)
                }
            })))
    }

    ## auxiliary function to get vehicle
    getVehicle <- function(tt, cc) {
        if(all(!tt$Compound %in% cc))
            return("")
        return(as.character(tt$Vehicle[tt$Compound %in% cc])[1])
    }

    ## build data table
    DT <- NULL
    for(kk in seq_len(nrow(study.data))) {

        dt <- NULL

        ## compound column
        dt$stimulus <- NULL
        compound <- splitString(study.data$COMPOUND.1[kk])
        for(cc in seq_len(4)) {
            col <- c(
                rep(compound[cc],6),
                getVehicle(vehicle.mapping,compound[cc])
            )
            if(cc != 4)
                col<- c(
                    col,
                    as.character(
                        vehicle.mapping$Compound[
                            vehicle.mapping$Assay %in% study.data$END.POINT[kk]]
                    )
                )
            else
                col<- c(
                    col,
                    as.character(
                        unique(
                            vehicle.mapping$Vehicle[
                                vehicle.mapping$Compound %in%
                                    vehicle.mapping$Compound[
                                        vehicle.mapping$Assay %in%
                                            study.data$END.POINT[kk]]]
                        )
                    )
                )
            dt$stimulus<- c(dt$stimulus, rep(col, 3))
        }

        ## dose column
        dt$`stimulus concentration` <- NULL
        doses <- NULL
        for(ii in seq_len(6))
            doses[[ii]] <- splitString(
                study.data[[paste0("Compound1Dose", ii)]][kk])
        for(ii in seq_len(4)) {
            dd <- NULL
            for(iii in seq_len(6))
                dd <- c(dd, doses[[iii]][ii])
            dd <- c(dd, "", posctr.doses[ii])
            dd[!dd %in% ""] <- paste(dd[!dd%in%""], "uM")
            dt$`stimulus concentration` <- c(
                dt$`stimulus concentration`, rep(dd, 3))
        }

        ## exposure duration
        dt$`exposure duration` <-
            rep(as.character(study.data$EXPOSURE.DURATION[kk]), 96)

        ## plate number
        dt$plate <- rep(kk, 96)

        ## tube
        dt$tube<- paste0(
            rep(toupper(letters[seq_len(8)]), 12),
            unlist(lapply(seq_len(12), function(xx) { rep(xx, 8) }))
        )

        ## well type
        dt$well_type <- rep("", 96)
        dt$well_type[!grepl("G", dt$tube) & !grepl("H", dt$tube) &
            !dt$stimulus %in% ""]<- "t"
        dt$well_type[grepl("G",dt$tube) & !dt$stimulus %in% ""] <- "n"
        dt$well_type[grepl("H",dt$tube)] <- "c"
        dt$well_type[grepl("H",dt$tube) &
            dt$`stimulus concentration` %in% ""] <- "n"

        ## Vehicle ID
        dt$vehicle_name <- as.character(
            vehicle.mapping$Vehicle[match(
                dt$stimulus,vehicle.mapping$Compound)])
        dt$vehicle_name[is.na(dt$vehicle_name)] <-
            as.character(dt$stimulus[is.na(dt$vehicle_name)])

        ## Study ID
        dt$study <- rep(as.character(study.data$STUDY.NUMBER[kk]), 96)
        dt$study.phase <- rep(phase, 96)

        ## Cell type
        dt$`cell type` <- rep(as.character(
            study.data$Tissue...original.sample.type[kk]), 96)

        ## endpoint
        dt$endpoint <- rep(as.character(study.data$END.POINT[kk]), 96)

        ## Exposure date
        dt$`exposure date`<- rep(format(
            as.Date(
                study.data$EXPOSURE.DATE[kk], format="%m/%d/%y"),"%Y-%m-%d"),
            96)

        ## Plate set and Replicate
        dt$plate_set <- rep(0,96)
        dt$`Biological Replicate` <- rep(1,96)

        ## smkID
        dt$smkid <- rep("", 96)
        ## dt$smkid[dt$well_type %in% "t"] <- "NOSMKID"

        ## Well format
        dt$`well format` <- rep("96-well", 96)

        ## Assay
        dt$assay <- paste(dt$endpoint, dt$`exposure duration`, sep="_")

        ## Study date
        dt$Date <- rep(
            format(
                as.Date(
                    study.data$STUDY.CREATION.DATE[kk],
                    format="%m/%d/%y"
                ),
                "%Y-%m-%d"
            ), 96
        )
        
        ## Sample ID
        dt$u_boxtrack <- rep(as.character(study.data$SAMPLE.ID[kk]), 96)

        if(is.null(DT))
            DT <- dt
        else
            DT <- mapply(c, DT, dt, SIMPLIFY=FALSE)
    }

    DT <- data.frame(DT, check.names=FALSE)
    return(DT)

}
