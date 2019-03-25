#-------------------------------------------------------------------------------
# .bldAnnWll: prepare annotation file for legacy data - well based
#-------------------------------------------------------------------------------

#' @title Prepare annotation file for legacy data - Well based
#' @description This function parses well based legacy data annotation files,
#' channel, positive control, and assay mapping to build a data.frame with
#' compatible format for GladiaTOX loading
#'
#' @param study.data Legacy study annotation file from biobanking
#' @param vehicle.mapping List of vehicles to treatments mapping
#' @param phase Study phase to be registered in GladiaTOC DB
#'
#' @details
#' Function used only when processing historical data
#'
#' @note PMI-specific
#' @return Data table with annotation info

.bldAnnWll <- function(study.data, vehicle.mapping, phase) {

    DT <- NULL
                                        # load stimulus info
    DT$stimulus <- study.data$u_stimulus
    DT$`stimulus concentration` <- gsub(
        "_", " ", study.data$u_stimuluscncntration)
    ## load exposure info
    DT$`exposure duration` <- study.data$u_exposureduration
    ## load well info
    DT$plate <- as.integer(as.factor(unlist(lapply(
        as.character(study.data$u_boxtrack),
        function(x){strsplit(x,"_")[[1]][1]}))))
    DT$tube <- unlist(lapply(as.character(study.data$u_boxtrack),
                            function(x){strsplit(x,"_")[[1]][2]}))
    ## set well types
    DT$well_type <- rep("",nrow(study.data))
    DT$well_type[DT$stimulus %in%
        vehicle.mapping$chms[vehicle.mapping$type=="t"]] <- "t"
    DT$well_type[DT$stimulus %in%
        vehicle.mapping$chms[vehicle.mapping$type=="c"]] <- "c"
    DT$well_type[DT$stimulus %in%
        unique(vehicle.mapping$vehs)] <- "n"
    ## load vehicle info
    vehs <- c(
        as.character(vehicle.mapping$vehs),
        unique(as.character(vehicle.mapping$vehs))
    )
    DT$vehicle_name <- vehs[match(
        DT$stimulus,
        c(
            as.character(vehicle.mapping$chms),
            unique(as.character(vehicle.mapping$vehs))
        )
    )]
    ## load other study info
    DT$study <- study.data$sstudyid
    DT$study.phase <- phase
    DT$`cell type` <- study.data$u_celltype
    DT$endpoint <- study.data$u_endpoint
    DT$`exposure date` <- study.data$createdt
    DT$plate_set <- study.data$plate_set
    DT$`Biological Replicate` <- study.data$u_biologicalreplicate
    DT$smkid <- ""
    DT$`well format` <- study.data$u_tubetype
    DT$assay <- paste(DT$endpoint,DT$`exposure duration`,sep="_")
    DT$Date <- study.data$createdt
    DT$u_boxtrack <- unlist(
        lapply(
            as.character(study.data$u_boxtrack),
            function(x){strsplit(x,"_")[[1]][1]}
        )
    )
    DT$s_sampleid <- study.data$s_sampleid

    return(data.frame(DT,check.names=FALSE))
}
