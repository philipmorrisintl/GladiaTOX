#####################################################################
## This program is distributed in the hope that it will be useful, ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of  ##
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    ##
## GNU General Public License for more details.                    ##
#####################################################################

#-------------------------------------------------------------------------------
# .buildAssayTab: prepare assay table mapping info
#-------------------------------------------------------------------------------

#' @title Prepare assay table mapping info
#' @description This function parses plate annotations and create a mapping
#' between assay endpoints and channels
#'
#' @param plate.mtd Legacy study annotation file from biobanking
#' @param chn.map List of endpoints to thermo channels mapping
#'
#' @details
#' Function used only when processing historical data
#'
#' @note PMI-specific
#' @return Table with assay information

.buildAssayTab <- function(plate.mtd, chn.map) {

    ## Merge channel mapping and data assay info
    tab <- as.data.frame(
        unique(
            cbind(
                as.character(plate.mtd$`endpoint`),
                as.character(plate.mtd$`exposure duration`)
            )
        )
    )
    tab <- merge.data.frame(tab, chn.map, by.x="V1", by.y="Assay")

    ## build assay table
    at <- NULL
    at$`assay` <- rep(paste(tab$V1, tab$V2,sep="_"),2)
    at$`timepoint` <- rep(tab$V2, 2)
    at$`component` <- rep(paste(tab$V1, tab$Endpoint, tab$V2,sep="_"),2)
    at$`endpoint` <- paste(
        at$`component`,
        c(
            rep("up",dim(tab)[1]),
            rep("dn",dim(tab)[1])
        ),
        sep="_"
    )
    at$`channel` <- rep(tab$Channel, 2)
    at <- data.frame(at, check.names=FALSE)
    at <- at[sort.int(
        as.character(at$component),
        decreasing=TRUE,
        index.return=TRUE
    )$ix, ]

    ## remove not compatible aeids
    at <- at[!(
        grepl(
            "Cell count",
            at$endpoint
        ) & grepl("_up", at$endpoint)
    ), ]
    at <- at[!(
        grepl(
            "DNA damage (pH2AX)",
            at$endpoint, fixed=TRUE
        ) & grepl("_dn", at$endpoint)
    ), ]
    at <- at[!(
        grepl(
            "Stress kinase (c-Jun)",
            at$endpoint, fixed=TRUE
        ) & grepl("_dn", at$endpoint)
    ), ]
    at <- at[!(
        grepl(
            "Caspase 3/7 intensity",
            at$endpoint, fixed=TRUE
        ) & grepl("_dn", at$endpoint)
    ), ]
    at <- at[!(
        grepl(
            "Cytochrome C release",
            at$endpoint, fixed=TRUE
        ) & grepl("_dn", at$endpoint)
    ), ]
    at <- at[!(
        grepl(
            "Cytotoxicity (TIER1)_Cell membrane permeability",
            at$endpoint, fixed=TRUE
        ) & grepl("_dn", at$endpoint)
    ), ]
    at <- at[!(
        grepl(
            "NF-kB nuclear content",
            at$endpoint, fixed=TRUE
        ) & grepl("_dn", at$endpoint)
    ), ]
    at <- at[!(
        grepl(
            "Apo Necro (casp37)_Cell membrane permeability",
            at$endpoint, fixed=TRUE
        ) & grepl("_dn", at$endpoint)
    ), ]
}
