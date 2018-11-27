#####################################################################
## This program is distributed in the hope that it will be useful, ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of  ##
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    ##
## GNU General Public License for more details.                    ##
#####################################################################

#-------------------------------------------------------------------------------
# gtoxPlotPlate: look at the plate map for a given apid
#-------------------------------------------------------------------------------

#' @title Plot plate heatmap
#'
#' @description
#' \code{gtoxPlotPlate} generates a heatmap of assay plate data
#'
#' @param dat data.table containing gtox data
#' @param apid Character of length 1, the apid to plot
#' @param id Integer of length 1, the assay component id (acid) or assay
#' endpoint id (aeid), depending on level. Only need to specify for multiplexed
#' assays when more than one acid/aeid share an apid.
#' @param quant Numeric vector, the range of data to include in the legend
#'
#' @details
#' The legend represents the range of the data supplied to dat, for the
#' applicable ID. The additional horizontal lines on the legend indcate the
#' range of the plotted plate, to show the relation of the plate to the assay
#' as a whole. A plot with a legend specific for the given apid can be created
#' by only supplying the data for the apid of interest to 'dat'.
#'
#' The quant parameter, by default including 99.8% of the data,
#' allows for extreme outliers without losing resolution. Outliers in either
#' direction will be highlighted with a dark ring, as seen in the example.
#' A NULL value for 'quant' will not restrict the data at all, and will use
#' the full range for the legend.
#'
#' Wells with a well quality of 0 (only applicable for level 1 plots), will
#' have an "X" through their center.
#'
#' @note
#' For the optimal output size, use width = 12, height = 8,
#' pointsize = 12, units = "in"
#'
#' @examples
#'  
#' acnm = "Cytotoxicity (TIER1)_Cytochrome C release_24h"
#' pltnm = "S-000049119"
#' myaid = gtoxLoadApid()[u_boxtrack == pltnm, aid]
#' myaid = myaid[myaid%in%gtoxLoadAid(fld = "asid", val = 1L)$aid]
#' apid = gtoxLoadApid()[u_boxtrack == pltnm & aid == myaid, apid]
#' acid = gtoxLoadAcid(fld = c("aid", "acnm"), val = list(myaid, acnm))[, acid]
#' l2 = gtoxLoadData(lvl = 2L, fld = "acid", val = acid)
#' gtoxPlotPlate(dat = l2, apid = apid, id = acid)
#' dev.off()
#' 
#' @return None
#'
#' @import data.table
#' @importFrom stats quantile
#' @export


gtoxPlotPlate <- function(dat, apid, id=NULL, quant=c(0.001, 0.999)) {

    ## Variable-binding to pass R CMD Check
    wllq <- aid <- wllt <- cndx <- nwll <- rown <- rowi <- coln <- NULL
    coli <- anm <- NULL

    if (length(apid) != 1) stop("'apid' must be of length 1.")
    ap <- apid

    lvl <- NULL
    dnames <- names(dat)
    if ("m1id" %in% dnames) {lvl <- 1L; idnm <- "acid"; vlnm <- "rval"}
    if ("m2id" %in% dnames) {lvl <- 2L; idnm <- "acid"; vlnm <- "cval"}
    if ("m3id" %in% dnames) {lvl <- 3L; idnm <- "aeid"; vlnm <- "resp"}
    if (is.null(lvl)) stop("Invalid dat input.")

    sub <- dat[apid == ap]
    setnames(sub, c(idnm, vlnm), c("aid", "val"))
    if (!"wllq" %in% dnames) sub[ , wllq := 1L]

    if (is.null(id)) id <- sub[ , unique(aid)]
    if (length(id) != 1) {
        stop("Multiple ids for the given plate, must specifiy 'id'.")
    }

    sub[, nwlt := paste0(wllt, cndx)]

    ap_size <- do.call(
        if (idnm == "acid") "gtoxLoadAcid" else "gtoxLoadAeid",
        list(fld=idnm, val=id, add.fld="assay_footprint")
    )
    setnames(ap_size, c("aid", "anm", "nwll"))

    ap_size[ , nwll := as.numeric(gsub("[^0-9]", "", nwll))]
    plate_dim <- data.table(
        nwll=c(6, 12, 24, 48, 96, 384, 1536),
        coln=c(3,  4,  6,  8, 12,  24,   48),
        rown=c(2,  3,  4,  6,  8,  16,   32)
    )
    setkey(plate_dim, nwll)
    setkey(ap_size, nwll)
    ap_size <- plate_dim[ap_size]
    ap_size[ , rown := max(rown, sub[ , max(rowi)])]
    ap_size[ , coln := max(coln, sub[ , max(coli)])]

    cat_name <- ap_size[ , paste0(toupper(idnm), aid)]
    apnm <- gtoxLoadApid("apid", ap)$u_boxtrack
    cat_name <- sprintf("%s: %s (%s)", cat_name, ap, apnm)

    if (is.null(quant)) {
        arng <- dat[get(idnm) == id, range(get(vlnm), na.rm=TRUE)]
    } else {
        arng <- dat[get(idnm) == id,
            quantile(get(vlnm), c(0.001, 0.999), na.rm=TRUE)]
    }

    with(
        data=sub[aid == id],
        .plateHeat(
            vals=val,
            rowi=rowi,
            coli=coli,
            wllt=nwlt,
            wllq=wllq,
            rown=ap_size[ , rown],
            coln=ap_size[, coln],
            main=cat_name,
            arng=arng
        )
    )

}

#-------------------------------------------------------------------------------
