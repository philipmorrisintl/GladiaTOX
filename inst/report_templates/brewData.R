## This script is intended to be sourced into the brew step of the report
## generation. Data used in all reports, and packages used by the brew step
## should be included in this script.

## Packages used by brew
library(xtable)

## Data to be loaded for the brew steps
asnm <- sanitize(gtoxLoadAsid("asid", asid)$asnm)
aes <- gtoxLoadAeid("asid", asid, 
                    c("acid", 
                      "acnm",
                      "aid",
                      "anm",
                      "signal_direction", 
                      "analysis_direction",
                      "timepoint_hr",
                      "assay_component_desc"))

l2 <- gtoxLoadData(2, "acid", aes[ , unique(acid)])

pcdat <- gtoxPrepOtpt(unique(l2[wllt == "c", list(spid, acid, apid)]))

aes <- merge(aes, 
             unique(pcdat[ , list(acid, pos_ctrl = chnm)]), 
             by = "acid", 
             all = TRUE)

aes[ , 
     clps := mapply(gsub, 
                    pattern = paste0("_", timepoint_hr, "h"), 
                    x = acnm, 
                    MoreArgs = list(replacement = ""))]
aes[ , acid := as.integer(acid)]
aes[ , clps_dir := paste0(clps, "_", sub("down", "dn", analysis_direction))]
aes[ , 
     clps_time := paste(unique(timepoint_hr[order(timepoint_hr)]), 
                        collapse = ", "),
     by = list(clps, assay_component_desc)]
aes[ , 
     clps_acid := paste(unique(acid[order(timepoint_hr)]), 
                        collapse = ", "),
     by = list(clps, assay_component_desc)]
aes[ , 
     clps_ctrl := paste(unique(pos_ctrl[order(timepoint_hr)]), 
                        collapse = ", "),
     by = list(clps, assay_component_desc)]
aes[ , 
     clps_anm := mapply(gsub, 
                        pattern = paste0("_", timepoint_hr, "h"), 
                        x = anm, 
                        MoreArgs = list(replacement = ""))]


l5 <- gtoxLoadData(5, "aeid", aes$aeid)
l5 <- gtoxPrepOtpt(l5)
l5_sngl <- gtoxSubsetChid(l5)

aes <- aes[aeid %in% l5_sngl[ , unique(aeid)]]
setkey(aes, aid)
save(l5, l5_sngl, aes, pcdat, file = "brewdat.RData")

