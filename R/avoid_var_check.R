#' \code{GladiaTOX} package
#'
#' @docType package
#' @name GladiaTOX
#' 
#' @importFrom utils globalVariables
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1"){
    globalVariables(c("waid", "u_smkid", "apid", "u_boxtrack", "s_sampleid"))
    globalVariables(c("normalized_data_type", "analysis_direction", "aeid"))
    globalVariables(c("modl_rmse", "aenm", ".", "chnm", "anm", "V1", "acid", 
                    "expTm", "stat_val", "stat_text", "slice", "value",
                    "wllt", "resp", "PosCtr_cntn", "date_treat",
                    "date_harvest", "inOrdr", "hr_barcode", "asnm",
                    "cell_type", "endpoint", "timepoint_hr", "bior", "spid",
                    "vehicle_name", "asph", "timepoint", "tu", "tmult",
                    "assay_footprint"))
    globalVariables(c("chid", "asid", "aid", "acnm", "machine_name", 
                    "burst_assay", "fit_all", "gui_sample_id", "well", "rowi",
                    "coli", "conc", "vhid", "J", "gtoxIDs", "rval",
                    "measure_val", "wllq", "chem_desc", "add_info", "logc",
                    "mean_resp", "tc", "GRP", "mom", "sem", "pot", "modl_acb",
                    "ptmn", "ptsd", "tmn", "up", "nsmp", "nwlt", "adir", 
                    "true_resp", "bmad", "coff"))
    globalVariables(c("hill_tp", "hill_ga", "hill_gw", "gnls_ga", "gnls_gw",
                    "gnls_la", "gnls_lw", "gnls_tp"))
    globalVariables(c("hill_rmse", "hill_prob", "hill_aic", "gnls_rmse", 
                    "gnls_prob", "gnls_aic", "gnls_lw_sd", "gnls_tp_sd",
                    "hill_tp_sd", "hill_ga_sd", "hill_gw_sd", "gnls_la_sd",
                    "gnls_gw_sd", "gnls_ga_sd", "resp_min", "resp_max", "val"))
    globalVariables(c("casn", "m4id", "cnst_aic", "cnst_prob", "cnst_rmse"))
}