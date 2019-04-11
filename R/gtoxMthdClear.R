#####################################################################
## This program is distributed in the hope that it will be useful, ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of  ##
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    ##
## GNU General Public License for more details.                    ##
#####################################################################

#-------------------------------------------------------------------------------
# gtoxMthdClear: Clear analysis method(s)
#-------------------------------------------------------------------------------

#' @rdname mthd_funcs
#' 
#' @examples
#' 
#' \dontrun{
#' ## Clear level 2 methods
#' gtoxMthdClear(lvl = 2L, id = 1L, mthd_id = NULL, type = "mc") 
#' 
#' ## Assign level 2 methods (none for all acid values)
#' gtoxMthdAssign(lvl = 2L, id = 1L, mthd_id = 1, ordr = 1, type = "mc")
#' 
#' ## Process data
#' gtoxRun(asid = 1L, slvl = 1, elvl = 6, mc.cores = 2)
#' }
#' 
#' @export

gtoxMthdClear <- function(lvl, id, mthd_id=NULL, type) {

    if (length(lvl) > 1) stop("'lvl' must be an integer of length 1.")
    if (!type %in% c("mc", "sc")) stop("Invalid 'type' value.")
    if (type == "mc" & !lvl %in% c(2, 3, 5, 6)) stop("Invalid 'lvl' value.")
    if (type == "sc" & !lvl %in% seq_len(2)) stop("Invalid 'lvl' value.")

    fld <- if (type == "mc" & lvl == 2) "acid" else "aeid"
    tbl <- paste0(type, lvl, "_", fld)
    if (!is.null(mthd_id)) {
        fld <- c(fld, sprintf("%s%s_mthd_id", type, lvl))
        val <- list(id, mthd_id)
    } else {
        val <- id
    }

    gtoxDelete(tbl=tbl, fld=fld, val=val, db=getOption("TCPL_DB"))

    gtoxCascade(lvl=lvl, type=type, id=id)

}

#-------------------------------------------------------------------------------
