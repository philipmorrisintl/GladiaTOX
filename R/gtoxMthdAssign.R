#####################################################################
## This program is distributed in the hope that it will be useful, ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of  ##
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    ##
## GNU General Public License for more details.                    ##
#####################################################################

#-------------------------------------------------------------------------------
# gtoxMthdAssign: Assign analysis method
#-------------------------------------------------------------------------------

#' @rdname mthd_funcs
#' 
#' @examples
#' 
#' \dontrun{
#' ## Assign level 2 methods (none for all acid values)
#' gtoxMthdAssign(lvl = 2L, id = 1L, mthd_id = 1, ordr = 1, type = "mc")
#' 
#' ## Process data
#' gtoxRun(asid = 1L, slvl = 1, elvl = 6, mc.cores = 2)
#' }
#' 
#' @export

gtoxMthdAssign <- function(lvl, id, mthd_id, ordr=NULL, type) {

    exec_ordr <- modified_by <- NULL

    if (length(lvl) > 1) stop("'lvl' must be an integer of length 1.")
    if (!type %in% c("mc", "sc")) stop("Invalid 'type' value.")
    if (type == "mc" & !lvl %in% c(2, 3, 5, 6)) stop("Invalid 'lvl' value.")
    if (type == "sc" & !lvl %in% seq_len(2)) stop("Invalid 'lvl' value.")

    id_name <- if (type == "mc" & lvl == 2) "acid" else "aeid"
    flds <- c(id_name, sprintf("%s%s_mthd_id", type, lvl))

    dat <- expand.grid(
        id=id,
        mthd=mthd_id,
        stringsAsFactors=FALSE
    )
    dat <- as.data.table(dat)

    if ((lvl < 4 & type == "mc") | (lvl == 1 & type == "sc")) {

        if (is.null(ordr) | length(mthd_id) != length(ordr)) {
            stop("'ordr' must be specified and the same length as 'mthd_id'")
        }

        dat[ , exec_ordr := ordr[match(get("mthd"), mthd_id)]]

    }

    setnames(dat, old=c("id", "mthd"), flds)

    mb <- paste(
        Sys.info()[c("login", "user", "effective_user")],
        collapse="."
    )
    dat[ , modified_by := mb]

    gtoxAppend(
        dat=dat,
        tbl=paste0(type, lvl, "_", flds[1]),
        db=getOption("TCPL_DB")
    )

    gtoxCascade(lvl=lvl, type=type, id=id)

}

#-------------------------------------------------------------------------------
