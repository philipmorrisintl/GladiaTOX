#####################################################################
## This program is distributed in the hope that it will be useful, ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of  ##
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    ##
## GNU General Public License for more details.                    ##
#####################################################################

#-------------------------------------------------------------------------------
# tcplLoadAsid: Load assay source id and name for the given fields
#-------------------------------------------------------------------------------

#' @rdname assay_funcs
#' @import data.table
#' @importFrom tcpl tcplQuery
#' @export

tcplLoadAsid <- function(fld=NULL, val=NULL, add.fld=NULL) {

    out <- c(
        "assay_source.asid",
        "assay_source.assay_source_name",
        "assay_source.assay_source_phase"
    )

    qstring <- .buildAssayQ(
        out=out,
        tblo=c(6, 4:1),
        fld=fld,
        val=val,
        add.fld=add.fld
    )

    dat <- tcplQuery(query=qstring, db=getOption("TCPL_DB"))

    dat[]

}

#-------------------------------------------------------------------------------
