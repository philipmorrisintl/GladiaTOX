#####################################################################
## This program is distributed in the hope that it will be useful, ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of  ##
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    ##
## GNU General Public License for more details.                    ##
#####################################################################

#-------------------------------------------------------------------------------
# tcplLoadAcid: Load assay component id and name for the given fields
#-------------------------------------------------------------------------------

#' @rdname assay_funcs
#' @import data.table
#' @export

tcplLoadAcid <- function(fld=NULL, val=NULL, add.fld=NULL) {

    out <- c(
        "assay_component.acid",
        "assay_component.assay_component_name"
    )

    qstring <- .buildAssayQ(
        out=out,
        tblo=c(1, 2, 4, 6, 3),
        fld=fld,
        val=val,
        add.fld=add.fld
    )

    dat <- tcplQuery(query=qstring, db=getOption("TCPL_DB"))

    dat[]

}

#-------------------------------------------------------------------------------
