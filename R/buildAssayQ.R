#####################################################################
## This program is distributed in the hope that it will be useful, ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of  ##
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    ##
## GNU General Public License for more details.                    ##
#####################################################################

#-------------------------------------------------------------------------------
# buildAssayQ: Generate query for assay information
#-------------------------------------------------------------------------------

#' @title Generate query for assay information
#' 
#' @description 
#' \code{.buildAssayQ} generates a query string to load assay information
#' 
#' @param out Character, the default fields to include
#' @param tblo Integer, the order to send the fields to prepOutput
#' @param fld Character, the field(s) to query/subset on
#' @param val List, vectors of values for each field to query/subset on. Must 
#' be in the same order as 'fld'.
#' @param add.fld Character, additional field(s) to include, but not query/
#' subset on
#' 
#' @return A character containing the query to send to tcplQuery
#' 
#' @import data.table

.buildAssayQ <- function(out, tblo, fld=NULL, val=NULL, add.fld=NULL) {
    qstring <- tcpl:::.buildAssayQ(out, tblo, fld=NULL, val=NULL, add.fld=NULL)
    sub("assay_source_phase", "assay_source_phase AS asph", qstring)
    qstring
}


#-------------------------------------------------------------------------------
