#####################################################################
## This program is distributed in the hope that it will be useful, ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of  ##
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    ##
## GNU General Public License for more details.                    ##
#####################################################################

#' @name Load assay information
#' @rdname assay_funcs
#' @title Functions for loading assay information
#'
#' @description 
#' These functions query the gtox databases and returns a data.table with 
#' assay ID and name information. More information about the assay
#' hierarchy is available in the overview vignette.
#' 
#' @param fld Character, the field(s) to query/subset on
#' @param val List, vectors of values for each field to query/subset on. Must 
#' be in the same order as 'fld'.
#' @param add.fld Character, additional field(s) to include, but not query/
#' subset on
#' 
#' @details
#' Each element in the assay hierarchy has its own function, loading the ID and
#' name for the given assay element. For example, \code{tcplLoadAsid} will 
#' return the assay source ID (asid) and assay source name (asnm). 
#' 
#' @examples
#' ## Store the current config settings, so they can be reloaded at the end 
#' ## of the examples
#' conf_store <- tcplConfList()
#' tcplConfDefault()
#' 
#' ## The load assay functions can be used without any parameters to list the 
#' ## full list of registered assay elements:
#' tcplLoadAsid()
#' tcplLoadAeid()
#' 
#' ## Similarly, the user can add fields without doing any element selection:
#' tcplLoadAeid(add.fld = c("asid", "aid", "acid"))
#' 
#' ## Or, the user can look only at a subset:
#' tcplLoadAeid(fld = "aeid", val = 1, add.fld = "asid")
#' 
#' ## The field can be any value in one of the corresponding assay element
#' ## tables, but the functions also recognize the abbreviated version of
#' ## the name fields.
#' tcplListFlds("assay")
#' a1 <- tcplLoadAeid(fld = "anm", val = "Apo Necro (casp37)_4h")
#' a2 <- tcplLoadAeid(fld = "assay_name", val = "Apo Necro (casp37)_4h")
#' identical(a1, a2)
#' 
#' ## Reset configuration
#' options(conf_store)
#' 
#' @return A data.table containing the ID, name, and any additional fields.
#' 
#' @importFrom tcpl tcplConfList
NULL
