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
#' name for the given assay element. For example, \code{gtoxLoadAsid} will 
#' return the assay source ID (asid) and assay source name (asnm). 
#' 
#' @examples
#' ## Store the current config settings, so they can be reloaded at the end 
#' ## of the examples
#' conf_store <- gtoxConfList()
#' gtoxConfDefault()
#' 
#' ## The load assay functions can be used without any parameters to list the 
#' ## full list of registered assay elements:
#' 
#' ## Assay source ID table
#' gtoxLoadAsid()
#' 
#' ## Assay ID table
#' gtoxLoadAid()
#' 
#' ## Assay component ID table
#' gtoxLoadAcid()
#' 
#' ## Assay endpoint ID table
#' gtoxLoadAeid()
#' 
#' ## Similarly, the user can add fields without doing any element selection:
#' gtoxLoadAeid(add.fld = c("asid", "aid", "acid"))
#' 
#' ## Or, the user can look only at a subset:
#' gtoxLoadAeid(fld = "aeid", val = 1, add.fld = "asid")
#' 
#' ## The field can be any value in one of the corresponding assay element
#' ## tables, but the functions also recognize the abbreviated version of
#' ## the name fields.
#' gtoxListFlds("assay")
#' a1 <- gtoxLoadAeid(fld = "anm", val = "Apo Necro (casp37)_4h")
#' a2 <- gtoxLoadAeid(fld = "assay_name", val = "Apo Necro (casp37)_4h")
#' identical(a1, a2)
#' 
#' ## Reset configuration
#' options(conf_store)
#' 
#' @return A data.table containing the ID, name, and any additional fields.
NULL
