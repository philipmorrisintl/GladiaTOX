#####################################################################
## This program is distributed in the hope that it will be useful, ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of  ##
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    ##
## GNU General Public License for more details.                    ##
#####################################################################

#' @name Method functions
#' @rdname mthd_funcs
#' @title Functions for managing processing methods
#'
#' @description
#' These functions are used to manage which methods are used to process data.
#' They include methods for assigning, clearing, and loading the assigned 
#' methods. Also, \code{gtoxMthdList} lists the available methods. 
#' 
#' @param lvl Integer of length 1, the method level
#' @param id Integer, the assay component or assay endpoint id(s)
#' @param mthd_id Integer, the method id(s)
#' @param ordr Integer, the order in which to execute the analysis methods,
#' must be the same length as mthd_id, does not apply to levels 5 or 6
#' @param type Character of length 1, the data type, "sc" or "mc"
#' 
#' @details
#' \code{gtoxMthdLoad} loads the assigned methods for the given level and 
#' ID(s). Similarly, \code{gtoxMthdList} displays the available methods for 
#' the given level. These two functions do not make any changes to the 
#' database.
#' 
#' Unlike the \code{-Load} and \code{-List} functions, the \code{-Assign} and 
#' \code{-Clear} functions alter the database and trigger a delete cascade. 
#' \code{gtoxMthdAssign} assigns methods to the given ID(s), and 
#' \code{gtoxMthdClear} removes methods. In addition to the method ID 
#' ('mthd_id'), assigning methods at some levels require an order ('ordr'). 
#' The 'ordr' parameter is necessary to allow progression of methods at level
#' one for single-concentration processing, and levels two and three for 
#' multiple-concentration processing. More information about method assignments 
#' and the delete cascade are available in the package vignette. 
#' 
#' @examples 
#' ## Store the current config settings, so they can be reloaded at the end 
#' ## of the examples
#' conf_store <- gtoxConfList()
#' gtoxConfDefault()
#' 
#' ## gtoxListMthd allows the user to display the available methods for 
#' ## a given level and data type
#' head(gtoxMthdList(lvl = 2, type = "mc"))
#' 
#' ## gtoxLoadMthd shows which methods are assigned for the given ID, level,
#' ## and data type. Here we will show how to register, load, and clear methods
#' ## using an acid not in the example database. Note: There is no check for
#' ## whether an ID exists before assigning/clearing methods. 
#' gtoxMthdLoad(lvl = 2, id = 1, type = "mc")
#' 
#' ## Reset configuration
#' options(conf_store)
#' 
#' @return None
#' 
#' 
NULL
