#####################################################################
## This program is distributed in the hope that it will be useful, ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of  ##
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    ##
## GNU General Public License for more details.                    ##
#####################################################################

#' @name Query functions
#' @rdname query_funcs
#' @title Wrappers for sending queries and fetching results
#'
#' @description 
#' These functions send a query to the given database, and are the access point
#' for all gtox functions that query or update the gtox database.
#' 
#' @param query Character of length 1, the query string
#' @inheritParams gtoxConf
#' 
#' @details
#' Currently, the gtox package only supports the "MySQL" and "SQLite" database
#' drivers.
#' 
#' \code{gtoxQuery} returns a data.table object with the query results.
#' \code{gtoxSendQuery} sends a query, but does not fetch any results, and 
#' returns 'TRUE' or the error message given by the database. 
#' 
#' @examples
#' 
#' ## Store the current config settings, so they can be reloaded at the end 
#' ## of the examples
#' conf_store <- gtoxConfList()
#' gtoxConfDefault()
#' 
#' gtoxQuery("SELECT 'Hello World';")
#' gtoxQuery("SELECT * FROM assay;")
#'  
#' ## Reset configuration
#' options(conf_store)
#' 
#' @return None
#' 
NULL
