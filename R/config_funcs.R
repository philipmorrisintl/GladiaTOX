#####################################################################
## This program is distributed in the hope that it will be useful, ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of  ##
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    ##
## GNU General Public License for more details.                    ##
#####################################################################

#' @name Configure functions
#' @rdname config_funcs
#' @title Functions for configuring the gtox package
#'
#' @description
#' These functions are used to configure the gtox settings.
#' 
#' @param drvr Character of length 1, which database driver to use
#' @param user Character of length 1, the database server username
#' @param pass Character of length 1, the database server password
#' @param host Character of length 1, the database server 
#' @param db   Character of length 1, the name of the gtox database
#' @param show.pass Logical, should the password be returned
#' 
#' @details
#' Currently, the gtox package only supports the "MySQL" and "SQLite" database
#' drivers.
#'
#' The settings can be stored in a configuration file to make the using the 
#' package more user-friendly. To create the configuration file, the user must
#' first create a system environment variable ('TCPL_CONF') that points to to 
#' the file. There is more information about system environment variables in
#' \code{\link{Startup}} and \code{\link{Sys.getenv}}. Briefly, the user 
#' needs to modify the '.Renviron' file in their home directory. If the file
#' does not exist, create it, and add the following line:
#' 
#' TCPL_CONF=path/to/confFile.conf
#' 
#' Here 'path/to/confFile.conf' can be any path to a file. One suggestion would
#' be to include gtoxConf in the home directory, eg. TCPL_CONF=~/gtoxConf. 
#' Note, '~' may not indicate the home directory on every operating system.
#' Once the environment variable is added, the user can change the settings
#' using \code{gtoxConf}, then save the settings to the file given by the
#' TCPL_CONF environment variable running \code{gtoxConfSave()}. 
#' 
#' @examples 
#' gtoxConfList() # List configuration parameters
#' 
#' @return None
#' 
#' \code{gtoxConf} changes \code{options} to set the gtox-specific options, 
#' most importantly to configure the connection to the gtox databases. 
#' \code{gtoxConf} will only change non-null values, and can be used to 
#' change a single value if needed. 
#' 
#' \code{gtoxConfSave} modifies the configuration file to reflect the current
#' gtox settings.
#' 
#' \code{gtoxConfList} lists the values assigned to the gtox global options.
#' 
#' \code{gtoxConfLoad} updates the gtox settings to reflect the current 
#' configuration file.
#' 
#' \code{gtoxConfDefault} changes the \code{options} to reflect the default
#' settings for the example SQLite database, but does not alter the 
#' configuration file.
#' 
#' \code{gtoxConfReset} is used to generate the initial configuration script,
#' and can be used to reset or regenerate the configuration script by the user.
NULL
