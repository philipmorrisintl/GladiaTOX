#####################################################################
## This program is distributed in the hope that it will be useful, ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of  ##
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    ##
## GNU General Public License for more details.                    ##
#####################################################################

.onLoad <- function(libname, pkgname) {
    conf_file <- Sys.getenv("TCPL_CONF")
    if (file.exists(conf_file)) {
        loadConf <- try(gtoxConfLoad(list.new=FALSE))
        if (is(loadConf, 'try-error')) {
            warning(
                "The configuration file given by 'TCPL_CONF' could not be ",
                "loaded. Using default settings. Please see ?gtoxConf for ",
                "more information."
            )
            gtoxConfDefault()
        }
    } else {
        gtoxConfDefault()
    }
}
