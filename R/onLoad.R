#####################################################################
## This program is distributed in the hope that it will be useful, ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of  ##
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    ##
## GNU General Public License for more details.                    ##
#####################################################################

#' @importFrom tcpl tcplConfLoad

.onLoad <- function(libname, pkgname) {
    
    assignInNamespace("tcplLoadData", tcplLoadData, ns="tcpl")
    assignInNamespace("mc1", mc1, ns="tcpl")
    assignInNamespace("mc3_mthds", mc3_mthds, ns="tcpl")
    assignInNamespace("mc5_mthds", mc5_mthds, ns="tcpl")
    assignInNamespace("mc6_mthds", mc6_mthds, ns="tcpl")
    assignInNamespace("tcplListFlds", tcplListFlds, ns="tcpl")
    assignInNamespace("tcplQuery", tcplQuery, ns="tcpl")
    assignInNamespace("tcplDelete", tcplDelete, ns="tcpl")
    assignInNamespace("tcplAppend", tcplAppend, ns="tcpl")
    assignInNamespace("mc4", mc4, ns="tcpl")
    
    conf_file <- Sys.getenv("TCPL_CONF")
    if (file.exists(conf_file)) {
        loadConf <- try(tcplConfLoad(list.new=FALSE))
        if (is(loadConf, 'try-error')) {
            warning(
                "The configuration file given by 'TCPL_CONF' could not be ",
                "loaded. Using default settings. Please see ?tcplConf for ",
                "more information."
            )
            tcplConfDefault()
        }
    } else {
        tcplConfDefault()
    }
}
