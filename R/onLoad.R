#####################################################################
## This program is distributed in the hope that it will be useful, ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of  ##
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    ##
## GNU General Public License for more details.                    ##
#####################################################################

.onLoad <- function(libname, pkgname) {

    conf_file <- file.path(system.file(package="GladiaTOX"), "TCPL.config")

    if (file.exists(conf_file)) {

        tcplConfLoad()

    } else {

        tcplConfReset()
        tcplConfLoad()

    }

}
