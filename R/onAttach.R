#####################################################################
## This program is distributed in the hope that it will be useful, ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of  ##
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    ##
## GNU General Public License for more details.                    ##
#####################################################################

.onAttach <- function(libname, pkgname) {

    v <- tcplConfList()
    p <- names(v)
    pn <- vapply(p, nchar, integer(1))
    sep <- vapply(
        pn, function(x) paste(rep(" ", 11 - x), collapse=""),
        character(1)
    )
    sep <- paste0(":", sep)
    cs <- vapply(
        seq_along(v), function(x) paste(p[x], v[[x]], sep=sep[x]),
        character(1)
    )

    packageStartupMessage(
        "GladiaTOX (v",
        as.character(utils::packageVersion("GladiaTOX")),
        ") loaded with the following settings:\n  ",
        paste(cs, collapse="\n  "),
        "\nDefault settings stored in TCPL.conf. See ",
        "?tcplConf for more information."
    )

}
