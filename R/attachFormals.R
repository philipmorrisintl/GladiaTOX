#####################################################################
## This program is distributed in the hope that it will be useful, ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of  ##
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    ##
## GNU General Public License for more details.                    ##
#####################################################################

.attachFormals <- function(fun, envir=NULL) {
    if (is.null(envir)) envir <- parent.frame()
    f <- formals(deparse(substitute(fun)))
    f <- lapply(f, function(x) if(!is(x, "name")) eval(x) else x)
    mapply(assign, names(f), f, MoreArgs=list(envir=envir))
    NULL
}
