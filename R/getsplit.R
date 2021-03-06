#####################################################################
## This program is distributed in the hope that it will be useful, ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of  ##
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    ##
## GNU General Public License for more details.                    ##
#####################################################################

#-------------------------------------------------------------------------------
# getsplit: supporting function
#-------------------------------------------------------------------------------

getsplit <- function(nm0, splitarg, k=1, remove=FALSE, fixed=TRUE,
    last=FALSE, last.n=1) {

    nm0 <- as.character(nm0)
    if (last == FALSE) {
        if (remove == FALSE) {
            y <- vapply(strsplit(nm0, splitarg, fixed=fixed),
                        function(x) {
                            paste(x[k], collapse=splitarg, sep="")
                        }, character(1))
        }
        if (remove == TRUE) {
            y <- vapply(
                strsplit(nm0, splitarg, fixed=fixed),
                function(x) {
                    paste(x[-k], collapse=splitarg, sep="")
                }, character(1)
            )
        }
    }
    else {
        if (remove == FALSE) {
            y <- vapply(
                strsplit(nm0, splitarg, fixed=fixed),
                function(x) {
                    paste(
                        x[(length(x) - last.n + 1):length(x)],
                        collapse=splitarg, sep= ""
                    )
                }, character(1)
            )
        }
        if (remove == TRUE) {
            y <- vapply(
                strsplit(nm0, splitarg, fixed=fixed),
                function(x) {
                    paste(
                        x[-c((length(x) - last.n + 1):length(x))],
                        collapse=splitarg, sep=""
                    )
                }, character(1)
            )
        }
    }
    names(y) <- names(nm0)
    return(y)

}
