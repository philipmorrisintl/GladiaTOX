#####################################################################
## This program is distributed in the hope that it will be useful, ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of  ##
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    ##
## GNU General Public License for more details.                    ##
#####################################################################

#-------------------------------------------------------------------------------
# .load6DR: Load data for gtox6
#-------------------------------------------------------------------------------

#' @title Load data for gtox6
#'
#' @description
#' \code{.load6DR} loads dose-response data for gtox6.
#' 
#' @param ae Assay endpoint
#' @return Data.table with level 6 gtox info

.load6DR <- function(ae) {

    qformat <-
        "
        SELECT
            mc4_agg.aeid,
            mc4_agg.m4id,
            mc4_agg.m3id,
            spid,
            logc,
            rval,
            resp,
            apid,
            rowi,
            coli,
            wllt,
            cndx,
            repi
        FROM
            mc0,
            mc1,
            mc3,
            mc4_agg
        WHERE
            mc3.m3id = mc4_agg.m3id
            AND
            mc1.m1id = mc4_agg.m1id
            AND
            mc1.m0id = mc0.m0id
            AND
            mc0.m0id = mc4_agg.m0id
            AND
            mc4_agg.aeid = %s;
        "

    qstring <- sprintf(qformat, ae)

    dat <- gtoxQuery(query=qstring, db=getOption("TCPL_DB"))

    dat[]

}

#-------------------------------------------------------------------------------
