#####################################################################
## This program is distributed in the hope that it will be useful, ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of  ##
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    ##
## GNU General Public License for more details.                    ##
#####################################################################

#-------------------------------------------------------------------------------
# glCheckInput: check input file
#-------------------------------------------------------------------------------

#' @title Check validity of input file
#' @description This function check the structure and content of an input file.
#'
#' @param file file URL
#'
#' @details
#' This function is useful to check the structure and content of an input file
#' from the GladiaTOX GUI
#'
#' @return List of error messages in JSON format
#'
#' @importFrom RJSONIO toJSON
#' @export
#'

glCheckInput <- function(file){

    if(!file.exists(file))
        return(toJSON(list('status'='error', 'results'=list('File does not exists.'))))

    tab = try(read.csv(file), silent=TRUE)
    if(inherits(tab, "try-error"))
        return(toJSON(list('status'='error', 'results'=list('File is not in csv format.'))))

    colnames = c("stimulus", "concentration", "duration", "vehicle", "assay",
                 "endpoint", "channel", "plate", "tube", "tubetype",
                 "plateformat", "exposuredate", "val")

    if(!(all(colnames(tab)%in%colnames) & all(colnames%in%colnames(tab))))
        return(toJSON(list('status'='error', 'results'=list('File colnames are not corrct. See sample file.'))))
    
    messages = list()
    
    # Check concentration type
    if(any(is.na(as.numeric(unique(str_extract(tab$concentration, pattern='^[0-9]+'))))))
        messages[[length(messages)+1]] = "Concentrations must be of the form real number followed by a unit. Please check the concentration column."

    # Check duration type
    if(any(is.na(as.numeric(unique(str_extract(tab$duration, pattern='^[0-9]+'))))))
        messages[[length(messages)+1]] = "Duration must be of the form real number followed by a unit. Please check the duration column."

    # No vehicle defined
    if(!any(tab$tubetype%in%"n"))
        messages[[length(messages)+1]] = "No type n (vehicle) defined in column tubetype."

    # All stimuli must be associated to a vehicle
    vhls = unique(tab$vehicle[tab$tubetype%in%"n"])
    vhls_tab = unique(tab[, c("stimulus", "vehicle", "tubetype")])
    vhls_tab$has_vhl = vhls_tab$vehicle%in%vhls
    if(!all(vhls_tab$has_vhl)){
        str1 = "The following stimuli/vehicles are not associated to a vehicle"
        str2 = as.character(unique(vhls_tab$stimulus[!vhls_tab$has_vhl]))
        messages[[length(messages)+1]] = paste(c(str1, str2), collapse=", ")
    }

    # vehicles must appear in column stimulus
    tf = as.character(unique(vhls_tab$vehicle))%in%as.character(unique(vhls_tab$stimulus))
    if(!all(tf)){
        str1 = "The following vehicles do not appear in the stimulus column:"
        str2 = as.character(unique(vhls_tab$vehicle))[!tf]
        messages[[length(messages)+1]] = paste(c(str1, str2), collapse=", ")
    }
    
    # File with bad content
    if(length(messages) > 0)
        return(toJSON(list('status'='error', 'results'=messages)))
    
    # File OK, prepare sumamry table
    columns = colnames(tab)[-ncol(tab)]
    for(kk in 1:length(columns)){
        col = columns[kk]
        messages[[kk]] = list(name=col,
                               value=as.matrix(data.frame(name=names(sort(table(tab[[col]]))),
                                                          value=as.character(sort(table(tab[[col]]))), fix.empty.names=FALSE)))
    }
    
#    toJSON(list('status'='ok', 'results'=messages))
    return(toJSON(list('status'='ok', 'results'=messages)))
}






