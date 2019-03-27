#####################################################################
## This program is distributed in the hope that it will be useful, ##
## but WITHOUT ANY WARRANTY; without even the implied warranty of  ##
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    ##
## GNU General Public License for more details.                    ##
#####################################################################

#-------------------------------------------------------------------------------
# glPlotPieLogo: plot GladiaTOX logo
#-------------------------------------------------------------------------------

#' @title plot package logo
#' @description This function plots the GladiaTOX logo.
#'
#' @details
#' This funtion is only used to plot the package logo.
#'
#' @examples
#' glPlotPieLogo()
#' 
#' @return None
#'
#' @import ggplot2
#' @import RColorBrewer
#' @export
#'

glPlotPieLogo <- function(){

    nslices = 32
    maxval = 10
    data = NULL
    data$slice = rep(paste0("slice",seq_len(nslices)),3)
    data$value = c(
        c(rep(6.75, 5), rep(0, 12), 5, 5.5, seq(4, 7, by=(7-4)/12)[-13], 6.75),
        c(rep(0, 5), 7.5, 7.5, rep(0, 12), seq(0, 2.5, by=2.5/11), 0),
        c(rep(0, 19), seq(0, 1, by=1/11), 0))
    data$layer <- c(rep("l3",nslices), rep("l2",nslices), rep("l1",nslices))
    data = as.data.frame(data)
    data$slice = factor(data$slice, levels = paste0("slice",seq_len(nslices)))
    
    yintercept <- 0:maxval
    
    colfunc <- colorRampPalette(c("brown", "red"))
    
    before = 47; after = 48
    logo = ggplot(data, aes(x = slice, y = value)) +
        geom_hline(color="gray", yintercept = yintercept, size=.3, 
                linetype="dashed") +
        geom_bar(stat="identity", aes(fill = layer), color="gray60") +
        geom_hline(color="white", yintercept = 0.5, size=5, linetype="solid") +
        scale_fill_manual(values = c("#533B22", "#917546", "#33190C")) +
        theme_minimal() +
        theme(
            panel.grid.major.y = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            legend.text = element_blank(),
            legend.title = element_blank(),
            legend.position = "none",
            strip.text.x = element_blank(),
            strip.text.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.x = element_blank()) +
        geom_label(
            aes(label = c(rep("", before), "GladiaTOX", rep("", after)),
                y = c(rep(0, before), 5, rep(0, after))),
            colour = c(rep("white", before), "white", rep("white", after)), 
            size = c(rep(0, before), 18, rep(0, after)),
            fontface = "bold", family = "mono", fill = "#533B22") +
        coord_polar()
    
    ## png("logo.png")
    print(logo)
    ## dev.off()
}

