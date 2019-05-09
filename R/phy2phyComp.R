#' @title Plot all pairwise phylogenetic comparisons 
#'
#' @description Plot all pairwise phylogenetic comparisons 
#'
#' @param tre the phylogeny
#' @param dat matrix of data for all pairwise comparisons
#' @param slab scale lable
#' @param ncolz number of colors for image plot
#' @example 
#' a <- read.tree(text = '(a:3, (b:2, (c:1, d:1):1):1);')
#' a <- rotate(a, 6)
#' b <- cophenetic(a)
#' phy2phyComp(a, b, slab = 'foo')

#' @author A J Rominger <ajrominger@@gmail.com>
#' 
#' @export

phy2phyComp <- function(tre, dat, slab = '', ncolz = 12) {
    colz <- viridis::viridis(12)
    # plot(tre)
    # tipz <- get('last_plot.phylo', envir = .PlotPhyloEnv)$yy[1:Ntip(tre)]
    # dat <- dat[order(tipz), order(tipz)]
    yy <- tre$edge[tre$edge[, 2] <= Ntip(tre), 2]
    dat <- dat[yy, yy]
    
    layout(matrix(c(0, 3, 0, 4, 1, 5, 0, 2, 0), nrow = 3), widths = c(2, 3, 1.5), heights = c(2, 3, 1.5))
    
    par(mar = rep(0, 4), cex = 1.05)
    image(1:Ntip(tre), 1:Ntip(tre), dat[nrow(dat):1, ], axes = FALSE, col = colz)
    box()
    lim <- par('usr')[1:2]
    
    par(mar = rep(0, 4))
    plot(1, type = 'n', axes = FALSE, ylim = lim, yaxs = 'i')
    text(rep(par('usr')[1], Ntip(tre)), 1:Ntip(tre), labels = tre$tip.label, pos = 4)
    
    par(mar = rep(0, 4))
    plot(tre, yaxs = 'i', y.lim = lim, show.tip.label = FALSE)
    
    par(mar = rep(0, 4))
    plot(tre, direction = 'downwards', x.lim = rev(lim), xaxs = 'i', show.tip.label = FALSE)
    
    par(mar = c(2.5, 0, 1, 0), mgp = c(1.2, 0.2, 0), tcl = - 0.2)
    plot(1, type = 'n', xlim = range(dat), xaxs = 'i', axes = FALSE, xlab = slab)
    xx <- seq(min(dat), max(dat), length.out = length(colz) + 1)
    rect(xleft = xx[-length(xx)], xright = xx[-1], 
         ybottom = par('usr')[3], ytop = par('usr')[4], 
         col = colz, border = NA)
    box()
    axis(1)
}
