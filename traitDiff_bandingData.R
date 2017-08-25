## a script to look at trait (including time) differences between species 
## co-occuring at a site

library(plyr)

setwd('~/Dropbox/Research/warbler')

allDat <- read.csv('birdBandingData/birdBanding_clean.csv', as.is = TRUE)
allDat$BANDING_DATE <- as.Date(allDat$BANDING_DATE, '%m/%d/%y')

traits <- c(gsub('good_', '', names(allDat)[grep('good_', names(allDat))]), 
            'BANDING_DATE')
allDat$good_BANDING_DATE <- 1

## all pairwise spp comparisons
sppComp <- array(dim = rep(length(unique(allDat$SPECIES_ID)), 2))
sppComp <- which(upper.tri(sppComp), arr.ind = TRUE)
sppComp <- cbind(unique(allDat$SPECIES_ID)[sppComp[, 1]], 
                 unique(allDat$SPECIES_ID)[sppComp[, 2]])
colnames(sppComp) <- c('sp1', 'sp2')

## function to calculate differences (Welch's t stat) between all spp 
## for a certain trait
#' @param dat the data set to use
#' @param x the trait of interest

traitDiff <- function(dat, x) {
    out <- lapply(1:nrow(sppComp), function(i) {
        x1 <- dat[dat$SPECIES_ID == sppComp[i, 1] &
                      dat[, paste0('good_', x)] == 1, 
                  x]
        x2 <- dat[dat$SPECIES_ID == sppComp[i, 2] &
                      dat[, paste0('good_', x)] == 1, 
                  x]
        
        x1 <- x1[!is.na(x1)]
        x2 <- x2[!is.na(x2)]
        
        if(length(x1) > 0 & length(x2) > 0) {
            return((mean(x1) - mean(x2)) / 
                       sqrt(var(x1)/length(x1) + var(x2)/length(x2)))
        } else {
            return(NA)
        }
    })
    
    return(cbind(sppComp, diff = unlist(out)))
}

# traitDiff(allDat, 'TAIL_LENGTH')

## function to divide data by site and calculate all trait differences
#' @param dat the data to be used

allTraitDiff <- function(dat) {
    ddply(dat, 'LOCATION_ID', function(d) {
        out <- lapply(traits, function(tr) {
            data.frame(trait = tr, traitDiff(d, tr))
        })
        
        return(do.call(rbind, out))
    })
}

foo <- allTraitDiff(allDat)
foo <- foo[!is.na(foo$diff), ]
