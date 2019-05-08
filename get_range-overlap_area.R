library(sp)
library(rgdal)
library(rgeos)

load('data/wrange.RData')

# reproject to equal area with km units
wrange <- spTransform(wrange, CRS('+proj=cea +lon_0=-90 +lat_ts=0 +x_0=0 +y_0=0 +datum=WGS84 +units=km +no_defs'))

# function to calculate area of overlap
intersectionArea <- function(spgeom1, spgeom2) {
    # do the bounding boxes overlap
    xlap <- bbox(spgeom1)[1, 1] <= bbox(spgeom2)[1, 2] & bbox(spgeom2)[1, 1] <= bbox(spgeom1)[1, 2]
    ylap <- bbox(spgeom1)[2, 1] <= bbox(spgeom2)[2, 2] & bbox(spgeom2)[2, 1] <= bbox(spgeom1)[2, 2]
    bboxOver <- xlap & ylap
    
    a1 <- rgeos::gArea(spgeom1)
    a2 <- rgeos::gArea(spgeom2)
    
    if(bboxOver) {
        # if so calculate intersection
        int <- rgeos::gIntersection(spgeom1, spgeom2)
        
        if(is.null(int)) {
            return(c(area1 = a1, area2 = a2, intersection = 0))
        } else {
            # return the average of the overlaps from each spp perspective...or not, let
            # that be caulculated with the output of the fun
            # return(rgeos::gArea(int) / 2 * 
            #            (1 / rgeos::gArea(spgeom1) + 1 / rgeos::gArea(spgeom2)))
            
            # return the actual overlap in addition to the two areas
            return(c(area1 = a1, area2 = a2, intersection = rgeos::gArea(int)))
        }
    } else {
        return(c(area1 = a1, area2 = a2, intersection = 0))
    }
}


# make all sp-sp comparisons
allComp <- outer(wrange$name, wrange$name, paste, sep = '_')
allComp <- allComp[lower.tri(allComp)]
allComp <- do.call(rbind, strsplit(allComp, '_'))
colnames(allComp) <- c('species1', 'species2')


# loop through calculating overlap
allROver <- parallel::mclapply(1:nrow(allComp), mc.cores = 10, mc.preschedule = FALSE, 
                               FUN = function(i) {
                                   print(i)
                                   thisComp <- allComp[i, ]
                                   out <- try(intersectionArea(wrange[wrange$name == thisComp[1], ], 
                                                               wrange[wrange$name == thisComp[2], ]), 
                                              silent = TRUE)
                                   
                                   if(class(out) == 'try-error') {
                                       print(paste(i, out, sep = ': '))
                                       return(c(area1 = NA, area2 = NA, intersection = NA))
                                   } else {
                                       return(out)
                                   }
})

allROver <- as.data.frame(cbind(allComp, as.data.frame(do.call(rbind, allROver))))

# calculate various proportional overlaps and differences
allROver$prop_interA1 <- allROver$intersection / allROver$area1
allROver$prop_interA2 <- allROver$intersection / allROver$area2
allROver$prop_interAvg <- (allROver$prop_interA1 + allROver$prop_interA2) / 2
allROver$diff <- abs(allROver$area1 - allROver$area2)
allROver$prop_diff <- allROver$diff / (allROver$area1 + allROver$area2 - allROver$intersection)

# write out
save(allROver, file = 'data/allROver.RData')
