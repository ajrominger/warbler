library(sp)
library(rgdal)
library(rgeos)

load('data/wrange.RData')

intersectionArea <- function(spgeom1, spgeom2) {
    int <- rgeos::gIntersection(spgeom1, spgeom2)
    
    if(is.null(int)) {
        return(0)
    } else {
        return(sum(sapply(int@polygons, function(x) x@area)))
    }
}


intersectionArea(foo[2, ], foo[4, ])

foo <- wrange[wrange$name %in% c('Setophaga occidentalis', 'Setophaga townsendi', 'Setophaga pinus', 'Setophaga fusca'), ]
foo

bla <- gIntersection(foo[c(1, 2), ], foo[c(3, 4), ], byid = TRUE)
bla@polygons[[2]]@area
