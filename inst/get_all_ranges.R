library(sp)
library(rgdal)

source('R/extractSppBirdLife.R')

syn <- read.csv('data/lovette_taxSyn.csv', as.is = TRUE)

extractSppBirdLife('~/Dropbox/Research/data/BirdLife_HBW_Bird_Maps/BOTW.gdb', 
                   '~/Dropbox/Research/warbler/data', 'botw_ranges', 
                   syn$botw_name, 
                   c(1, 2), mc.cores = 10)


wrange <- readOGR(path.expand('data/botw_ranges.shp'), 'botw_ranges')
wrange <- aggregate(wrange, by = list(name = wrange$SCINAME), FUN = paste, sep = '; ', 
                    dissolve = TRUE)
wrange@data <- wrange@data[, c('name', 'SISID', 'DATE_', 'SOURCE', 'COMPILER', 
                               'REVIEWERS', 'CITATION', 'VERSION')]
names(wrange) <- c('name', 'SISID', 'date', 'source', 'compiler', 'reviewers', 
                   'citation', 'version')

save(wrange, file = 'data/wrange.RData')
