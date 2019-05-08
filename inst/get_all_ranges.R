library(sp)
library(rgdal)

source('R/extractSppBirdLife.R')

syn <- read.csv('data/lovette_taxSyn.csv', as.is = TRUE)

# the path to BOTW.gdb is machine-spacific and must be set by each user
botwPath <- '~/Dropbox/Research/data/BirdLife_HBW_Bird_Maps/BOTW.gdb'
extractSppBirdLife(botwPath, 
                   'data', 'botw_ranges',  # change data to inst in all cases but final line
                   syn$botw_name, 
                   c(1, 2), mc.cores = 10)

# read back in the extracted ranges
wrange <- readOGR(path.expand('data/botw_ranges.shp'), 'botw_ranges')

# aggregate all ranges within a species
wrange <- aggregate(wrange, by = list(name = wrange$SCINAME), FUN = paste, sep = '; ', 
                    dissolve = TRUE)

# retain only neccesary columns
wrange@data <- wrange@data[, c('name', 'SISID', 'DATE_', 'SOURCE', 'COMPILER', 
                               'REVIEWERS', 'CITATION', 'VERSION')]
names(wrange) <- c('name', 'SISID', 'date', 'source', 'compiler', 'reviewers', 
                   'citation', 'version')

# write out
save(wrange, file = 'data/wrange.RData')
