## script to read in multiple data files from bird banding data, 
## clean them and combine them into one tidy data.frame

setwd('~/Dropbox/Research/warbler')

## where data live
dataWD <- 'birdBandingData'

## desired columns
taxaCols <- c('SPECIES_ID', 'SPECIES_NAME')

morphoCols <- c('BILL_HEIGHT', 'BILL_LENGTH', 'BILL_WIDTH', 'BIRD_WEIGHT',
                'CULMEN_LENGTH', 'TAIL_LENGTH', 'TARSUS_LENGTH', 'WING_CHORD')

covCols <- c('AGE_CODE', 'FLIGHT_FEATHER_MOLT', 'MOLT_CYCLE_CODE', 'WEIGHT_TIME', 
             'CAPTURE_TIME', 'FAT_SCORE')

locDateCols <- c('LOCATION_ID', 'BANDIT_LOCATION_ID', 'B_LOCATION_DESCRIPTION', 
                 'LAT_DECIMAL_DEGREES', 'LON_DECIMAL_DEGREES', 'COORD_PRECISION', 
                 'BANDING_DATE', 'BANDING_DAY', 'BANDING_MONTH', 'BANDING_YEAR')

allCols <- c(taxaCols, locDateCols, morphoCols, covCols)

## read in all data
allDat <- lapply(list.files(dataWD), function(f) {
    dat <- read.csv(file.path(dataWD, f), as.is = TRUE)
    missCol <- setdiff(allCols, names(dat))
    if(length(missCol) > 0) dat[missCol] <- NA
    
    dat <- dat[rowSums(!is.na(dat[, morphoCols])) > 0, allCols]
    
    return(dat)
})

allDat <- do.call(rbind, allDat)

## remove entries with no sp id
allDat <- allDat[!is.na(allDat$SPECIES_ID), ]

## clean up inconsistent column entries
allDat$FAT_SCORE[!(allDat$FAT_SCORE %in% c('T', 'H', 'N', 'F', 'L', 0:7))] <- NA
allDat$FLIGHT_FEATHER_MOLT[allDat$FLIGHT_FEATHER_MOLT == ''] <- NA
allDat$MOLT_CYCLE_CODE[allDat$MOLT_CYCLE_CODE == ''] <- NA

## merge sub-spp
allDat$SPECIES_ID[allDat$SPECIES_ID %in% c(6550, 6560)] <- 6556

## remove or correct outliers

morphoGood <- lapply(morphoCols, function(m) {
    ## cutoff based on all taxa
    cutoff2 <- qnorm(c(.Machine$double.eps^0.5, 1 - .Machine$double.eps^0.5), 
                     mean(allDat[, m], na.rm = TRUE), 
                     sd(allDat[, m], na.rm = TRUE))
    
    out <- lapply(unique(allDat$SPECIES_ID), function(sp) {
        dat <- allDat[allDat$SPECIES_ID == sp, m]
        
        ## correct morpho outliers with inter-specific cutoff
        good <- rep(1, length(dat))
        good[dat < min(cutoff2) | dat > max(cutoff2)] <- 3
        
        if(sum(!is.na(dat)) > 2) {
            ## cutoff based on intra-specific variation
            cutoff1 <- qnorm(c(.Machine$double.eps^0.5, 1 - .Machine$double.eps^0.5), 
                             mean(dat, na.rm = TRUE), 
                             sd(dat, na.rm = TRUE))
            good[dat < min(cutoff1) | dat > max(cutoff1)]
        }
        
        # plot(1:max(tapply(allDat[, m], allDat$SPECIES_ID, function(x) sum(!is.na(x)))), 
        #      ylim = range(allDat[, m], na.rm = TRUE),
        #      xlab = allDat$SPECIES_NAME[match(sp, allDat$SPECIES_ID)], ylab = m,
        #      type = 'n', axes = FALSE, frame.plot = TRUE)
        # points(sort(dat), col = c('black', 'blue', 'red')[good[order(dat)]])
        
        # good[is.na(dat)] <- 4
        
        return(good)
    })
    
    return(unlist(out))
})

morphoGood <- unlist(morphoGood)

## make `allDat` tidy
allDat <- data.frame(do.call(rbind, replicate(length(morphoCols), 
                                              allDat[, !(names(allDat) %in% morphoCols)], 
                                              simplify = FALSE)),
                     morpho = rep(morphoCols, each = nrow(allDat)), 
                     value = unlist(allDat[, morphoCols]), 
                     good = morphoGood)

allDat <- allDat[!is.na(allDat$value), ]

write.csv(allDat, 'birdBanding_clean.csv', row.names = FALSE)