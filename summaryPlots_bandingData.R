library(socorro)

setwd('~/Dropbox/Research/warbler')

allDat <- read.csv('birdBandingData/birdBanding_clean.csv', as.is = TRUE)
morphoCols <- gsub('good_', '', names(allDat)[grep('good_', names(allDat))])

## plotting cutoffs

pdf('fig_bandingCutoffs.pdf', 
    width = 1.25 * length(unique(allDat$SPECIES_ID)), 
    height = 1.25 * length(morphoCols))

par(mfrow = c(length(morphoCols), length(unique(allDat$SPECIES_ID))), 
    mar = rep(0.1, 4), oma = c(2, 3.5, 2, 0), mgp = c(2, 0.75, 0), xpd = NA)

lapply(morphoCols, function(m) {
    lapply(unique(allDat$SPECIES_ID), function(sp) {
        plot(1:max(tapply(allDat[, m], allDat$SPECIES_ID, function(x) sum(!is.na(x)))), 
             type = 'n', xaxt = 'n', xlab = '', 
             yaxt = ifelse(sp == unique(allDat$SPECIES_ID)[1], 's', 'n'), 
             ylab = ifelse(sp == unique(allDat$SPECIES_ID)[1], m, ''), 
             ylim = range(allDat[, m], na.rm = TRUE))
        
        if(m == morphoCols[1]) {
            mtext(gsub(' .*', '', allDat$SPECIES_NAME[match(sp, allDat$SPECIES_ID)]), 
                  side = 3, line = 1)
        }
        
        dat <- allDat[allDat$SPECIES_ID == sp, paste0(c('', 'good_'), m)]
        dat <- dat[!is.na(dat[, m]), ]
        
        if(nrow(dat) > 0) {
            points(sort(dat[, 1]), 
                   col = c('black', 'blue', 'red')[dat[order(dat[, 1]), 2]])
        }
        
        invisible(NULL)
    })
    
    invisible(NULL)
})

mtext('Rank', side = 1, line = 0.5, outer = TRUE)

dev.off()


## map of traits


