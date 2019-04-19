extractSppBirdLife <- function(dsnSource, dsnOut, layer, scinames, seasons, mc.cores = 1) {
    if(!dir.exists(dsnOut)) dir.create(dsnOut)
    
    # scinames <- paste(scinames, collapse = ' OR ')
    
    if(length(scinames) > 1) {
        tempDir <- paste0(gsub(':| ', '', Sys.time()), '_temp')
        tempDir <- '2019-04-16202600_temp'
        dir.create(file.path(dsnOut, tempDir))
        # o <- parallel::mclapply(scinames, mc.cores = mc.cores, 
        o <- lapply(scinames,
                                FUN = function(s) {
                                    fsp <- gsub(' ', '_', s)
                                    # browser()
                                    print(fsp)
                                    f <- file.path(dsnOut, tempDir, paste0(fsp, '.shp'))
                                    # .extract1sp(dsnSource, f, s, seasons)
                                    return(rgdal::readOGR(path.expand(f), fsp))
                                })
        o <- do.call(rbind, o)
        rgdal::writeOGR(o, path.expand(dsnOut), layer, driver = 'ESRI Shapefile', overwrite_layer = TRUE)
        # system(sprintf('rm -fr %s', file.path(dsnOut, tempDir)))
    } else {
        print('good')
        .extract1sp(dsnSource, file.path(dsnOut, paste0(layer, '.shp')), scinames, seasons)
    }
}

.extract1sp <- function(dsn, file, sciname, seasons) {
    seas <- paste(paste0("SEASONAL='", seasons, "'"), collapse = ' OR ')
    system(sprintf('ogr2ogr %s %s -SQL \"SELECT * FROM All_Species WHERE SCINAME=\'%s\' AND (%s)\"', 
                   path.expand(file), path.expand(dsn), sciname, seas))
}

# extractSppBirdLife('~/Dropbox/Research/data/BirdLife_HBW_Bird_Maps/BOTW.gdb',
#                    '~/Dropbox/Research/warbler/foo', 'new_test',
#                    c('Cardellina pusilla', 'Myioborus melanocephalus'),
#                    c(1, 2), mc.cores = 2)
