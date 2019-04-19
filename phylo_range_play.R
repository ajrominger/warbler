library(ape)
library(rotl)
library(sp)
library(rgdal)

wtre <- get_study_tree(study_id = 'pg_2591', tree_id = 'tree6024')

x <- read.csv('~/Dropbox/Research/warbler/lovette_taxonomy.csv', as.is = TRUE)
botw <- read.csv('~/Dropbox/Research/data/BirdLife_HBW_Bird_Maps/BOTW.csv', as.is = TRUE)

newname <- sapply(1:nrow(x), function(i) {
    gsub('.* ', paste0(x$Recommended.genus[i], '_'), x$Taxon[i])
})
newname[newname == 'Myiothlypis_coronatus'] <- 
    paste(newname[newname == 'Myiothlypis_coronatus'], 1:2, sep = '_')

wtre <- drop.tip(wtre, wtre$tip.label[!(wtre$tip.label %in% newname)])



write.csv(cbind(x, newname = gsub('_', ' ', newname), 
                inbotw = gsub('_', ' ', newname) %in% botw$SCINAME), file = 'lovette_taxSyn.csv')
