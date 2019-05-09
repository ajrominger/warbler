library(ape)
library(rotl)
library(socorro)
library(viridis)

# synonym matching between Lovette and BOTW
syn <- read.csv('data/lovette_taxSyn.csv', as.is = TRUE)

# Lovette phylo
wtre <- get_study_tree(study_id = 'pg_2591', tree_id = 'tree6024')
wtre$tip.label <- gsub('_', ' ', wtre$tip.label)
# drop outgroups
wtre <- drop.tip(wtre, wtre$tip.label[!(wtre$tip.label %in% syn$lovette_name)])

# make phylo distances
treDist <- cophenetic(wtre)
treDist <- data.frame(lovette_sp1 = rep(colnames(treDist), each = nrow(treDist)), 
                      lovette_sp2 = rep(rownames(treDist), ncol(treDist)), 
                      phylo_dist = as.vector(treDist), stringsAsFactors = FALSE)

# match up names
treDist$species1 <- syn$botw_name[match(treDist$lovette_sp1, syn$lovette_name)]
treDist$species2 <- syn$botw_name[match(treDist$lovette_sp2, syn$lovette_name)]

# combine with range overlap
load('data/allROver.RData')
phyloOver <- merge(allROver, treDist[, -(1:2)], by = c('species1', 'species2'))

with(phyloOver, 
     {
         layout(matrix(1:2, nrow = 1), widths = c(3, 1))
         
         # browser()
         par(mar = c(3, 3, 0, 0) + 0.5, mgp = c(2, 0.5, 0))
         plot(phylo_dist, prop_interAvg,
              col = quantCol(1 - prop_diff, pal = viridis(40)))

         par(mar = c(3, 1, 0, 0) + 0.5, mgp = c(2, 0.5, 0))
         plot(sort(1 - prop_diff), log = 'y',
              col = quantCol(sort(1 - prop_diff), pal = viridis(40), trans = 'log'),
              axes = FALSE)
         logAxis(2)
     }
)


plot(phyloOver$area1, phyloOver$area2, col = quantCol(phyloOver$phylo_dist, pal = viridis(40)))

nrow(expand.grid(unique(phyloOver$area1), unique(phyloOver$area2)))
points(phyloOver$area1, phyloOver$area2, col = 'red')

# [grepl('Setophaga', phyloOver$species1) & 
#         grepl('Setophaga', phyloOver$species2), ]

pdf('foo.pdf')
pairs(phyloOver[, -(1:4)])
dev.off()
