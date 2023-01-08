#### Load the packages ####
library(tidyverse)
library(tidyr)
library(stringr)
library(bibliometrix)
library(tidytext)
library(here)
library(igraph)
library(Matrix)
library(RColorBrewer)

##### Binding the clean df from the three sources WOS, SCO and DIM #############

M <- read_csv(here("Data", # loading CR raw files as csv files 
                   "Processed", 
                   "M_EU.csv"),
                   col_names = T)

rownames(M) <- M$SRDI # using SR as row names

CR <- strsplit(n_1p$CR, 
                   split = ";") # this code splits the CR and create a list

TERMS <- data.frame(CITING = rep(n_1p$SRDI,lengths(CR)), CITED = trimws(unlist(CR)))

n_distinct(TERMS$CITING)
n_distinct(TERMS$CITED)

cgAU <- as.data.frame(AU= unique(TERMS$CITING),)
cdAU <- as.data.frame(unique(TERMS$CITED))
AUtot <- unique(rbind(cgAU,cdAU))
A <- as.data.frame(unique(rbind(unique(TERMS$CITING), unique(TERMS$CITED))))

# CR <- lapply(seq_along(CR), function(i) { # this function creates a data frame 
#   l <- data.frame(CR = CR[[i]], # for the number of CR 
#                   paper = i , # of each paper and store it as a list
#                   stringsAsFactors = FALSE)})
# 
# CR <- (do.call(rbind, CR)) # this binds all the CR of each paper into a df format
# 
# CR$CR <- trimws(CR$CR)

ECR <- as.data.frame(table(TERMS))
ECR1 <- subset(ECR, Freq>0)

netCR_deg <- degree(netCR, mode = c("All"))

# Create igraph object 
set.seed(1001)
netCR <- graph_from_data_frame(ECR1,directed = F, vertices = NULL)
pal <- brewer.pal(length(unique(V(net))))
plot(netCR, edge.color = 'black', vertex.label.cex = 0.5,
     vertex.size= sqrt(netCR_deg)/ 3, layout = layout.fruchterman.reingold)

















