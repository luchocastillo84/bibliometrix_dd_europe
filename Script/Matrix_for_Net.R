# Load the necessary packages
library(tidyverse)
library(here)
library(bibliometrix)
library(devtools)
library(readtext)
library(tidyr)
library(readr)
library(visdat)
library(stringr)
library(dplyr)
library(readxl)
library(Matrix)


################################################################################
############################# Co-Citation Analysis #############################
################################################################################

# n_1p <- metaTagExtraction(n_1p, Field = "CR_AU", sep = ";" )
CC <- cocMatrix(n_1p, Field = "CR", type = "sparse", sep = ";")
CC_mat <- crossprod(CC, CC)

CC_mat <- CC_mat[nchar(colnames(CC_mat)) != 0, nchar(colnames(CC_mat)) != 0]

ind <- which(regexpr("[A-Za-z]", substr(colnames(CC_mat), 1, 1)) == 1)
LABEL <- labelShort(CC_mat, db = tolower(n_1p$DB[1]))
LABEL <- removeDuplicatedlabels(LABEL)
colnames(CC_mat) <- rownames(CC_mat) <-  LABEL

set.seed(1001)
net=netPlot(CC_mat, n = 25, Title = "Co-Citation Network P1 louvain", 
                type = "fruchterman", size.cex=TRUE, size=20, 
                remove.multiple=FALSE, labelsize=0.7,edgesize = 4, edges.min=1, label = T,
            cluster = "louvain")

co_cite1 <- head(net$cluster_res,30)



CC2 <- cocMatrix(n_2p, Field = "CR", type = "sparse", sep = ";")
CC2_mat <- crossprod(CC2, CC2)

CC2_mat <- CC2_mat[nchar(colnames(CC2_mat)) != 0, nchar(colnames(CC2_mat)) != 0]

ind <- which(regexpr("[A-Za-z]", substr(colnames(CC2_mat), 1, 1)) == 1)
LABEL <- labelShort(CC2_mat, db = tolower(n_2p$DB[1]))
LABEL <- removeDuplicatedlabels(LABEL)
colnames(CC2_mat) <- rownames(CC2_mat) <-  LABEL

set.seed(1001)
net=netPlot(CC2_mat, n = 25, Title = "Co-Citation Network P2 louvain", 
            type = "fruchterman", size.cex=TRUE, size=20, 
            remove.multiple=FALSE, labelsize=0.7,edgesize = 4, edges.min=1, label = T,
            cluster = "louvain")

co_cite2 <- head(net$cluster_res,30)


CC3 <- cocMatrix(n_3p, Field = "CR", type = "sparse", sep = ";")
CC3_mat <- crossprod(CC3, CC3)

CC3_mat <- CC3_mat[nchar(colnames(CC3_mat)) != 0, nchar(colnames(CC3_mat)) != 0]

ind <- which(regexpr("[A-Za-z]", substr(colnames(CC3_mat), 1, 1)) == 1)
LABEL <- labelShort(CC3_mat, db = tolower(n_3p$DB[1]))
LABEL <- removeDuplicatedlabels(LABEL)
colnames(CC3_mat) <- rownames(CC3_mat) <-  LABEL

set.seed(1001)
net=netPlot(CC3_mat, n = 25, Title = "Co-Citation Network P3 louvain", 
            type = "fruchterman", size.cex=TRUE, size=20, 
            remove.multiple=FALSE, labelsize=0.7,edgesize = 4, edges.min=1, label = T,
            cluster = "louvain")

co_cite3 <- head(net$cluster_res,30)


################################################################################
############################# Bibliographic Coupling ###########################
################################################################################


################################### Period 1  ##################################
BC1 <- Matrix::t(cocMat(n_1p, Field = "CR", type = "sparse", sep = ";"))
BC1_mat <- crossprod(BC1, BC1)

BC1_mat <- BC1_mat[nchar(colnames(BC1_mat)) != 0, nchar(colnames(BC1_mat)) != 0]

BC1_mat_labels <- colnames(BC1_mat)
AU <- # creates a AU column
  trimws(gsub("[ ]{2,}", # extract the first string 2 spaces to separate the last name 
              "", # the names are place after the last name space
              (gsub("\\.", 
                    "", 
                    unlist(lapply(strsplit(BC1_mat_labels, 
                                           ',', # split the strings by "," separator 
                                           fixed = TRUE), 
                                  '[', 1)))))) # extract the first string of the array

PY <- # creates a AU column
  trimws(gsub("[ ]{2,}", # extract the first string 2 spaces to separate the last name 
              "", # the names are place after the last name space
              (gsub("\\.", 
                    "", 
                    unlist(lapply(strsplit(BC1_mat_labels, 
                                           ',', # split the strings by "," separator 
                                           fixed = TRUE), 
                                  '[', 2)))))) # extract the first string of the array
LABEL <- paste(str_to_title(AU), PY, sep = ", ")
LABEL <- removeDuplicatedlabels(LABEL)

colnames(BC1_mat) <- rownames(BC1_mat) <-  LABEL

set.seed(1001)
net=netPlot(BC1_mat, n = 25, Title = "Bibliographic Copling louvain", 
            type = "fruchterman", size.cex=TRUE, size=15, 
            remove.multiple=FALSE, labelsize=0.7,edgesize = 3, edges.min=1, label = T,
            cluster = "louvain")

head(net$cluster_res,30)


################################### Period 2  ##################################
BC2 <- Matrix::t(cocMat(n_2p, Field = "CR", type = "sparse", sep = ";"))
BC2_mat <- crossprod(BC2, BC2)

BC2_mat <- BC2_mat[nchar(colnames(BC2_mat)) != 0, nchar(colnames(BC2_mat)) != 0]

BC2_mat_labels <- colnames(BC2_mat)
AU <- # creates a AU column
  trimws(gsub("[ ]{2,}", # extract the first string 2 spaces to separate the last name 
              "", # the names are place after the last name space
              (gsub("\\.", 
                    "", 
                    unlist(lapply(strsplit(BC2_mat_labels, 
                                           ',', # split the strings by "," separator 
                                           fixed = TRUE), 
                                  '[', 1)))))) # extract the first string of the array

PY <- # creates a AU column
  trimws(gsub("[ ]{2,}", # extract the first string 2 spaces to separate the last name 
              "", # the names are place after the last name space
              (gsub("\\.", 
                    "", 
                    unlist(lapply(strsplit(BC2_mat_labels, 
                                           ',', # split the strings by "," separator 
                                           fixed = TRUE), 
                                  '[', 2)))))) # extract the first string of the array
LABEL <- paste(str_to_title(AU), PY, sep = ", ")
LABEL <- removeDuplicatedlabels(LABEL)

colnames(BC2_mat) <- rownames(BC2_mat) <-  LABEL

set.seed(1001)
net=netPlot(BC2_mat, n = 25, Title = "Bibliographic Copling louvain", 
            type = "fruchterman", size.cex=TRUE, size=15, 
            remove.multiple=FALSE, labelsize=0.7,edgesize = 3, edges.min=1, label = T,
            cluster = "louvain")

head(net$cluster_res,30)


################################### Period 3  ##################################
BC3 <- Matrix::t(cocMat(n_3p, Field = "CR", type = "sparse", sep = ";"))
BC3_mat <- crossprod(BC3, BC3)

BC3_mat <- BC3_mat[nchar(colnames(BC3_mat)) != 0, nchar(colnames(BC3_mat)) != 0]

BC3_mat_labels <- colnames(BC3_mat)
AU <- # creates a AU column
  trimws(gsub("[ ]{2,}", # extract the first string 2 spaces to separate the last name 
              "", # the names are place after the last name space
              (gsub("\\.", 
                    "", 
                    unlist(lapply(strsplit(BC3_mat_labels, 
                                           ',', # split the strings by "," separator 
                                           fixed = TRUE), 
                                  '[', 1)))))) # extract the first string of the array

PY <- # creates a AU column
  trimws(gsub("[ ]{2,}", # extract the first string 2 spaces to separate the last name 
              "", # the names are place after the last name space
              (gsub("\\.", 
                    "", 
                    unlist(lapply(strsplit(BC3_mat_labels, 
                                           ',', # split the strings by "," separator 
                                           fixed = TRUE), 
                                  '[', 2)))))) # extract the first string of the array
LABEL <- paste(str_to_title(AU), PY, sep = ", ")
LABEL <- removeDuplicatedlabels(LABEL)

colnames(BC3_mat) <- rownames(BC3_mat) <-  LABEL

set.seed(1001)
net=netPlot(BC3_mat, n = 20, Title = "Bibliographic Copling louvain", 
            type = "fruchterman", size.cex=TRUE, size=15, 
            remove.multiple=FALSE, labelsize=0.7,edgesize = 3, edges.min=1, label = T,
            cluster = "louvain")

head(net$cluster_res,30)


################################################################################
############################# Conceptual Structure #############################
################################################################################


WCo1 <- cocMat(N, Field = "ID", type = "sparse", sep = ";")

WCo1_mat <- crossprod(WCo1, WCo1)

WCo1_mat <- WCo1_mat[nchar(colnames(WCo1_mat)) != 0, nchar(colnames(WCo1_mat)) != 0]

WCo1_mat_labels <- colnames(WCo1_mat)

net=netPlot(WCo1_mat, n = 30, Title = "Co-Word Network 1P louvain", 
            type = "fruchterman", size.cex=TRUE, size=15, 
            remove.multiple=FALSE, labelsize=0.7,edgesize = 3, edges.min=5, label = T,
            cluster = "louvain")



WCo2 <- cocMat(n_2p, Field = "DE", type = "sparse", sep = ";")

WCo2_mat <- crossprod(WCo2, WCo2)

WCo2_mat <- WCo2_mat[nchar(colnames(WCo2_mat)) != 0, nchar(colnames(WCo2_mat)) != 0]

WCo2_mat_labels <- colnames(WCo2_mat)

net=netPlot(WCo2_mat, n = 30, Title = "Co-Word Network 2P louvain", 
            type = "fruchterman", size.cex=TRUE, size=15, 
            remove.multiple=FALSE, labelsize=0.7,edgesize = 3, edges.min=5, label = T,
            cluster = "louvain")


WCo3 <- cocMat(n_3p, Field = "DE", type = "sparse", sep = ";")

WCo3_mat <- crossprod(WCo3, WCo3)

WCo3_mat <- WCo3_mat[nchar(colnames(WCo3_mat)) != 0, nchar(colnames(WCo3_mat)) != 0]

WCo3_mat_labels <- colnames(WCo3_mat)

net=netPlot(WCo3_mat, n = 30, Title = "Co-Word Network 3P louvain", 
            type = "fruchterman", size.cex=TRUE, size=15, 
            remove.multiple=FALSE, labelsize=0.7,edgesize = 3, edges.min=5, label = T,
            cluster = "louvain")


################################################################################
################################# Collaboration  ###############################
################################################################################

COLco1 <- cocMat(n_3p, Field = "AU_CO", type = "sparse", sep = ";")


COLco1_mat <- crossprod(COLco1, COLco1)

COLco1_mat <- COLco1_mat[nchar(colnames(COLco1_mat)) != 0, nchar(colnames(COLco1_mat)) != 0]

COLco1_mat_labels <- colnames(COLco1_mat)

net=netPlot(COLco1_mat, n = 20, Title = "Country Collaboration louvain", 
            type = "circle", size.cex=TRUE, size=15, 
            remove.multiple=FALSE, labelsize=0.7,edgesize = 3, edges.min=1, label = T,
            cluster = "louvain")





COLun1 <- cocMat(N, Field = "AU_UN", type = "sparse", sep = ";")


COLun1_mat <- crossprod(COLun1, COLun1)

COLun1_mat <- COLun1_mat[nchar(colnames(COLun1_mat)) != 0, nchar(colnames(COLun1_mat)) != 0]

COLun1_mat_labels <- colnames(COLun1_mat)

net=netPlot(COLun1_mat, n = 20, Title = "University Collaborations louvain", 
            type = "circle", size.cex=TRUE, size=15, 
            remove.multiple=FALSE, labelsize=0.7,edgesize = 3, edges.min=1, label = T,
            cluster = "louvain")







