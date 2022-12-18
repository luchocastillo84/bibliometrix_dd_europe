#### Load the packages ####
library(tidyverse)
library(tidyr)
library(stringr)









m_bdf <- convert2df(here("Data",  # processed data frame for bibliometrics 30 columns
                         "Processed", 
                         "M_EU.csv"),
                    dbsource = "scopus", 
                    format = "csv")

histResults <- histNetwork(m_bdf, sep = ";")

all_isi <- m_bdf %>% filter(PF %in% c("WOS"))
all_sco <- m_bdf %>% filter(PF %in% c("SCO"))
all_dim <- m_bdf %>% filter(PF %in% c("DIM"))


all_isi$CR[400]

results <- biblioAnalysis(M)
summary(results, k=20, pause=F, width=100)


all_isi = all_isi[order(all_isi$PY), ] # this makes a descendant order of the year
all_isi$Paper <- 1:nrow(all_isi) # this create a new column paper with row number
all_isi_orig <- all_isi
all_isi$nLABEL <- 1:nrow(all_isi) # this create a new column nLABLE with row number


CR <- strsplit(all_isi$CR, 
               split = ";") # this code splits the CR and create a list

CR <- lapply(seq_along(CR), function(i) { # this function creates a data frame 
  l <- data.frame(ref = CR[[i]], # for the number of CR 
                  paper = i, # of each paper and store it as a list
                  stringsAsFactors = FALSE)})

CR <- (do.call(rbind, CR)) # this binds all the CR of each paper into a df format

cr_ncols <- max(stringr::str_count(CR$ref, ",")) 

cr_colmn <- paste("field", 1:ncols, sep ="") # create and paste the name of the colums

CRsplit <-
  tidyr::separate(
    data = CR,
    col = ref,
    sep = ",",
    into = colmn,
    remove = T
  )

CRsplit$field1 <- gsub("\\*", "",CRsplit$field1) # deletes all the * in column one

CRsplit$field1 <- gsub("\\.[A-Z]", "", # delete all the dots 
                       gsub("\\. [A-Z]", "", # delete all the dots
                            gsub("\\.", "",CRsplit$field1))) # delete all the dots

CRsplit$field1 <- gsub("VANDIJK*", "VAN DIJK J", 
                       gsub("VANSDIJK J", "VAN DIJK J", 
                            gsub("VAN-DIJCK JAN", "VAN DIJK J",
                                 CRsplit$field1))) # standardize the name of this author

CRsplit$field1 <- str_replace_all(CRsplit$field1, 
                                  "VAN DIJK", 
                                  "VAN DIJK J") # standardize the name of this author
CRsplit$field1 <- sub("VAN DIJK\\s+J.*$", 
                      "VAN DIJK J", 
                      CRsplit$field1) # standardize the name of this author
CRsplit$field1 <- str_replace_all(CRsplit$field1, 
                                  "BUREAU", 
                                  "") # delete this word within author's names

CRsplit$field1 <- sub("VAN DEURSEN\\s+A.*$", 
                      "VAN DEURSEN A", 
                      CRsplit$field1) # standardize the name of this author

CRsplit$field1 <- gsub("VANDEURSEN A", 
                       "VAN DEURSEN A",
                       CRsplit$field1) # standardize the name of this author

CRsplit$field1 <- sub("SILVERSTONE\\s+R.*$", 
                      "SILVERSTONE R", 
                      CRsplit$field1) # standardize the name of this author

CRsplit$field1 <- sub("SELWYN\\s+N.*$", 
                      "SELWYN N", 
                      CRsplit$field1) # standardize the name of this author

CRsplit <- CRsplit %>% filter(field1 != "ANONYMOUS") # delete this word in author's column 

CR <- data.frame(ref= apply(CRsplit[,c(1:8)], 
                            1, function(x) paste(x, 
                                                 collapse = ","))) %>% 
  cbind(paper= CRsplit$paper)


# CR$DI <-
#  trimws(unlist(lapply(strsplit(CR$ref, # this splits the array using
#                                'DOI', # as separator the DOI word
#                                fixed = TRUE), 
#                       '[', 2))) # returs the second string after DOI
  
vis_miss(CR)

# CR$DI[is.na(CR$DI) | CR$DI=="NA"] <- "" # converts NA into empty values ""

CR$AU <- # creates a AU column
  trimws(gsub("[ ]{2,}", # extract the first string 2 spaces to separate the last name 
              "", # the names are place after the last name space
              (gsub("\\.", 
                    " ", 
                    unlist(lapply(strsplit(CR$ref, 
                                           ',', # split the strings by "," separator 
                                           fixed = TRUE), 
                                  '[', 1)))))) # extract the first string of the array
  

CR$PY <- # adds a PY column 
  trimws(unlist(lapply(strsplit(CR$ref, 
                                ',', # separated by comma
                                fixed = TRUE), 
                       '[', 2))) # extract the second string in the array

#CR$SO <- # adds a SO column 
#  trimws(unlist(lapply(strsplit(CR$ref, 
#                                ',', # separated by comma
#                                fixed = TRUE), 
#                       '[', 3))) # extract the third string in the array

CR$SR <- 
  paste(CR$AU, ", ", 
        CR$PY,
        sep = "")

all_isi$LABEL <- paste(all_isi$SR_FULL)
# all_isi$LABEL <- gsub(regex(", [^,]+$"), "", all_isi$LABEL)
all_isi$LABEL <- str_extract(all_isi$LABEL, "^[^,]*")
all_isi$LABEL <- sub("VAN DEURSEN\\s+A.*$", 
                      "VAN DEURSEN A", 
                     all_isi$LABEL)
all_isi$LABEL <- paste(all_isi$LABEL, all_isi$PY, sep = ", ")

CR$LABEL <- paste(CR$SR) 


L <- inner_join(all_isi,CR,by=c("LABEL")) # better to do an inner join 

L <- L[!is.na(L$paper),] # no need to run if there is an inner join
L$CITING <- all_isi$LABEL[L$paper] # this brings paper No. of the label to citing column
L$nCITING <- all_isi$nLABEL[L$paper] # this create a column nCITING adding LC
L$CIT_PY <- all_isi$PY[L$paper] # this create a col with the PY of the citing doc

LCS <- L %>% group_by(.data$nLABEL) %>% # create a LCS df
  summarize(LABEL = .data$LABEL[1],
            n = length(.data$nLABEL)) %>%
  as.data.frame()

all_isi$LCS <- 0
all_isi[LCS$nLABEL, "LCS"] <- LCS$n
all_isi_orig$LCS <- all_isi$LCS

histData <- all_isi[c("LABEL","TI","DE","ID","PY","LCS","TC")]
names(histData) <- c("Paper","Title","Author_Keywords","KeywordsPlus","Year","LCS","GCS")

CITING <- L %>% group_by(.data$CITING) %>%
  summarize(
    LCR = paste(.data$LABEL, collapse = ";"),
    PY = .data$CIT_PY[1],
    Paper = .data$paper[1]
  ) %>%
  ungroup() %>%
  arrange(.data$PY) %>% as.data.frame()

all_isi_orig$LCR <- NA
all_isi_orig$LCR[CITING$Paper] <- CITING$LCR
all_isi_orig$LABEL <- all_isi$LABEL
all_isi <- all_isi_orig

st<-i<-0
while(st==0){
  ind <- which(duplicated(all_isi$LABEL))
  if (length(ind)>0){
    i <- i+1
    all_isi$LABEL[ind]=paste0(all_isi$LABEL[ind],
                              "-",letters[i],sep="")}else{st <- 1}}

row.names(all_isi) <- all_isi$LABEL 
WLCR <- cocMatrix(all_isi, "LCR", sep = ";")

missingLABEL <- setdiff((all_isi$LABEL), colnames(WLCR))
colLab <- c(colnames(WLCR), missingLABEL)
WLCR <- cbind(WLCR, matrix(0, nrow(WLCR), length(missingLABEL)))
WLCR <- as.data.frame(as.matrix(WLCR), stringsAsFactors = FALSE)
colnames(WLCR) <- colLab
LABEL <- (row.names(WLCR))
WLCR <- as.matrix(WLCR[LABEL])

results <-
  list(
    NetMatrix = WLCR,
    histData = histData,
    all_isi = all_isi_orig,
    LCS = all_isi$LCS
  )
return(results)
results

#### Scopus LCS 361 obs ####

all_sco = all_sco[order(all_sco$PY), ] # this makes a descendant order of the year
all_sco <- all_sco %>%
  mutate(paper = row_number() + 574)
all_sco_orig <- all_sco
all_sco <- all_sco %>%
  mutate(nLABEL = row_number() + 574)



CRsco <- strsplit(all_sco$CR, 
               split = ";") # this code splits the CR and create a list
i <- 575
CRsco <- lapply(seq_along(CRsco), function(i) { # this function creates a data frame 
  l <- data.frame(ref = CRsco[[i]], # for the number of CR 
                  paper = i , # of each paper and store it as a list
                  stringsAsFactors = FALSE)})



CRsco <- (do.call(rbind, CRsco)) # this binds all the CR of each paper into a df format

CRsco <- CRsco %>%
  mutate(paper = paper + 574)

CRsco$ref <- sub(pattern = ",", replacement = "", 
                 x = CRsco$ref, fixed = TRUE, perl = TRUE, useBytes = FALSE)

CRsco$AU <- # creates a AU column
  trimws(gsub("[ ]{2,}", # extract the first string 2 spaces to separate the last name 
              "", # the names are place after the last name space
              (gsub("\\.", 
                    "", 
                    unlist(lapply(strsplit(CRsco$ref, 
                                           '.', # split the strings by "." separator 
                                           fixed = TRUE), 
                                  '[', 1)))))) # extract the first string of the array

CRsco$AU <- str_replace_all(CRsco$AU, 
                            "VAN DIJCK", 
                            "VAN DIJK J")

CRsco$AU <- sub("VAN DIJK\\s+J.*$", 
                      "VAN DIJK J", 
                CRsco$AU)# standardize the name of this author


CRsco$AU <- sub("VAN DIJK\\s+(.*$", 
                "VAN DIJK J", 
                CRsco$AU)

CRsco$AU <- sub("VAN DEURSEN\\s+A.*$", 
                      "VAN DEURSEN A", 
                CRsco$AU) # standardize the name of this author



CRsco$PY <- # creates a PY column that contains the year
  str_extract(string = CRsco$ref, # extract a string from the selected column  
                        pattern = "\\((\\d{4})\\)") # this pastern search for the year

CRsco$PY <- gsub("[()]", # it will look for parenthesis 
                 "", # and replace it 
                 CRsco$PY) # in the column PY

CRsco$SR <- # creates a column 
  paste(CRsco$AU, ", ", 
        CRsco$PY, 
        sep = "")

all_sco$LABEL <- all_sco$SR

s <- "HUGGINS R, 2002, LOCAL ECON"
extract_first_two <- function(s) {
  split_s <- unlist(strsplit(s , ","))
  first_two <- split_s[1:2]
  paste(first_two, collapse = ",")
}
all_sco$LABEL <- lapply(all_sco$LABEL, extract_first_two)

CRsco$LABEL <- paste(CRsco$SR) 

all_sco

