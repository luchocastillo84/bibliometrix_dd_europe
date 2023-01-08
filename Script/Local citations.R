


m <- M

m = m[order(m$PY), ] # this makes a descendant order of the year

#m <- m %>% # this will add a column paper with the consecutive starting
#  mutate(Paper = row_number() + 574) # from 574
m$Paper <- 1:nrow(m)
m_orig <- m # creates the original df

#m <- m %>% # this will add a column nLALBEL with the consecutive starting
#  mutate(nLABEL = row_number() + 574)# from 574
m$nLABEL <- 1:nrow(m)

crm <- strsplit(m$CR, 
                   split = ";") # this code splits the CR and create a list

crm <- lapply(seq_along(crm), function(i) { # this function creates a data frame 
  l <- data.frame(LABEL = crm[[i]], # for the number of CR 
                  paper = i , # of each paper and store it as a list
                  stringsAsFactors = FALSE)})

crm <- (do.call(rbind, crm)) # this bin
crm$LABEL <- trimws(crm$LABEL)


Lm <- inner_join(m, # 132 matches were found 
                   crm,
                   by=c("SRDI"="LABEL")) # better to do an inner join 

Lm <- Lm[!is.na(Lm$paper),] # no need to run if there is an inner join
#The expression m$LABEL[L$paper] selects the values in the LABEL column of 
#the m data frame for which the corresponding elements in the paper column #
# of the L data frame are TRUE.
Lm$CITING <- m$SRDI[Lm$paper] # this brings paper No. of the label to citing column
Lm$CITING[1]
Lm$nCITING <- m$nLABEL[Lm$paper] # this create a column nCITING adding LC
Lm$CIT_PY <- m$PY[Lm$paper] # this create a col with the PY of the citing doc

LCSm <- Lm %>% group_by(.data$nLABEL) %>% # create a LCS df
  summarize(SRDI = .data$SRDI[1],
            n = length(.data$nLABEL)) %>%
  as.data.frame()

m$LCS <- 0
m[LCSm$nLABEL, "LCS"] <- LCSm$n
n_distinct(m$SRDI)
m_orig$LCS <- m$LCS

histData_m <- m[c("SRDI","TI","DE","ID","PY","LCS","TC")]
names(histData_m) <- c("Paper","Title","Author_Keywords","KeywordsPlus","Year","LCS","GCS")


# because not all the papers published in the same year by the same author can not be
# identify the the LCR column has in some rows repeated references
# this suggest that the citing paper has cited one of this references
CITING_sco <- Lm %>% group_by(.data$CITING) %>%
  summarize(
    LCR = paste(.data$LABEL, # 
                collapse = ";"),
    PY = .data$CIT_PY[1],
    Paper = .data$paper[1]
  ) %>%
  ungroup() %>%
  arrange(.data$PY) %>% as.data.frame()

m_orig$LCR <- NA
m_orig$LCR[CITING_sco$Paper] <- CITING_sco$LCR
m_orig$LABEL <- m$LABEL
m <- m_orig