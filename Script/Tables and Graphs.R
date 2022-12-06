#### 0. Loading Packages ####
library(tidyverse)
library(here)
library(bibliometrix)
library(devtools)
library(readtext)
library(tidyr)
library(bib2df)
library(readr)
library(visdat)
library(stringr)
library(dplyr)
library(bibtex)
library(readxl)
library(janitor)
library(naniar)
library(dimensionsR)
library(jsonlite)
library(purrr)

### 1. Descriptive Analysis ######

MM <- read_csv(here("Data", # reading the bibliographic data
                    "Processed", 
                    "M_EU.csv"))

##### 1.1. Total ####
Details <- c("Timespan", "Journals", # creating details vector
             "Annual growth rate %", "Average citations per doc", 
             "Total published documents", "Articles","Book chapters",
             "Porceeding papers","Conference papers") # 11 items

tot_period <-paste(min(MM$PY, # the studied period in the bibliometric
                       na.rm = T) , 
                   "-", max(MM$PY, 
                            na.rm = T))

Y=table(MM$PY) # number of published docs by year
ny=dim(Y)[1] # returns how many years 
CAGR<-as.numeric(
  round(((Y[ny]/Y[1])^(1/(ny-1))-1)*100,2)) # Compound annual growth rate
sources <- n_distinct(MM$SO) # counts the number of journals in MM
n_docs <- nrow(MM) # counts the number of published documents in MM
avg_cite <- round(sum(MM$TC)/nrow(MM), 2) # returns the average citation 

arti <- as_vector(  # counting and merging articles with reviewed articles
        nrow(MM[MM$DT=='article', ]) + 
        nrow(MM[MM$DT=='review', ])) 

bchap <- as_vector(
         nrow(MM[MM$DT=='book chapter',]) + # counting and merging book chapters 
         nrow(MM[MM$DT=='article; book chapter',])) # with arti & and book chapters

procee <- as_vector(
          nrow(MM[MM$DT=='proceedings paper',]) + 
          nrow(MM[MM$DT=='article; proceedings paper', 
                  ])) # counting and merging proceedings 

confe <- as_vector(nrow(MM[MM$DT=='conference paper', 
                           ])) # counting conference papers


Total <- c(tot_period, sources, CAGR, # creating a total vector
           avg_cite, n_docs, arti, bchap, 
           procee, confe)

##### 1.2. 2000 to 2007: first period ####

M_1p <- MM %>% filter(PY <= 2007) # sub-setting the first period

tot_period1 <-paste(min(M_1p$PY, # this returns the first period 
                        na.rm = T) , # studied in the bibliometric
                    "-", max(M_1p$PY, 
                             na.rm = T))
Y1=table(M_1p$PY) # number of published docs by year
ny1=dim(Y1)[1] # returns how many years 
CAGR1<-as.numeric(
       round(((Y1[ny1]/Y1[1])^(1/(ny1-1))-1)*100,
             2)) # Compound annual growth rate

sources1 <- n_distinct(M_1p$SO) # counts the number of journals
n_docs1 <- nrow(M_1p) # counts the number of published documents
avg_cite1 <- round(sum(M_1p$TC)/nrow(M_1p), 2)  # returns the average citation 

arti1 <- as_vector(
         nrow(M_1p[M_1p$DT=='article', ]) + # counting and merging 
         nrow(M_1p[M_1p$DT=='review', ])) # articles with reviewed articles

bchap1 <- as_vector(
          nrow(M_1p[M_1p$DT=='book chapter',]) + 
          nrow(M_1p[M_1p$DT=='article; book chapter',
                    ])) # counting and merging book chapters 

procee1 <- as_vector(
           nrow(M_1p[M_1p$DT=='proceedings paper',]) + 
           nrow(M_1p[M_1p$DT=='article; proceedings paper', 
                     ])) # counting and merging proceedings 

confe1 <- as_vector(
          nrow(M_1p[M_1p$DT=='conference paper', 
                    ])) # counting conference papers

"Period 1" <- c(tot_period1, sources1, # creating a vector with the first period
                CAGR1, avg_cite1, n_docs1, arti1, 
                bchap1, procee1, confe1 )

##### 1.3. 2008 to 2014: second period ####

M_2p <- MM %>% filter(PY > 2007 & PY <= 2014) # sub-setting the second period

tot_period2 <-paste(min(M_2p$PY, # this returns the second period 
                        na.rm = T) , # studied in the bibliometric
                    "-", max(M_2p$PY,
                             na.rm = T))
Y2=table(M_2p$PY) # number of published docs by year
ny2=dim(Y2)[1] # returns how many years 
CAGR2<-as.numeric(
       round(((Y2[ny2]/Y2[1])^(1/(ny1-1))-1)*100,
             2)) # Compound annual growth rate

sources2 <- n_distinct(M_2p$SO) # counts the number of journals
n_docs2 <- nrow(M_2p) # counts the number of published documents
avg_cite2 <- round(sum(M_2p$TC)/nrow(M_2p), 2) # returns the average citation 

arti2 <- as_vector(
         nrow(M_2p[M_2p$DT=='article', ]) + # counting and merging 
         nrow(M_2p[M_2p$DT=='review', ])) # articles with reviewed articles

bchap2 <- as_vector(
          nrow(M_2p[M_2p$DT=='book chapter',]) + 
          nrow(M_2p[M_2p$DT=='article; book chapter',
                    ])) # counting and merging book chapters 

procee2 <- as_vector(
           nrow(M_2p[M_2p$DT=='proceedings paper',]) + 
           nrow(M_2p[M_2p$DT=='article; proceedings paper',
                     ])) # counting and merging proceedings 

confe2 <- as_vector(
          nrow(M_2p[M_2p$DT=='conference paper',
                    ])) # counting conference papers

"Period 2" <- c(tot_period2, sources2, # creating a vector with the second period
                CAGR2, avg_cite2, n_docs2, 
                arti2, bchap2, procee2, confe2 )

##### 1.4. 2015 to 2021: third period ####

M_3p <- M %>% filter(PY > 2014 & PY <= 2021) # sub-setting the third period

tot_period3 <-paste(min(M_3p$PY, # this returns the second period 
                        na.rm = T) , # studied in the bibliometric
                    "-", max(M_3p$PY,
                             na.rm = T))
Y3=table(M_3p$PY) # number of published docs by year
ny3=dim(Y3)[1] # returns how many years 
CAGR3<-as.numeric(
       round(((Y3[ny3]/Y3[1])^(1/(ny3-1))-1)*100,
             2)) # Compound annual growth rate

sources3 <- n_distinct(M_3p$SO) # counts the number of journals 
n_docs3 <- nrow(M_3p) # counts the number of published documents 
avg_cite3 <- round(sum(M_3p$TC)/nrow(M_3p), 2) # returns the average citation 

arti3 <- as_vector(
         nrow(M_3p[M_3p$DT=='article', ]) + # counting and merging 
         nrow(M_3p[M_3p$DT=='review', ])) # articles with reviewed articles

bchap3 <- as_vector(
          nrow(M_3p[M_3p$DT=='book chapter',]) + 
          nrow(M_3p[M_3p$DT=='article; book chapter',
                    ])) # counting and merging book chapters 

procee3 <- as_vector(
           nrow(M_3p[M_3p$DT=='proceedings paper',]) + 
           nrow(M_3p[M_3p$DT=='article; proceedings paper', 
                     ])) # counting and merging proceedings 

confe3 <- as_vector(nrow(M_3p[M_3p$DT=='conference paper', 
                              ])) # counting conference papers

"Period 3" <- c(tot_period3, sources3, # creating a vector with the third period
                CAGR3, avg_cite3, n_docs3, 
                arti3, bchap3, procee3, confe3 )

##### 1.5. Table with totals and periods ####

tot_docs <- data.frame(Details, # Table with descriptive analysis
                       `Period 1`, 
                       `Period 2`, 
                       `Period 3`, 
                       Total)

write_csv(tot_docs, file = here("Data", 
                                "Processed", 
                                "desc_analysis.csv")) # writing as CSV for Rmd


### 2. Line chart: publications and citations per year ####

M %>% group_by(PY) %>% 
  summarise(totc = sum(TC)) %>%  
  ggplot(aes(x= PY, y= totc)) + geom_line() + geom_vline(xintercept = c(2007, 2014), 
                                                         linetype = "dotted",
                                                         color = "blue")

M %>% group_by(PY) %>% 
  summarise(totd = n()) %>%  
  ggplot(aes(x= PY, y= totd)) + geom_line() + geom_vline(xintercept = c(2007, 2014), 
                                                         linetype = "dotted",
                                                         color = "blue")


### 3. Most influential articles ####

##### 3.1 Total #####
influ_arti <- M %>% select(AU,PY, TI, TC) %>% # select columns with most cited art
              arrange(desc(TC)) %>% # order by citations descending order 
              mutate(Rank = 1:nrow(M)) %>%  # add a rank column
              select( c(5, 1:4)) %>% # re order
              head(10) # select first 10 observations

write_csv(influ_arti, file = here("Data", 
                                "Processed", 
                                "influ_arti.csv")) # writing as CSV for Rmd


##### 3.1. 2000 to 2007: first period ####

influ_arti_1p <- M_1p %>% select(AU,PY, TI, TC) %>% # select columns with most cited art
                 arrange(desc(TC)) %>% # order by citations descending order 
                 mutate(Rank = 1:nrow(M_1p)) %>% # add a rank column
                 select(c(5, 1:4)) %>% # re order
                 head(10) # select first 10 observations

write_csv(influ_arti_1p, file = here("Data", 
                                  "Processed", 
                                  "influ_arti_1p.csv")) # writing as CSV for Rmd


##### 3.2. 2008 to 2014: second period ####

influ_arti_2p <- M_2p %>% select(AU,PY, TI, TC) %>% # select columns with most cited art
                arrange(desc(TC)) %>% # order by citations descending order 
                mutate(Rank = 1:nrow(M_2p)) %>%  # add a rank column
                select(c(5, 1:4)) %>% # re order
                head(10) # select first 10 observations

write_csv(influ_arti_2p, file = here("Data", 
                                     "Processed", 
                                     "influ_arti_2p.csv")) # writing as CSV for Rmd


##### 3.3. 2015 to 2021: second period ####

influ_arti_3p <- M_3p %>% select(AU,PY, TI, TC) %>% # select columns with most cited art
                 arrange(desc(TC)) %>% # order by citations descending order 
                 mutate(Rank = 1:nrow(M_3p)) %>%  # add a rank column
                 select(c(5, 1:4)) %>% # re order
                 head(10) # select first 10 observations

write_csv(influ_arti_3p, file = here("Data", 
                                     "Processed", 
                                     "influ_arti_3p.csv")) # writing as CSV for Rmd

##### 3.4. Local most cited articles ####
# How the most cited articles must be conduct?
# with a global sample or by periods ?

CR <- citations(MM, field = "article", sep = ";")
cbind(CR$Cited[1:10 ])
dfCR <- head(data.frame(Article=(unlist(CR$Cited))),10)
dfCR$Article.CR <- str_replace_all(as.character(dfCR$Article.CR), 
                          pattern = ", ", 
                          repl= ",") # convert the AU_CO into a chr vector


n_colscr <- max(stringr::str_count(dfCR$Article.CR, 
                                   ",")) # count the number of strings separated by ","
colmna_cr <- paste("field", # naming the columns 
                   1:n_colscr, 
                   sep ="")

cr_dfs <- separate(dfCR, # create a df with the local CR 
                   Article.CR, 
                   into = colmna_cr, 
                   sep = ",",
                   remove = TRUE) %>% 
          select(c(1:3,6)) %>% 
          rename("AU"= field1,
                 "PY"= field2,
                 "SO"= field3,
                 "Total PD"= Article.Freq)


write_csv(influ_arti_3p, file = here("Data", 
                                     "Processed", 
                                     "influ_arti_3p.csv")) # writing as CSV for Rmd


### 4. Most influential journals  ####

SO <- M %>% filter(!is.na(SO)) %>%  # filter out NA
  group_by(SO) %>% # group by journal 
  dplyr::summarise(TC = sum(TC), # sum citations
                   PD= n()) %>%   # sum No of articles
  arrange(desc(TC)) %>% # order by descending order
  head(10)


### 5. Most productive countries ####

##### 5.1. Single country publications ####

head(M$AU_CO)
auco_v <- str_replace_all(as.character(M$AU_CO), # convert the AU_CO into a chr vector
                          pattern = "; ", # deleting spaces between string elements
                          repl= ";") 

auco_df <- data.frame(country=auco_v) # converting the AU_CO vector into df
auco_df_u <-  auco_df %>% 
  mutate(split = str_split(auco_v, ";")) %>% # split
  mutate(split = purrr::map(.$split, ~ unique(.x))) %>% # drop duplicates
  mutate(split = purrr::map_chr(.$split, ~paste(.x, collapse = ";"))) %>% # recombine
  select(split)

n_colsco <- max(stringr::str_count(auco_df_u$split, 
                                   ";")) # count how many columns to split
colmna <- paste("country_", 1:n_colsco,
                sep ="") # naming the split columns 
auco_dfs <- separate(auco_df_u, # create df with authors country affiliation
                     split, # selecting the column
                     into = colmna, # choosing the names of the columns
                     sep = ";", # choosing separator 
                     remove = TRUE)

auco_1df <- auco_dfs %>% 
  filter(is.na(country_2)) %>% # filtering by NA country_2 to get SCP
  select(country_1) %>% 
  group_by(country_1) %>% 
  dplyr::summarise(tot_docs= n()) %>% 
  arrange(desc(tot_docs))

##### 5.2. Multiple country publications ####
# The max number of country collaboration was 7
# To get the total number of published documents by multiple countries
# we need to count the documents by column, pass the results to rbind and then
# get then group by coutnry

auco_2df <- auco_dfs %>% 
  select(country_2) %>% 
  group_by(country_2) %>% 
  drop_na(country_2) %>%
  dplyr::summarise(tot_docs= n()) %>% 
  arrange(desc(tot_docs))

auco_3df <- auco_dfs %>% 
  select(country_3) %>% 
  group_by(country_3) %>% 
  drop_na(country_3) %>%
  dplyr::summarise(tot_docs= n()) %>% 
  arrange(desc(tot_docs)) %>% 
  rbind(setNames(auco_2df, names(auco_3df)))

auco_4df <- auco_dfs %>%
  select(country_4) %>% 
  group_by(country_4) %>% 
  drop_na(country_4) %>%
  dplyr::summarise(tot_docs= n()) %>% 
  arrange(desc(tot_docs)) %>% 
  rbind(setNames(auco_3df, names(auco_4df)))

auco_5df <- auco_dfs %>%
  select(country_5) %>% 
  group_by(country_5) %>% 
  drop_na(country_5) %>%
  dplyr::summarise(tot_docs= n()) %>% 
  arrange(desc(tot_docs)) %>% 
  rbind(setNames(auco_4df, names(auco_5df)))

auco_6df <- auco_dfs %>%
  select(country_6) %>% 
  group_by(country_6) %>% 
  drop_na(country_6) %>%
  dplyr::summarise(tot_docs= n()) %>% 
  arrange(desc(tot_docs)) %>% 
  rbind(setNames(auco_5df, names(auco_6df)))

auco_7df <- auco_dfs %>%
  select(country_7) %>% 
  group_by(country_7) %>% 
  drop_na(country_7) %>%
  dplyr::summarise(tot_docs= n()) %>% 
  arrange(desc(tot_docs)) %>% 
  rbind(setNames(auco_6df, names(auco_7df)))

auco_mcp <- auco_7df %>% # table with the total of multiple country collaboration
  group_by(country_7) %>% 
  dplyr::summarise(tot_docs= sum(tot_docs)) %>% 
  arrange(desc(tot_docs))

##### 5.3. Table SCP and MCP and total publications by country ####
count_prod <- full_join(auco_1df, # make a full join with SCP
                        auco_mcp, # and MCP 
                        by= c("country_1" = "country_7")) %>% 
  replace(is.na(.), 0) %>% # raplacing NA with 0
  mutate(PD = tot_docs.x + tot_docs.y) %>% # total publications by country
  rename( "SCP" = tot_docs.x, # total SCP
          "MCP" = tot_docs.y,
          "Country"= country_1) %>% # total MCP
  arrange(desc(PD)) %>% # order in descendent order by total
  head(10) # select the first 10 observations 

write_csv(count_prod, file = here("Data", 
                                     "Processed", 
                                     "country_prod.csv")) # writing as CSV for Rmd


CW <- cocMatrix(M, Field = "DE", type="matrix", sep=";",binary=binary)

DE_TM <- termExtraction(M,Field="DE"
                        ,remove.numbers=TRUE, 
                        stemming=stemming, 
                        language="english", 
                        remove.terms = remove.terms, 
                        synonyms = synonyms,
                        keep.terms=NULL, verbose=FALSE)






