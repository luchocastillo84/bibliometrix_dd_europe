# Load the necessary packages
library(prettydoc)
library(tidyverse)
library(rio)
library(here)
library(bibliometrix)
library(devtools)
library(readtext)
library(tidyr)
library(bib2df)
library(readr)
library(visdat)
library(stringr)
library(purrr)
library(dplyr)
library(bibtex)
install_formats()
library(bib2df)
library(RefManageR)
library(rbibutils)
library(rscopus)
library(readxl)
biblioshiny()

# loading the data from wos using \t tab as separator
# The query bellow brought 3338 documents in the WoS platform
# "Digital divide$" (Title) OR "digital divide$" (Author Keywords) OR 
# "Digital gap$" (Author Keywords) OR "Digital gap$" (Title) OR 
# "Digital inequalit$" (Title) OR "Digital inequalit$" (Author Keywords) and 
# 2022 (Exclude – Publication Years) and Article or Proceeding Paper or Book Chapters or


# Review Article (Document Types)
wos_file <- c("nwos_1_1000.txt", "nwos_1001_2000.txt", "nwos_2001_2616.txt")


#tot_wos <- convert2df(here("Data", "Raw", wos_file))
#vis_miss(tot_wos)

#save(tot_wos, file = "tot_wos.rda")

# export the files as .txt to upload it in biblioshiny 
#write_delim(tot_wos_3337, 
            #file = here("Data", "Processed", "text_m.txt"), 
            #col_names= F, quote =  "none", escape = "none")

# Converting the data into a data frame readable in the bibliometrx package
raw_wos <- convert2df(here("Data", "Raw", wos_file), 
                      dbsource = "wos", format = "plaintext")

M <- convert2df(here("Data", "Raw", wos_file), 
                dbsource = "wos", format = "plaintext")

# this code bring the country of each author into the column AU_CO
M <- metaTagExtraction(M, Field = "AU_CO", sep = ";")


### dimensionality reduction process by choosing key columns for bibliometrix  ###
tags <- colnames(M) # extracting the colnames into a vector
col_names <- as.data.frame(tags) # convert the vector into a df
write_excel_csv(col_names, "col_names_wos.xls") #export colnames to excel
per_na <- colSums(is.na(M)/ nrow(M)) # percentage of na for every column 
na_tags <- as.data.frame(per_na) # convert the vector into df
write_excel_csv(na_tags, "na_tags.xls") # export the percentages to excel

# According to the bibliometrix manual these are the key column to conduct
# a bibliometric analysis. 
# http://www.bibliometrix.org/vignettes/Data-Importing-and-Converting.html
# The reduction was from 73 columns to 23 
# also check the metaTagExtraction command which allow to extract more columns
M <- M[ , c(1, 59, 56, 36, 21, 19, 33, 4, 11, 
            51, 3, 58, 49, 52, 62, 67, 68, 69, 
            70, 71, 72, 73,20,64,66,48,28)]

vis_miss(M, sort_miss = T)

# Write a R object to test the file into biblioshiny (:it worked :)
save(M, file = "m_col_redux.rda")

M <- M %>% filter(!is.na(CR)) # filters out NA values in the citations references column
M <- M %>% filter(!is.na(AU_CO)) # filters out NA values in the author's country


head(M$AU_CO) # view how the countries are separated by ;
au_countries <- as.character(M$AU_CO) # convert the AU_CO into a chr vector
au_country <- as.data.frame(au_countries) # converting the AU_CO vector into df

## selecting the unique values such as USA;USA... of AU_CO for each document row 
countrydf <-  au_country %>% 
  mutate(split = str_split(au_countries, ";")) %>% # split
  mutate(split = map(.$split, ~ unique(.x))) %>% # drop duplicates
  mutate(split = map_chr(.$split, ~paste(.x, collapse = ";"))) # recombine

country_split <- as.data.frame(countrydf$split) ## converting the split vector into df
## split the vector AU_CO into a df
## count the split for every ; semicolon that appears in the line resulting number of columns
ncols <- max(stringr::str_count(country_split$`countrydf$split`, ";")) + 1

colmn <- paste("country_", 1:ncols, sep ="") # create and paste the name of the colums

#3 convert the splits into columns 
authors_countries <-
  tidyr::separate(
    data = country_split,
    col = `countrydf$split`,
    sep = ";",
    into = colmn,
    remove = T
  )

vis_miss(authors_countries)
## filters the unique countries in the database
tot_countries <- as.data.frame(cbind(
                              c(unique(df$country_1),
                              unique(df$country_2),
                              unique(df$country_3),
                              unique(df$country_4),
                              unique(df$country_5),
                              unique(df$country_6),
                              unique(df$country_7))))

## filters out the NA values that appear in the column 
tot_countries <- unique(tot_countries) %>% filter(!is.na(V1))
write_excel_csv(tot_countries, "country_list.xls") # save in xls the country list
## load the countries, codes and regions 
country_code_reg <- read_excel(here("Data", "Processed", "country_code_region.xlsx"))

## merging 
sel_col <- country_code_reg %>% select(Country, Continent) %>% rename("Continent_1" = "Continent")
M_plus <- M_plus %>% inner_join(y= sel_col,  by= c( "country_1" = "Country"))

au_con_reg <- authors_countries %>% 
  inner_join(y = country_code_reg[ , c(1,4)], by = c("country_1" = "Country")) %>% 
  rename("Continent_1" = "Continent")

au_con_reg <- au_con_reg %>% 
  left_join(y = country_code_reg[ , c(1,4)], by = c("country_2" = "Country")) %>% 
  rename("Continent_2" = "Continent")

au_con_reg <- au_con_reg %>% 
  left_join(y = country_code_reg[ , c(1,4)], by = c("country_3" = "Country")) %>% 
  rename("Continent_3" = "Continent")

au_con_reg <- au_con_reg %>% 
  left_join(y = country_code_reg[ , c(1,4)], by = c("country_4" = "Country")) %>% 
  rename("Continent_4" = "Continent")

au_con_reg <- au_con_reg %>% 
  left_join(y = country_code_reg[ , c(1,4)], by = c("country_5" = "Country")) %>% 
  rename("Continent_5" = "Continent")

au_con_reg <- au_con_reg %>% 
  left_join(y = country_code_reg[ , c(1,4)], by = c("country_6" = "Country")) %>% 
  rename("Continent_6" = "Continent")

au_con_reg <- au_con_reg %>% 
  left_join(y = country_code_reg[ , c(1,4)], by = c("country_7" = "Country")) %>% 
  rename("Continent_7" = "Continent")

au_con_reg <- au_con_reg %>% 
  left_join(y = country_code_reg[ , c(1,4)], by = c("country_8" = "Country")) %>% 
  rename("Continent_8" = "Continent")

## filtering the columns continet that contains Europe 
eu_authors <- au_con_reg %>% filter(Continent_1 == "Europe" |
                                      Continent_2 == "Europe" |
                                      Continent_3 == "Europe" |
                                      Continent_4 == "Europe" |
                                      Continent_5 == "Europe" |
                                      Continent_6 == "Europe" |
                                      Continent_7 == "Europe" |
                                      Continent_8 == "Europe")


namebyrow<- as.data.frame(rownames(M)) %>% mutate(i_num = row_number())
M_2filter <-  cbind(namebyrow, au_con_reg)


M_2filter <- M_2filter %>% filter(Continent_1 == "Europe" |
                                   Continent_2 == "Europe" |
                                   Continent_3 == "Europe" |
                                   Continent_4 == "Europe" |
                                   Continent_5 == "Europe" |
                                   Continent_6 == "Europe" |
                                   Continent_7 == "Europe" |
                                   Continent_8 == "Europe")

## filter by the row index so the df does not lose the attributes 
M_index <- M_2filter$`rownames(M)`
M <- M[M_index ,]
save(M, file = "M_EU.rda") # save as rda file for bibliometrix


glimpse(M)
class(M_eu)



# this code will filter out the NA values in book number BN, DIO = DI and cited references CR
# & is.na(BN) & is.na(DI)
na_adress <- M %>% filter(is.na(C1))
m_redux <- M %>% filter(!is.na(CR))
m_redux <- m_redux %>% filter(!is.na(BN) | !is.na(DI))
sum(is.na(m_redux$C1))





M1 <- convert2df(here("Data", "Raw", "scopus_1_1786.csv"),
                      dbsource = "scopus", format = "csv")

M1 <- metaTagExtraction(M1, Field = "AU_CO", sep = ";")
M1 <- M1 %>% mutate("SC" = "", "WC" = "", "Z9" = "")
M1 <- M1 %>% rename("TI" = "TIs", "ID" = "Indexed.Keywords", "PU" = "Publisher")
n_distinct(M1$URL)


M1 <- M1[, c(1, 4, 6, 31, 26, 20, 19, 18, 17, 
             24, 23, 14, 5, 39, 30, 29, 33, 34, 
             35, 36, 37, 38, 13, 40, 41, 25, 21)]



colnames_W <- colnames(M)
colnames_S <- colnames(M1)
colnames_WS <- as.data.frame(paste(colnames_W, colnames_S))
M1$DI[which(M1$DI == "")] <- NA
M1 %>% group_by(DT) %>% summarize(DOI = n_distinct(DI), NA_ = sum(is.na(DI)))
n_distinct(raw_wos$D2)
raw_wos %>% group_by(DT) %>% summarize(count = n(), DOI = n_distinct(DI), na_doi = sum(is.na(DI)),
                                       na_bn = sum(is.na(D2)))
n_distinct(M$DI)
M_merged <- rbind(M, M1)
n_distinct(M_merged$TI)

glimpse(M1)

vis_miss(M1)
save(M1, file = "M1_EU.rda")

results1 <- biblioAnalysis(M1)
summary(results1, k=20, pause=F, width=130)

### dimensionality reduction process by choosing key columns for bibliometrix  ###
tags_sco <- colnames(M1) # extracting the colnames into a vector
col_names_sco <- as.data.frame(tags_sco) # convert the vector into a df
write_excel_csv(col_names_sco, "col_names_scopus.xls") #export colnames to excel
per_na_sco <- colSums(is.na(M1)/ nrow(M1)) # percentage of na for every column 
na_tags_sco <- as.data.frame(per_na_sco) # convert the vector into df
write_excel_csv(na_tags_sco, "na_tags_sco.xls") # export the percentages to excel




M1 <- bib2df(here("Data", "Raw", "SD_1_100.bib"), separate_names = F)

M2 <- convert2df(here("Data", "Raw", "dim_1_2347.csv"),
                 dbsource = "dimensions", format = "csv")


# trying to connect the rsopus packages with the Scopus API but
# there is a authentication error 
Elsevier_API = "abb3c9ab7a77484fbc524a4b240e8199"
hdr = inst_token_header(token)
hdr = inst_token_header(token)
res = author_df(last_name = "Castillo Tellez", first_name = "Luis Carlos", 
                verbose = FALSE, general = FALSE, headers = hdr, api_key = Elsevier_API)
elsevier_authenticate(api_key = 'abb3c9ab7a77484fbc524a4b240e8199', api_key_error = F, 
                      choice = NULL, verbose = F, headers = "X-ELS-APIKey")



# this code will filter out the NA values in book number BN, DIO = DI and cited references CR
# & is.na(BN) & is.na(DI)
na_adress <- M %>% filter(is.na(C1))
m_redux <- M %>% filter(!is.na(CR))
m_redux <- m_redux %>% filter(!is.na(BN) | !is.na(DI))
sum(is.na(m_redux$C1))




save(m_col_redux, file = "m_col_redux.rda")

vis_miss(na_doi)
na_filter <- M %>% filter(is.na(AU_CO))
here("Data", "Processed", save(na_filter, file = "na_filter.rda"))
write_csv(na_abs, "na_abs.csv")
r <- tempfile(fileext = ".R")
bibConvert("na_filter.rda", "bibtex", "r", "bibtex")
glimpse(M)
length(unique(M$DI))





na_doi <- M %>% filter(is.na(DI))
sum(is.na(na_doi$BN))
na_booknum <- na_doi %>% filter(is.na(BN))

save(na_doi, file = "na_doi.rda")
is.BibEntry(na_doi)
as.BibEntry(na_doi)
toBibtex(na_filter)
na_abs <- M %>% filter(is.na(AB))

# Summary report of bibliometric analysis in the sample 3337 documents
results <- biblioAnalysis(M_eu)
summary(results, k=20, pause=F, width=130)

## observe the observation ## 6, 27 using as sep = ; 
country <- strsplit(M$AU_CO, ";") [27][1]
country

## splitting the string vectors of the authors' country
countries <-  sapply(strsplit(as.character(M$AU_CO), ";"), `[`, 6)
country1 <- as.data.frame(countries)
country1 %>% group_by(countries) %>% summarize(count= n()) %>% arrange(desc(count))

#unique(M$PA)
# visualization of missing data AU_CO= 4.02%, AU_UN = 6.86% , DOI DI = 37.43%
vis_miss(M, sort_miss = TRUE)

# converting the authors country AU_CO vector as character and data frame
au_countries <- as.character(M$AU_CO)
au_country <- as.data.frame(au_countries)

## selecting the unique values of AU_CO for each document 
df1 <- au_country %>%
  mutate(split = str_split(au_countries, ";")) %>% # split
  mutate(split = map(.$split, ~ unique(.x))) %>% # drop duplicates
  mutate(split = map_chr(.$split, ~paste(.x, collapse = ";"))) # recombine

df2 <- as.data.frame(df1$split)
au_cou <-  data.frame(do.call("rbind", 
                              strsplit(as.character(df2), ";", fixed = T)))

ncols <- max(stringr::str_count(df2$`df1$split`, ";")) + 1

colmn <- paste("col", 1:ncols)

df <-
  tidyr::separate(
    data = df2,
    col = `df1$split`,
    sep = ";",
    into = colmn,
    remove = T
  )

vis_miss(df)

count_co <- df %>% group_by(`col 1`,`col 2`,`col 3`) %>% 
  summarize(count= n()) %>% arrange(desc(count))
count_co
unique(df$`col 5`)

country_au <- lapply(df2,  data.frame(do.call("rbind", 
                                    strsplit(as.character(df2), ";"))))
  
  
p <- lapply(df2, str_split( ";"))



unique(df2)

