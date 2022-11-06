# bibliometrix_dd_europe

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE)
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

```

# Biliometric Analysis of European Research on Digital divide

This study aims to examine the intellectual interactions, structural connections, and thematic relationships among European research components regarding the digital divide using different bibliometric techniques such as citation and co-citations analysis, bibliographic coupling, and co-word analysis. At the same time, this work endeavors to explore the corporate digital divide and answer the following research questions. *What are the most influential publications and authors? What are the thematic clusters among the citing publications? What are the intellectual structure and the knowledge foundations within the domain of the digital divide?* Understanding the digital divide as a multidimensional issue, *what are the thematic clusters among publications? Is the corporate digital divide considered a gap for future research?*

```{r nrows, include=FALSE}
MM <- read.csv(here("Data", "Processed", "M_EU.csv"))
docs <- nrow(MM)

```


This investigation will conduct a bibliometric analysis by merging data from three databases (Web of Science, Scopus, and Dimensions), limiting the search to authors with European affiliations within the business, management, economics, technology, and computer science disciplines. A total of `r nrow(MM)` unique documents, including articles, book chapters, conferences, and proceedings, were found in the three bibliographical sources. The results will be obtained by operating the R programming language using the bibliometrics package and the biblioshiny application.
