# To see what the sep is
M$CR[1]

# To obtain the most frequent cited manuscripts:
CR <- citations(M, field = "article", sep = ";")
cbind(CR$Cited[1:10])
CRf <- data.frame(CR=unlist(CR))
CRf <- ldply (CR, data.frame)

# To obtain the most frequent cited first authors:
CR <- citations(M, field = "author", sep = ";")
cbind(CR$Cited[1:10])

topAU <- authorProdOverTime(M, k = 10, graph = T)

AU_CO <- M$AU_CO
MM <- as.data.frame(AU_CO)
A <- cocMatrix(MM, Field = "AU_CO", sep = ";")
au_co <- unlist(sort(Matrix::colSums(A), decreasing = TRUE))

TCm <- format(mean(as.numeric(M$TC), na.rm=TRUE),digits = 4)
TCmy <- format(mean(as.numeric(M$MostCitedPapers$TCperYear), na.rm = TRUE), digits = 4) 
CollIndex <- format(object$AuMultiAuthoredArt/sum(object$nAUperPaper>1),digits=3)  # Collaboration Index
MYfP <- as.numeric(substr(Sys.time(),1,4))-mean(M$PY,na.rm = TRUE)

Y=table(M$PY)
ny=dim(Y)[1]
CAGR<-as.numeric(round(((Y[ny]/Y[1])^(1/(ny-1))-1)*100,2))


results <- biblioAnalysis(M)
summary(results, k=20, pause=F, width=100)
sum(M$TC) / nrow(M)

listAU <- (strsplit(M$AU, ";"))
nAU <- lengths(listAU)
df <- data.frame(AU=trimws(unlist(listAU)), 
                 SR=rep(M$SR,nAU), 
                 TC=rep(M$TC, nAU)) 


AU <- df %>% 
  group_by(AU) %>% 
  dplyr::summarise(Total_articles= n(), 
                   Total_citations= sum(TC)) %>% 
  arrange(desc(Total_citations)) %>%
  ungroup() 



topic <-   which(grepl("", M$AB, ignore.case = TRUE))
m_cited <- head(order(-M$TC), 10)
# 10  219 1173 1355 1580 1714 corporate 
# 556  618 1024 1044 1120 1176 1231 1361 1414 1487 1573 1597 1648 1723 1860 business
# 308  379r  556  593  999 1006 1189 1573 1580 1600 1679 1874 firms

i <- topic
toJSON(M[i, c(2, 1, 3, 17, 13, 6, 8, 12, 22, 29, 23)], pretty= T)

# delete this doi 10.1145/1242572.1242583





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

Details <- c("Timespan", "Sources (Journals, Books, etc)", "Annual growth rate %", 
         "Average citations per doc", "Total published documents", "Articles", 
         "Book chapters", "Porceeding papers", "Conference papers") # 11 items

tot_period <-paste(min(M$PY, na.rm = T) , "-", max(M$PY, na.rm = T))
Y=table(M$PY)
ny=dim(Y)[1]
CAGR<-as.numeric(round(((Y[ny]/Y[1])^(1/(ny-1))-1)*100,2))
sources <- n_distinct(M$SO)
n_docs <- nrow(M)
avg_cite <- round(sum(M$TC)/nrow(M), 2)
arti <- as_vector(count(M[which(M$DT=='article'), 5]) + 
                    count(M[which(M$DT=='review'), 5]) )

bchap <- as_vector(count(M[which(M$DT=='book chapter'), 5]) + 
                     count(M[which(M$DT=='article; book chapter'), 5]) )

procee <- as_vector(count(M[which(M$DT=='proceedings paper'), 5]) + 
                     count(M[which(M$DT=='article; proceedings paper'), 5]) )

confe <- as_vector(count(M[which(M$DT=='conference paper'), 5]) )

arti+ bchap+ procee + confe

Total <- c(tot_period, sources, CAGR,
           avg_cite, n_docs, arti, bchap, procee, confe)



M_1p <- M %>% filter(PY <= 2007)
unique(M_1p$PY)


tot_period1 <-paste(min(M_1p$PY, na.rm = T) , "-", max(M_1p$PY, na.rm = T))
Y1=table(M_1p$PY)
ny1=dim(Y1)[1]
CAGR1<-as.numeric(round(((Y1[ny1]/Y1[1])^(1/(ny1-1))-1)*100,2))
sources1 <- n_distinct(M_1p$SO)
n_docs1 <- nrow(M_1p)
avg_cite1 <- round(sum(M_1p$TC)/nrow(M_1p), 2)
arti1 <- as_vector(count(M_1p[which(M_1p$DT=='article'), 5]) + 
                    count(M_1p[which(M_1p$DT=='review'), 5]) )

bchap1 <- as_vector(count(M_1p[which(M_1p$DT=='book chapter'), 5]) + 
                     count(M_1p[which(M_1p$DT=='article; book chapter'), 5]) )

procee1 <- as_vector(count(M_1p[which(M_1p$DT=='proceedings paper'), 5]) + 
                      count(M_1p[which(M_1p$DT=='article; proceedings paper'), 5]) )

confe1 <- as_vector(count(M_1p[which(M_1p$DT=='conference paper'), 5]) )

"Period 1" <- c(tot_period1, sources1, CAGR1,
           avg_cite1, n_docs1, arti1, bchap1, procee1, confe1 )

listAU1 <- (strsplit(M_1p$AU, ";"))
nAU1 <- lengths(listAU1)
df1 <- data.frame(AU=trimws(unlist(listAU1)), SR=rep(M_1p$SR,nAU1), TC=rep(M_1p$TC, nAU1)) 
AU1 <- df1 %>% 
  group_by(AU) %>% 
  dplyr::summarise(Total_articles= n(), 
                   Total_citations= sum(TC)) %>% 
  arrange(desc(Total_citations)) %>%
  ungroup() 

m_cited <- head(order(-M_1p$TC), 10)
i <- m_cited
toJSON(M_1p[i, c(2, 1, 3, 17, 13, 6, 8, 12, 22, 29, 23)], pretty= T)

results <- biblioAnalysis(M_1p)
summary(results, k=20, pause=F, width=100)

M_2p <- M %>% filter(PY > 2007 & PY <= 2014)
unique(M_2p$PY)

tot_period2 <-paste(min(M_2p$PY, na.rm = T) , "-", max(M_2p$PY, na.rm = T))
Y2=table(M_2p$PY)
ny2=dim(Y2)[1]
CAGR2<-as.numeric(round(((Y2[ny2]/Y2[1])^(1/(ny1-1))-1)*100,2))
sources2 <- n_distinct(M_2p$SO)
n_docs2 <- nrow(M_2p)
avg_cite2 <- round(sum(M_2p$TC)/nrow(M_2p), 2)
arti2 <- as_vector(count(M_2p[which(M_2p$DT=='article'), 5]) + 
                     count(M_2p[which(M_2p$DT=='review'), 5]) )

bchap2 <- as_vector(count(M_2p[which(M_2p$DT=='book chapter'), 5]) + 
                      count(M_2p[which(M_2p$DT=='article; book chapter'), 5]) )

procee2 <- as_vector(count(M_2p[which(M_2p$DT=='proceedings paper'), 5]) + 
                       count(M_2p[which(M_2p$DT=='article; proceedings paper'), 5]) )

confe2 <- as_vector(count(M_2p[which(M_2p$DT=='conference paper'), 5]) )

"Period 2" <- c(tot_period2, sources2, CAGR2,
            avg_cite2, n_docs2, arti2, bchap2, procee2, confe2 )

listAU2 <- (strsplit(M_2p$AU, ";"))
nAU2 <- lengths(listAU2)
df2 <- data.frame(AU=trimws(unlist(listAU2)), SR=rep(M_2p$SR,nAU2), TC=rep(M_2p$TC, nAU2)) 
AU2 <- df2 %>% 
  group_by(AU) %>% 
  dplyr::summarise(Total_articles= n(), 
                   Total_citations= sum(TC)) %>% 
  arrange(desc(Total_citations)) %>%
  ungroup() 

m_cited <- head(order(-M_2p$TC), 10)
i <- m_cited
toJSON(M_2p[i, c(2, 1, 3, 17, 13, 6, 8, 12, 22, 29, 23)], pretty= T)


results <- biblioAnalysis(M_2p)
summary(results, k=20, pause=F, width=100)

M_3p <- M %>% filter(PY > 2014 & PY <= 2021)
unique(M_3p$PY)

tot_period3 <-paste(min(M_3p$PY, na.rm = T) , "-", max(M_3p$PY, na.rm = T))
Y3=table(M_3p$PY)
ny3=dim(Y3)[1]
CAGR3<-as.numeric(round(((Y3[ny3]/Y3[1])^(1/(ny3-1))-1)*100,2))
sources3 <- n_distinct(M_3p$SO)
n_docs3 <- nrow(M_3p)
avg_cite3 <- round(sum(M_3p$TC)/nrow(M_3p), 2)
arti3 <- as_vector(count(M_3p[which(M_3p$DT=='article'), 5]) + 
                     count(M_3p[which(M_3p$DT=='review'), 5]) )

bchap3 <- as_vector(count(M_3p[which(M_3p$DT=='book chapter'), 5]) + 
                      count(M_3p[which(M_3p$DT=='article; book chapter'), 5]) )

procee3 <- as_vector(count(M_3p[which(M_3p$DT=='proceedings paper'), 5]) + 
                       count(M_3p[which(M_3p$DT=='article; proceedings paper'), 5]) )

confe3 <- as_vector(count(M_3p[which(M_3p$DT=='conference paper'), 5]) )

"Period 3" <- c(tot_period3, sources3, CAGR3,
            avg_cite3, n_docs3, arti3, bchap3, procee3, confe3 )

tot_docs <- data.frame(Details, `Period 1`, `Period 2`, `Period 3`, Total)


listAU3 <- (strsplit(M_3p$AU, ";"))
nAU3 <- lengths(listAU3)
df3 <- data.frame(AU=trimws(unlist(listAU3)), SR=rep(M_3p$SR,nAU3), TC=rep(M_3p$TC, nAU3)) 
AU3 <- df3 %>% 
  group_by(AU) %>% 
  dplyr::summarise(Total_articles= n(), 
                   Total_citations= sum(TC)) %>% 
  arrange(desc(Total_citations)) %>%
  ungroup() 

m_cited <- head(order(-M_3p$TC), 10)
i <- m_cited
toJSON(M_3p[i, c(2, 1, 3, 17, 13, 6, 8, 12, 22, 29, 23)], pretty= T)


results <- biblioAnalysis(M_3p)
summary(results, k=20, pause=F, width=100)
dominance(results, k= 10)


CR1 <- which(grepl("no title capture", M$CR, ignore.case = TRUE))
i <- 1450
toJSON(M[i, c(2, 1, 3, 17, 13, 6, 8, 12, 22, 29, 23, 11)], pretty= T)

ISI <- M %>% filter(DB == "ISI")
SCOPUS <- M %>% filter(DB == "SCOPUS")


CRisi <- citations(ISI, field = "article", sep = ";")
CRsco <- citations(SCOPUS, field = "article", sep = ";")

