# load the necessary packages
library(dimensionsR)

token <- dsAuth(key = "D177F8DF43DA4EA1ACF3832BC64A33B5")

# https://docs.dimensions.ai/dsl/datasource-publications.html
# https://github.com/massimoaria/dimensionsR

query <- dsQueryBuild(item = "publications", 
                      words = c("digital divide*", 
                                "digital inquality*", 
                                "digital gap*"),
                      words_boolean_op = "OR",
                      type = "article ; chapter", 
                      categories = "management; 
                                     economics; 
                                     commerce; 
                                     tourisim and services;
                                     information and computer science",
                      start_year = 1990, 
                      end_year = 2022,
                      output_fields = c("basics", 
                                        "extras", 
                                        "authors", 
                                        "concepts", 
                                        "book_doi",
                                        "abstract",
                                        "isbn",
                                        "reference_ids",
                                        "referenced_pubs"))


res <- dsApiRequest(token = token, query = query, limit = 0)

res$total_count

D_api <- dsApiRequest(token = token, query = query, step = 500, limit = res$total_count)

D_api <- D

D1 <- dsApi2df(D_api)



