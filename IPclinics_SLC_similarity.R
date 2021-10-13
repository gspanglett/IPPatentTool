# initialize libraries
library(xml2)
library(data.table)
library(stringdist)
library(rvest)
library(stringi)
library(stringr)
library(tm)
library(ggplot2)
library(gplots)
library(igraph)
library(pdftools)
library(data.table)
library(plyr)
library(dplyr)
library(tidyr)
library(tidytext)
library(NLP)

library(patentsview)
library(ggplot2)
library(RColorBrewer)
library(htmltools)

library(leaflet)

# read dataset
data <- readRDS("pv_res.rds")

# unnest data
data_unnest <- unnest_pv_data(data$data, "patent_number")

# search within a field for a keyword - "combust"
subset_pat <- data_unnest$patents %>%
  filter(str_detect(patent_abstract,"combust"))
# convert patent numbers into numbers
as.numeric(subset_pat$patent_numbers)

#######################################################
# similarity calcs between abstracts
# create a corpus of the IP chapters
corpus_ch <- VCorpus(VectorSource(subset_pat$patent_abstract))

# pre-process text - remove punctuation, whitespace, numbers, stopwords, make all lower case
corpus_ch <- tm_map(corpus_ch, removePunctuation)
corpus_ch <- tm_map(corpus_ch, content_transformer(tolower))
corpus_ch <- tm_map(corpus_ch, stripWhitespace)
corpus_ch <- tm_map(corpus_ch, removeNumbers)
corpus_ch <- tm_map(corpus_ch, removeWords, stopwords("english"))

# create document-term matrix
#doc_term_matrix <- DocumentTermMatrix(corpus_ch, control=list(bounds=list(global=c(2,Inf)))) # restricts analysis to terms appearing only in 2 or more documents
doc_term_matrix <- DocumentTermMatrix(corpus_ch)
doc_term_matrix <- as.matrix(doc_term_matrix)

# method #1 - create binary distance matrix
distance_matrix <- as.matrix(dist(doc_term_matrix,method="binary"))
# add colnames, rownames
colnames(distance_matrix) <- subset_pat$patent_number
rownames(distance_matrix) <- subset_pat$patent_number
#write.csv(distance_matrix, file="test_distM_Sept28.csv")

# method #2 - calculate Jaccard distance between 5-character components
#distance_matrix_5gram <- stringdistmatrix(subset_pat$patent_abstract, subset_pat$patent_abstract, method = "jaccard", q=5)
#dist_5gram_mean <- rowMeans(distance_matrix_5gram)

#####################################################
# HEAT MAPS - binary distance matrix, ordered by data.frame
#jpeg("binary.jpg")
heatmap.2(distance_matrix,
          dendrogram='none',
          Rowv=FALSE,
          Colv=FALSE,
          symm=TRUE,
          trace='none',
          density.info='none',
          margin=c(10,10),
          #main="Similarity IP chapters, binary",
          #labCol=paste(chapters_by_year$tota_name,chapters_by_year$tota_year,sep="-"),
          #labRow=paste(chapters_by_year$tota_name,chapters_by_year$tota_year,sep="-"),
          cexRow=0.6,
          cexCol=0.6)
#dev.off()

# HEAT MAPS - binary distance matrix, use hierarchical clustering for order
#jpeg("5gram_HC.jpg")
heatmap.2(distance_matrix, 
          dendrogram='none',
          Rowv=TRUE,
          Colv=TRUE,
          symm=TRUE,
          symbreaks=FALSE,
          breaks=seq(0.8,1,length.out=100),
          trace='none',
          density.info='none',
          margin=c(10,10),
          main="binary, hierarchical",
          # labCol=paste(chapters_by_year$tota_name,chapters_by_year$tota_year,sep="-"),
          # labRow=paste(chapters_by_year$tota_name,chapters_by_year$tota_year,sep="-"),
          cexRow=0.8,
          cexCol=0.8)
#dev.off()

### extract assignees for patent numbers of interest - IGNORE, DIDNT USE
##subset_assign_t <- as.data.frame(cbind(data_unnest$assignees$patent_number,data_unnest$assignees$assignee_last_name,data_unnest$assignees$assignee_organization,
##                                       data_unnest$assignees$assignee_id,data_unnest$assignees$assignee_lastknown_longitude,data_unnest$assignees$assignee_lastknown_latitude))
##as.numeric(subset_assign_t$patent_numbers)
##subset_assign <- subset_assign_t[subset_assign_t$V1 %in% subset_pat$patent_number,]

### create table of top assignees (needs to user-defined, can be with a slider or fill-in box)
##assign_table <- as.data.frame(table(subset_assign$V3))
##assign_table_sort <- assign_table[order(assign_table$Freq, decreasing=TRUE),]

### assignees (geo): https://www.r-bloggers.com/2017/09/accessing-patent-data-with-the-patentsview-package/
subset_assign <- data$data$patents %>%
  unnest(assignees) %>%
  select(assignee_id, assignee_organization,patent_number,assignee_longitude,assignee_latitude) %>%
  group_by_at(vars(-matches("pat"))) %>%
  mutate(num_pats = n()) %>%
  ungroup() %>%
  distinct() %>%
  mutate(popup = paste0("<font color='Black'>",
                        htmlEscape(assignee_organization), "<br><br>Patents:",
                        num_pats, "</font>")) %>%
  mutate_at(vars(matches("_l")), as.numeric) %>%
  filter(!is.na(assignee_id))
#!!!! issue for follow-up - this excludes assignees who are not classified as organizations, could be important !!!!!

# extract patents from "combust" search - note, final set contains 201 patents (original combust group contained 206)
subset_assign_pat <- subset_assign[subset_assign$patent_number %in% subset_pat$patent_number,]
  
# extract similarity scores for patent numbers in assignee set - note, final set contains 197 patents (subset_assign_pat contains 201)
subset_dist_matrix_assign <- distance_matrix[rownames(distance_matrix) %in% subset_assign_pat$patent_number,]
# (suspected issue is multiple rows for patents with multiple assignees and exclusion of patents with non-organization assignees)

# make cool geo plot
# first, need to remove $patent_number and use distinct to avoid assignee duplicates (already have pat_num for each assignee)
subset_assign_pat_geo <- subset_assign_pat %>%
  select(-patent_number) %>%
  distinct()
# geo plot - can't really combine with similarity score without having assignee duplicates (plot uses assignee org and num_pat)
leaflet(subset_assign_pat_geo) %>%
  addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>%
  addCircleMarkers(lng = ~assignee_longitude, lat = ~assignee_latitude, popup = ~popup, ~sqrt(num_pats), color = "yellow")
#!!!! issue for follow-up - 13 rows with lat/long ignored; need to identify them and alert user, maybe put in a table with assignee country??


