# set working directory to source file location first
options(stringsAsFactors = FALSE)
library(quanteda)
require(topicmodels)

textdata <- read.csv("mn_dn_sutta_data_places_audiences.csv", sep="\t", encoding = "UTF-8")

library(ggplot2)
library(tidyverse)

# Basic bar plots of word count per country and audience, colored by sutta
textdata %>% 
  count(country, sutta, wt = wordcount, name = "wordcount") %>% 
  ggplot() +
    geom_bar(aes(x=country, y=wordcount, fill=sutta), stat="identity") +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
    theme(legend.position="none")

textdata %>% 
  count(audience, sutta, wt = wordcount, name = "wordcount") %>% 
  ggplot() +
  geom_bar(aes(x=audience, y=wordcount, fill=sutta), stat="identity") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  theme(legend.position="none")

##############################topic model##################################

# creating a paragraph variable with unique names to serve as docname
textdata <- textdata %>% mutate(sutta_para = paste(sutta,section_num,sep = "_"))

corpus <- corpus(textdata$segment_text, docnames = textdata$sutta_para, unique_docnames = FALSE)
# Build a dictionary of lemmas
lemma_data <- read.csv("baseform_en.tsv", encoding = "UTF-8")
# extended stopword list
stopwords_extended <- readLines("stopwords_en.txt", encoding = "UTF-8")
# Create a DTM (may take a while)
corpus_tokens <- corpus %>%
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>%
  tokens_tolower() %>%
  tokens_replace(lemma_data$inflected_form, lemma_data$lemma,
                 valuetype = "fixed") %>%
  tokens_remove(pattern = stopwords_extended, padding = T)

# collocations, if needed
collocations <- quanteda.textstats::textstat_collocations(corpus_tokens,
                                                               min_count = 25)
collocations <- collocations[1:250, ]
corpus_tokens <- tokens_compound(corpus_tokens, collocations)

# Create DTM, but remove terms which occur in less than 1%
# of all documents
DTM <- corpus_tokens %>%
  tokens_remove("") %>%
  dfm() %>%
  dfm_trim(min_docfreq = 3)
# have a look at the number of documents and terms in the
# matrix
dim(DTM)

# removing audience specific terms to make modeling more accurate
audience_terms <- c("mendicant", "brahmin", "brahman", "ascetic", "king", "god", "prince")
DTM <- DTM[, !(colnames(DTM) %in% audience_terms)]

# due to vocabulary pruning, we have empty rows in our DTM
# LDA does not like this. So we remove those docs from the
# DTM and the metadata
sel_idx <- rowSums(DTM) > 0
DTM <- DTM[sel_idx, ]
textdata <- textdata[sel_idx, ]

# load package topicmodels
require(topicmodels)
# number of topics
K <- 7
# compute the LDA model, inference via n iterations of Gibbs sampling
topicModel <- LDA(DTM, K, method="Gibbs", control=list(
  iter = 500,
  seed = 1,
  verbose = 25,
  alpha = 0.02))

# have a look a some of the results (posterior
# distributions)
tmResult <- posterior(topicModel)
# format of the resulting object
attributes(tmResult)

# topics are probability distributions over the entire
# vocabulary
beta <- tmResult$terms # get beta from results
dim(beta) # K distributions over ncol(DTM) terms

# for every document we have a probability distribution of
# its contained topics
theta <- tmResult$topics
dim(theta) # nDocs(DTM) distributions over K topics

terms(topicModel, 10)

top10termsPerTopic <- terms(topicModel, 10)
topicNames <- apply(top10termsPerTopic, 2, paste, collapse = " ")

# LDAvis browser
library(LDAvis)
library("tsne")
svd_tsne <- function(x) tsne(svd(x)$u)
json <- createJSON(phi = beta, theta = theta, doc.length = rowSums(DTM),
                   vocab = colnames(DTM), term.frequency = colSums(DTM), mds.method = svd_tsne,
                   plot.opts = list(xlab = "", ylab = ""))
serVis(json)

#################################select topics###############################################
# selecting manually
topicToFilter <- 5
# selecting by a term in the topic name
topicToFilter <- grep("reborn ", topicNames)[1]
# minimum share of content must be attributed to the
# selected topic
topicThreshold <- 0.9
selectedDocumentIndexes <- (theta[, topicToFilter] >= topicThreshold)
filteredCorpus <- corpus %>%
  corpus_subset(subset = selectedDocumentIndexes)
# show documents in the filtered corpus, controlling number of documents and length in characters to show
print(filteredCorpus, max_ndoc = 500, max_nchar = 1000)

#################################mean topics by country#########################################

library("reshape2")

# get mean topic proportions per country
topic_proportion_per_country <- aggregate(theta, by = list(country = textdata$country), mean)
# set topic names to aggregated columns
colnames(topic_proportion_per_country)[2:(K+1)] <- topicNames

# reshape data frame
vizDataFrame <- melt(topic_proportion_per_country, id.vars = "country")

# plot topic proportions per country as bar plot
require(pals)
ggplot(vizDataFrame, aes(x=country, y=value, fill=variable)) + 
  geom_bar(stat = "identity") + ylab("proportion") + 
  scale_fill_manual(values = paste0(alphabet(20), "FF"), name = "topic") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#################################mean topics by audience#########################################

# get mean topic proportions per audience
topic_proportion_per_audience <- aggregate(theta, by = list(audience = textdata$audience), mean)
# set topic names to aggregated columns
colnames(topic_proportion_per_audience)[2:(K+1)] <- topicNames

# reshape data frame
vizDataFrame <- melt(topic_proportion_per_audience, id.vars = "audience")

# plot topic proportions per audience as bar plot
require(pals)
ggplot(vizDataFrame, aes(x=audience, y=value, fill=variable)) + 
  geom_bar(stat = "identity") + ylab("proportion") + 
  scale_fill_manual(values = paste0(alphabet(20), "FF"), name = "topic") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))