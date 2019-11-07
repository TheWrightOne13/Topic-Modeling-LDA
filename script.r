#topic modeling script using IMDB data
#for figuring out themes when unsure of how many reside in data

library(topicmodels)
library(tm)
library(plyr)
library(dplyr)
library(tidyr)
library(magrittr)
library(lubridate)
library(ggplot2)
library(doBy)
library(Rmpfr)
library(psych)
library(slam)
library(stringr)
library(xlsx)
library(readxl)
library(plyr)

setwd("C:/Users/......")

Master<-read.csv("IMDB Dataset.csv",header = TRUE, na.strings = c(""))

# built on TM .6. 
#running the next two lines will download TM .6...might need to install "slam" package first if you don't have it as well
# require(devtools)
# install_version("tm", version = "0.6", repos = "http://cran.us.r-project.org")


#formatting for code
rat.display<-as.data.frame(Master)

#create unique id value for each text or document entry
rat.display$doc_id<-1:nrow(rat.display)

#create new dataframe or eliminate extra colums so that only "doc_id" and "text" exist. we can merge back later if necessary
rat.display$sentiment<-NULL

rat.display <- plyr::rename(rat.display, c("review" = "text"), warn_missing = TRUE, warn_duplicated = TRUE)

rat.display<-rat.display[,c(2,1)]

#for this example lets limit this dataset to 5k instead of 50k so that we don't wait forever
rat.display<-rat.display[1:5000,]


str(rat.display)
d <- list(content = "text", id = "doc_id")


#creating the corpus
rat.corpus <- tm::Corpus(tm::DataframeSource(rat.display), readerControl = list(reader = tm::readTabular(mapping = d)))

#Pick your flavor of analysis. Feel free to add tokenizers to control n-grams. For this example I stuck with word length, removing numbers, and removing punctuation. further cleaning of emojis and foreign characters may be necessary depending on data. all found in TM package. this step may take a bit depending on data size.
rat.dtm <- tm::DocumentTermMatrix(rat.corpus, control = list(stopwords = TRUE,
                                                             minWordLength = 2, removeNumbers = TRUE, removePunctuation = TRUE))

str(rat.dtm)

#if your analysis warrants a tfidf correction, here's some code. TM package offers a few ways to do tfidf, but this is how I wanted to analyze it

# rat_tfidf <- tapply(rat.dtm$v/slam::row_sums(rat.dtm)[rat.dtm$i], rat.dtm$j, mean) *
#   log2(tm::nDocs(rat.dtm)/slam::col_sums(rat.dtm > 0))
# summary(rat_tfidf)
# 
# ratreduced.dtm <- rat.dtm[,rat_tfidf >= 0.7022]
# summary(slam::col_sums(ratreduced.dtm))



harmonicMeanrat <- function(logLikelihoods, precision = 2000L) {
  ratMed <- median(logLikelihoods)
  as.double(ratMed - log(mean(exp(-mpfr(logLikelihoods,
                                        prec = precision) + ratMed))))
}

k <- 25
burnin <- 250
iter <- 250
keep <- 50
rowTotals<-apply(rat.dtm,1,sum) #these two lines needed to remove docs with sum 0. aka, no words.
rat.dtm<-rat.dtm[rowTotals>0,]#see above
ratfitted <- topicmodels::LDA(rat.dtm, k = k, method = "Gibbs",control = list(burnin = burnin, iter = iter, keep = keep) )
## assuming that burnin is a multiple of keep
ratlogLiks <- ratfitted@logLiks[-c(1:(burnin/keep))]

## This returns the harmomnic mean for k = 25 topics.
harmonic.mean(ratlogLiks)

#using the function we created earlier for comparison
harmonicMeanrat(ratlogLiks)

seqk <- seq(2, 20, 1)
burnin <- 250
iter <- 250
keep <- 50

#with example this took 10 mins on my computer. reduce dataset size if you want faster example results
system.time(fitted_many <- lapply(seqk, function(k) topicmodels::LDA(rat.dtm, k = k,
                                                                     method = "Gibbs",control = list(burnin = burnin,
                                                                                                     iter = iter, keep = keep) )))

ratlogLiks_many <- lapply(fitted_many, function(L)  L@logLiks[-c(1:(burnin/keep))])
rathm_many <- sapply(ratlogLiks_many, function(h) harmonicMeanrat(h))

#add the ggplot::annotate there because one of the librarys has a conflict with ggplot2 annotate. 
ratldaplot <- ggplot(data.frame(seqk, rathm_many), aes(x=seqk, y=rathm_many)) + geom_path(lwd=1.5) +
  theme(text = element_text(family= NULL),
        axis.title.y=element_text(vjust=1, size=16),
        axis.title.x=element_text(vjust=-.5, size=16),
        axis.text=element_text(size=16),
        plot.title=element_text(size=20)) +
  xlab('Number of Topics') +
  ylab('Harmonic Mean') +
  ggplot2::annotate("text", x = 15, y = -80000, label = paste("The optimal number of topics is", seqk[which.max(rathm_many)])) +
  ggtitle(expression(atop("Latent Dirichlet Allocation Analysis of Survey Comments", atop(italic("How many distinct topics in the survey?"), ""))))

ratldaplot

seqk[which.max(rathm_many)]


#run the model with the optimal amount of topics (you can use the optimal harmonic mean topics from above or set it to whatever you want). In this example I got 20 returned from the analysis, but set it to 15. Made more sense for the data. SHould be much faster than previous step
system.time(rat.model <- topicmodels::LDA(rat.dtm, 15, method = "Gibbs", control = list(iter=2000, seed = 0622)))


#Let's take a look at how the topics look. This code produces the first 5 themes and presents them here for eyeballing
#Use these topics to develop "themes". We can name them later if you choose so. 
rat.topics <- topicmodels::topics(rat.model, 1)
rat.terms <- as.data.frame(topicmodels::terms(rat.model, 30), stringsAsFactors = FALSE)
rat.terms[1:5]



rattopics.df<-as.data.frame(rat.topics)
rattopics.df<-dplyr::transmute(rattopics.df, EvalID = rownames(rattopics.df), Topic = rat.topics)
rattopics.df$EvalID<-as.integer(rattopics.df$EvalID)
# rat.display<-dplyr::inner_join(rat.display,rattopics.df,by="EvalID")

#create ranked list of terms by topic
rattopicTerms <- tidyr::gather(rat.terms, Topic)
rattopicTerms <- cbind(rattopicTerms, Rank = rep(1:30))
#top 3 terms by topic
rattopTerms <- dplyr::filter(rattopicTerms, Rank < 4)
rattopTerms <- dplyr::mutate(rattopTerms, Topic = stringr::word(Topic, 2))
rattopTerms$Topic <- as.numeric(rattopTerms$Topic)
rattopicLabel <- data.frame()

#for the 15 themes, what are the top 3 words and apply that to each row
for (i in 1:15){
  z <- dplyr::filter(rattopTerms, Topic == i)
  l <- as.data.frame(paste(z[1,2], z[2,2], z[3,2], sep = " " ), stringsAsFactors = FALSE)
  rattopicLabel <- rbind(rattopicLabel, l)
  
}
colnames(rattopicLabel) <- c("Label")
rattopicLabel

#By topic ranking
rat.topics <- topicmodels::topics(rat.model, k=4, threshold=0.07)
rattopics.df<- t(sapply(rat.topics, '[', seq(max(sapply(rat.topics, length)))))
colnames(rattopics.df) <- c("Topic1","Topic2","Topic3","Topic4")


## Dataframe of theta, the per-document probabilities from topics from the fitted model's posteriors
theta <- as.data.frame(topicmodels::posterior(rat.model)$topics)


#rat.terms is going to be the main dataset for ascribing terms to the themes

write.xlsx(rat.terms, ".....xlsx")





