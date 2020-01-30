#This script contains all of the logic nessescary to complete an unsupervised learning analysis of the Kaggle
# 2019 ML & DS survey challenge. It also produces the output used in the accompanying powerpoint
#  (Kaggle_2019_presentation_Samuel_Andrews), including a PCA of numeric converted variables, an MCA of categorical data,
#  association rules for text response, and sentiement analysis. A number of .csv files were created in openrefine
#  prior that are used here and included within this branch, as certain steps required the response to be formatted in 
#  particular ways. A full write up of everything done here is also found within this branch as
#  "Kaggle_2019_write_up_Complete_Samuel_Andrews"


require(proxy)
require(cab)
require(cluster)
require(vegan)
require(ade4)
require(MASS)
require(rgl)
require(cba)
require(proxy)
require(cluster)
require(corrplot)
require(smacof)
require(dplyr)
require(ggplot2)
require(fastICA)
require(FactoMineR)
require(pactoextra)
require(factoextra)
require(gplots)
require(mixOmics)
require(fpc)
require(proxy)
require(cab)
require(cluster)
require(vegan)
require(ade4)
require(MASS)
require(rgl)
require(cba)
require(proxy)
require(cluster)
require(corrplot)
require(smacof)
require(dplyr)
require(ggplot2)
require(fastICA)
require(FactoMineR)
require(pactoextra)
require(factoextra)
require(gplots)
require(mixOmics)
require(fpc)

unpreped_multC <- read.csv("multiple_choice_responses_MCA-prep-foroOF.csv")

Multi_choice <- read.csv("MCA_Ready.csv")

names(MCA_Ready)


MCA_Ready[-c(352)]



Multi.mat = apply(MCA_Ready,2,as.factor)
Mutli.MCA = MCA(Multi.mat)

plot(Mutli.MCA)


plot(Mutli.MCA, invisible = c("ind"))

plot(Mutli.MCA, invisible = c("var"))



#MCA_Ready2 <- MCA_Ready[-c(352)]


#Multi.mat = apply(MCA_Ready2,2,as.factor)
#Mutli.MCA = MCA(Multi.mat)

#plot(Mutli.MCA)


#plot(Mutli.MCA, invisible = c("ind"))

#plot(Mutli.MCA, invisible = c("var"))


fviz_mca_ind(Mutli.MCA, col.ind="cos2") +
  scale_color_gradient2(low="lightsalmon", mid = "lightsalmon2", 
                        high="lightsalmon4")+theme_minimal() 


fviz_mca_var(Mutli.MCA, col.var = "contrib") +
  scale_color_gradient2(low="springgreen", mid = "springgreen2", 
                        high="springgreen4")+theme_minimal()


fviz_mca_biplot(Mutli.MCA, select.var = list(contrib = 50)  )

fviz_mca_biplot(Mutli.MCA, select.var = list(contrib = 10)  )

fviz_mca_biplot(Mutli.MCA, select.var = list(contrib = 25)  )

fviz_mca_biplot(Mutli.MCA, select.var = list(contrib = 100)  )

fviz_mca_biplot(Mutli.MCA, select.var = list(contrib = 150)  )

fviz_mca_biplot(Mutli.MCA, select.var = list(contrib = 250), geom.ind = c("point")  )

fviz_mca_biplot(Mutli.MCA, select.var = list(contrib = 100), geom.ind = c("point")  ) +
  scale_color_gradient2(low="springgreen", mid = "springgreen2", 
                        high="springgreen4")+theme_minimal()



#Cluster

Kclust <- read.csv("Cluster_columns_Recoded_Relabeled2.csv")


clust.grps = function(X,grps,parcoord=F,suppress=F) {
  require(MASS)
  k = length(unique(grps))
  p = dim(X)[[2]]
  Xmeans = matrix(0,nrow=length(unique(grps)),ncol=p+1)
  X = as.data.frame(X)
  X[is.na(X)] <- 0
  for (i in 1:k){
    cat("\n")
    cat(paste("Cluster",i,"\n"))
    cat("=======================================================================\n")
    if (suppress==F){
      cat(row.names(X)[grps==i])
      cat("\n\n")}
    cat("Variable means in this cluster are:\n")
    cat("----------------------------------------------------------------------\n")
    print(apply(X[grps==i,],2,mean))
    Xmeans[i,]=c(apply(X[grps==i,],2,mean),as.numeric(i))
    cat("\n\n")
    
  } 
  if (parcoord) {
    par(mfrow=c(2,1))
    parcoord(Xmeans[,-(p+1)],col=as.numeric(Xmeans[,(p+1)])+2,
             lwd=2,var.label=T)
    parcoord(X,col=as.numeric(grps)+2,lwd=2)
  }
}


#Kclust[is.na(Kclust)] <- 0


Kclust.mat <- Kclust[,-c(1)]

Kclust.mat <- scale(Kclust.mat)

#clust.grps(Kclust.mat,4)

#Kclust.agnes = agnes(Kclust.mat,metric = "euclidean",method = "complete", diss = F)

Kclust.de <- dist(Kclust.mat, method = "euclidean")

Kclust.dman <- dist(Kclust.mat, method = "manhattan")

Kclust.dcan <- dist(Kclust.mat, method = "canberra")

Kclust.dmax <- dist(Kclust.mat, method = "maximum")


#euclidean
Kclust.desing <- hclust(Kclust.de, method = "sing")
plot(Kclust.desing, labels = NULL, cex = 0.01)


Kclust.decomp <- hclust(Kclust.de, method = "comp")
plot(Kclust.decomp, labels = NULL, cex = 0.01)


Kclust.deave <- hclust(Kclust.de, method = "ave")
plot(Kclust.deave, labels = NULL, cex = 0.01)


Kclust.deward <- hclust(Kclust.de, method = "ward.D")
plot(Kclust.deward, labels = NULL, cex = 0.01)


#manhattan
Kclust.dmansing <- hclust(Kclust.dman, method = "sing")
plot(Kclust.dmansing, labels = NULL, cex = 0.01)


Kclust.dmancomp <- hclust(Kclust.dman, method = "comp")
plot(Kclust.dmancomp, labels = NULL, cex = 0.01)


Kclust.dmanave <- hclust(Kclust.dman, method = "ave")
plot(Kclust.dmanave, labels = NULL, cex = 0.01)


Kclust.dmanward <- hclust(Kclust.dman, method = "ward.D")
plot(Kclust.dmanward, labels = NULL, cex = 0.01)




#canberra
Kclust.dcansing <- hclust(Kclust.dcan, method = "sing")
plot(Kclust.dcansing, labels = NULL, cex = 0.01)


Kclust.dcancomp <- hclust(Kclust.dcan, method = "comp")
plot(Kclust.dcancomp, labels = NULL, cex = 0.01)


Kclust.dcanave <- hclust(Kclust.dcan, method = "ave")
plot(Kclust.dcanave, labels = NULL, cex = 0.01)


Kclust.dcanward <- hclust(Kclust.dcan, method = "ward.D")
plot(Kclust.dcanward, labels = NULL, cex = 0.01)



#max
Kclust.dmaxsing <- hclust(Kclust.dmax, method = "sing")
plot(Kclust.dmaxsing, labels = NULL, cex = 0.01)


Kclust.dmaxcomp <- hclust(Kclust.dmax, method = "comp")
plot(Kclust.dmaxcomp, labels = NULL, cex = 0.01)


Kclust.dmaxave <- hclust(Kclust.dmax, method = "ave")
plot(Kclust.dmaxave, labels = NULL, cex = 0.01)


Kclust.dmaxward <- hclust(Kclust.dmax, method = "ward.D")
plot(Kclust.dmaxward, labels = NULL, cex = 0.01)


#canberra ward

Kclust.clust1 = cutree(Kclust.dcanward,5)

clust.grps(Kclust.mat,Kclust.clust1,parcoord = F, suppress = T)


Kclust.clust1 = cutree(Kclust.dcanward,4)

clust.grps(Kclust.mat,Kclust.clust1, suppress = T)

#Kclust.dist = dist(Kclust.mat)
Kmds = cmdscale(Kclust.dcan, k = 2)
plot(Kmds, type = "n")

text(Kmds, labels = as.character(Kclust.clust1), col = as.numeric(Kclust.clust1))



Kclust.pca = PCA(Kclust.mat)
plot(Kclust.pca, col.ind = Kclust.clust1)



#PAM

Kclust <- read.csv("Cluster_columns_Recoded_Relabeled2.csv")

#Kclust[is.na(Kclust)] <- 0

Kclust.mat2 <- Kclust[,-c(1)]

Kclust.mat2 <- scale(Kclust.mat)

Kfit = pamk(Kclust.mat2,krange = 11:15, diss = F, criterion = "asw", critout = T)

kfit4 = pam(Kclust.mat2, diss = F, k = 4, keep.diss=TRUE)
plot(kfit2)

kfit5 = pam(Kclust.mat2, diss = F, k = 3, keep.diss=TRUE)
plot(kfit2)

kfit6 = pam(Kclust.mat2, diss = F, k = 4, keep.diss=TRUE)
plot(kfit2)

#clara




#Association rules on text

require(lubridate)
require(stringr)
require(tm)
require(wordcloud)
require(arules)
require(arulesViz)
require(dplyr)
require(tidyr)


q14 <- read.csv("Q14_full_textcsv_nullremoved.csv")

q28 <- read.csv("Q28_full_textcsv_null_removed.csv")


CleanCorpus = function(tweets) {
  tweetCorpus = Corpus(VectorSource(tweets))
  tweetCorpus = tm_map(tweetCorpus,content_transformer(tolower))
  tweetCorpus = tm_map(tweetCorpus,removePunctuation)
  tweetCorpus = tm_map(tweetCorpus,removeNumbers)
  tweetCorpus = tm_map(tweetCorpus,removeWords,stopwords("english"))
  tweetCorpus = tm_map(tweetCorpus, removeWords, c("google","microsoft"))
  return(tweetCorpus)
}



q14corpus = CleanCorpus(q14$ï..Q14_full_text)

q14tdm = TermDocumentMatrix(q14corpus)

q14.mat = as.matrix(q14tdm)

q14.wfreq = sort(rowSums(q14.mat), decreasing = T)

q14.wfreq[1:20]

barplot(q14.wfreq[1:20], xlab = "Frequent Data Anlysis Tools", cex.names = 0.8)




wordcloud(words = names(q14.wfreq), freq = q14.wfreq, random.order = F, col = rainbow(1000), min.freq = 50)

wordcloud(words = names(q14.wfreq), freq = q14.wfreq, random.order = F, col = rainbow(1000), min.freq = 10)

wordcloud(words = names(q14.wfreq), freq = q14.wfreq, random.order = F, col = rainbow(1000), min.freq = 100)


findAssocs(q14tdm, "jupyter", 0.1)


findAssocs(q14tdm, "excel", 0.1)


findAssocs(q14tdm, "sheets", 0.1)

findAssocs(q14tdm, "tableau", 0.05)


temp = t(q14.mat)

temp[temp>1] = 1

q14.trans = as(temp,"transactions")



q14.arules  = apriori(q14.trans, parameter = list(supp = 0.001, conf = 0.5, target = "rules"),
                      appearance = list(none = c("jupyter","learning"))
                      )
inspect(q14.arules)

plot(q14.arules, method = "grouped")

plot(q14.arules, method = "graph", engine = "interactive")

ruleExplorer(q14.arules)

#28

q28 <- read.csv("Q28_full_textcsv_null_removed.csv")

trimws(q28$Q_28_full, which = c("both"))

CleanCorpus = function(tweets) {
  tweetCorpus = Corpus(VectorSource(tweets))
  tweetCorpus = tm_map(tweetCorpus,content_transformer(tolower))
  tweetCorpus = tm_map(tweetCorpus,removePunctuation)
  tweetCorpus = tm_map(tweetCorpus,removeNumbers)
  tweetCorpus = tm_map(tweetCorpus,removeWords,stopwords("english"))
  tweetCorpus = tm_map(tweetCorpus, removeWords, c("google","microsoft"))
  return(tweetCorpus)
}

q28corpus = CleanCorpus(q28$Q_28_full)

q28tdm = TermDocumentMatrix(q28corpus)

q28.mat = as.matrix(q28tdm)

q28.wfreq = sort(rowSums(q28.mat), decreasing = T)

q28.wfreq[1:50]

barplot(q28.wfreq[1:20], xlab = "Popular Macinhe Learning Frameworks", cex.names = 0.7)

wordcloud(words = names(q28.wfreq), freq = q28.wfreq, random.order = F, col = rainbow(100), min.freq = 50)

wordcloud(words = names(q28.wfreq), freq = q28.wfreq, random.order = F, col = rainbow(1000))

wordcloud(words = names(q28.wfreq), freq = q28.wfreq, random.order = T, col = rainbow(20), min.freq = 20)

findAssocs(q28tdm, "scikitlearn", 0.1)

findAssocs(q28tdm, "tensorflow", 0.1)

findAssocs(q28tdm, "keras", 0.1)

findAssocs(q28tdm, "randomforest", 0.1)

findAssocs(q28tdm, "xgboost", 0.1)


temp = t(q28.mat)

temp[temp>1] = 1

q28.trans = as(temp,"transactions")



q28.arules  = apriori(q28.trans, parameter = list(supp = 0.1, conf = 0.5, target = "rules", maxlen = 3, minlen = 1)
                      
                      )
q28.arules = sort(q28.arules, by="confidence", decreasing=TRUE)
inspect(q28.arules)

ruleExplorer(q28.arules)


#sentiment

require(lubridate)
require(stringr)
require(tm)
require(wordcloud)
require(arules)
require(arulesViz)
require(dplyr)
require(tidyr)
require(sentimentr)
require(textclean)
require(lexicon)


sent <- read.csv("Subset of other_text_responses 34.csv")

CleanCorpus = function(tweets) {
  tweetCorpus = Corpus(VectorSource(tweets))
  tweetCorpus = tm_map(tweetCorpus,content_transformer(tolower))
  tweetCorpus = tm_map(tweetCorpus,removePunctuation)
  tweetCorpus = tm_map(tweetCorpus,removeNumbers)
  tweetCorpus = tm_map(tweetCorpus,removeWords,stopwords("english"))
  return(tweetCorpus)
}

sentcorpus = CleanCorpus(sent$ï..Full_Other_Text_12_13_19_5_9)

sentTDM = TermDocumentMatrix(sentcorpus)

sent.mat = as.matrix(sentTDM)

sent.wfreq = sort(rowSums(sent.mat), decreasing = T)

sent.wfreq[1:50]

barplot(sent.wfreq[1:20], xlab = "Popular Other Text Responses", cex.names = 0.8)

wordcloud(words = names(sent.wfreq), freq = sent.wfreq, random.order = F, col = rainbow(100), min.freq = 50)

wordcloud(words = names(sent.wfreq), freq = sent.wfreq, random.order = F, col = rainbow(1000))

wordcloud(words = names(sent.wfreq), freq = sent.wfreq, random.order = F, col = rainbow(20), min.freq = 20)

temp = sent$ï..Full_Other_Text_12_13_19_5_9

temp = str_replace_all(temp,"&amp", replacement = "and")

temp_sente = get_sentences(temp)

sent.sen = sentiment(temp_sente, polarity_dt = hash_sentiment_huliu)

hist(sent.sen$sentiment)


par(mar = c(2, 2, 2, 2))

sent.nz = sent.sen$sentiment[sent.sen$sentiment!=0]
hist(sent.nz)

mean(sent.sen3$sentiment)

mean(sent.sen3$sentiment[sent.sen$sentiment!=0])



sent.sen2 = sentiment(temp_sente, polarity_dt = hash_sentiment_senticnet)

hist(sent.sen2$sentiment)

sent.nz2 = sent.sen2$sentiment[sent.sen2$sentiment!=0]

hist(sent.nz2)



sent.sen3 = sentiment(temp_sente, polarity_dt = hash_sentiment_socal_google)

hist(sent.sen3$sentiment)

sent.nz3 = sent.sen3$sentiment[sent.sen3$sentiment!=0]

hist(sent.nz3)



#pca

require(missMDA)

Kpca <- read.csv("Cluster_columns_Recoded_Relabeled2.csv")

Kpca.mat <- Kpca[,-c(1)]

Kpca.mat <- scale(Kpca.mat)

cor(Kpca.mat)


Kpca.ms <- estim_ncpPCA(Kpca.mat,method.cv = "Kfold", verbose = FALSE)

Kpca.ms$ncp

plot(0:5, Kpca.ms$criterion, xlab = "Dim", ylab = "MSEP", type = "line")


Kpca.comp <- imputePCA(Kpca.mat, ncp = Kpca.ms$ncp)
Kpca.comp$completeObs[1:10,] # the imputed data set

#imp <- cbind.data.frame(res.comp$completeObs,WindDirection)

res.pca <- PCA(Kpca.comp$completeObs, ncp = Kpca.ms$ncp, graph=FALSE)


res.pca$var$coord

par(mar = c(4, 4, 4, 4))

plot(res.pca, lab="quali")

plot(res.pca, choix="var")



#alt using 2



Kpca.comp <- imputePCA(Kpca.mat, ncp = 2)



res.pca <- PCA(Kpca.comp$completeObs, ncp = 2, graph=FALSE)

res.pca$var

res.pca$ind$cos2

par(mar = c(4, 4, 4, 4))

plot(res.pca, lab="quali")

plot(res.pca, choix="var")

vars <- get_pca_var(res.pca = res.pca)

corrplot(vars$cos2, is.corr = F )

fviz_pca_biplot(res.pca, label = "var", col.var = "black", col.ind = "lightgray")


