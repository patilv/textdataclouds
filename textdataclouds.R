load("textdata.rda")
library(tm)
library(wordcloud)
summary(textdata)
level1=paste(as.character(textdata$details[textdata$division==levels(textdata$division)[1]]), collapse=" ")
level2=paste(as.character(textdata$details[textdata$division==levels(textdata$division)[2]]), collapse=" ")
level3=paste(as.character(textdata$details[textdata$division==levels(textdata$division)[3]]), collapse=" ")
level4=paste(as.character(textdata$details[textdata$division==levels(textdata$division)[4]]), collapse=" ")
level5=paste(as.character(textdata$details[textdata$division==levels(textdata$division)[5]]), collapse=" ")
level6=paste(as.character(textdata$details[textdata$division==levels(textdata$division)[6]]), collapse=" ")


wordcloudcategory<-function(text)
{
    textCorpus<-Corpus(VectorSource(text))
    textTDM<-TermDocumentMatrix(textCorpus,control=list(removePunctuation=TRUE,
                                                        #stopwords=c(stopwords('english')),
                                                        removeNumbers=TRUE,tolower=TRUE))
    tdMatrix <- as.matrix(textTDM) # creating a data matrix
    
    sortedMatrix<-sort(rowSums(tdMatrix),decreasing=TRUE) # calculate row sum of each term and sort in descending order (high freq to low)
    cloudFrame<-data.frame(word=names(sortedMatrix),freq=sortedMatrix)#extracting names from named list in prev command and binding together into a dataframe with frequencies - called cloudFrame, names in separate columns
    
    wcloudcategory<-wordcloud(cloudFrame$word,cloudFrame$freq,colors=brewer.pal(8,"Dark2"), 
                              scale = c(5, 1), rot.per=.25, random.order=TRUE)
    
    print(wcloudcategory)
  }
  
}


  png(filename="level1.png",width=1200,height=1050)
  wordcloudcategory(level1)
  dev.off()


png(filename="level2.png",width=1200,height=1050)
wordcloudcategory(level2)
dev.off()


png(filename="level3.png",width=1200,height=1050)
wordcloudcategory(level3)
dev.off()


png(filename="level4.png",width=1200,height=1050)
wordcloudcategory(level4)
dev.off()


png(filename="level5.png",width=1200,height=1050)
wordcloudcategory(level5)
dev.off()


png(filename="level6.png",width=1200,height=1050)
wordcloudcategory(level6)
dev.off()

alltext=c(level1,level2,level3,level4,level5,level6)

  tweetCorpus<-Corpus(VectorSource(alltext))
  tweetTDM<-TermDocumentMatrix(tweetCorpus,control=list(removePunctuation=TRUE,
                                                        # stopwords=c(stopwords('english'),"amp"),
                                                        removeNumbers=TRUE,tolower=TRUE))
  tdMatrix <- as.matrix(tweetTDM) # creating a data matrix
  colnames(tdMatrix)<-levels(textdata$division) #adding column names
  
png(filename="comparative.png",width=1200,height=1050)
comparison.cloud(tdMatrix, random.order=FALSE, 
                                  colors = brewer.pal(8,"Dark2"),
                                  title.size=2,scale=c(5,1),rot.per=.25)
dev.off()
  

png(filename="commonality.png",width=1200,height=1050)
commonality.cloud(tdMatrix, random.order=FALSE, 
                                 colors = brewer.pal(8,"Dark2"),rot.per=.25,
                                 scale=c(5,1))
  
dev.off()

