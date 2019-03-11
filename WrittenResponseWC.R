#Create word clouds for all written response questions which are stored in seperate csv's
library("NLP")
library("tm")
library("RColorBrewer")
library("wordcloud")

setwd(-------------------)

df=read.csv("**NAME***.csv",header=TRUE)

corpus=Corpus(VectorSource(df[,1]))
corpus[[1]][1]


corpus=tm_map(corpus,content_transformer(tolower))
corpus=tm_map(corpus,removeNumbers)
corpus=tm_map(corpus,removeWords,stopwords("english"))
corpus=tm_map(corpus,removePunctuation)
corpus=tm_map(corpus,stripWhitespace)
corpus=tm_map(corpus,removeWords,c("can","job","lot","also","nice","say","just","edc","like","feel","one","especially","james","perhaps","senior",
                                   "wide","job","well","will","even","coo","door"))
corpus=tm_map(corpus,removeWords,c("get","often","way","many","done","needs","good","see","feels","much","sure","still","departments","department","take",
                                   "stay","long","continue","great","presidents","within","put","want","successful","due","need","realnew",
                                   "etc","true","know","big","let","first"))
corpus[[1]][1]

tdm=TermDocumentMatrix(corpus)
m=as.matrix(tdm)
v=sort(rowSums(m),decreasing=T)
d=data.frame(word=names(v),freq=v)

wordcloud(words = d$word, freq = d$freq, random.order = F,rot.per=0.35,scale=c(4.2,.5),max.words = 200, colors=brewer.pal(8, "Dark2"))
