library(stringr)
library(reshape2)
df=read.csv("2013-2014 players.csv")
colnames(df)[1]="Player"
df=df[-1,]
list=str_split(df$Player,"\\(")
df1=as.data.frame(list)
df1=df1[-2,]
df1$dummycol=1
df1=melt(df1,id="dummycol")
df$Player=df1$value
head(df)
