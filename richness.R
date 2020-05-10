# ============================================================
# Tutorial on drawing a richness plot using ggplot2
# by hafez ahmad
# =============================================================

abund_table<-read.csv("SPE_pitlatrine.csv",row.names=1,check.names=FALSE)
#Transpose the data to have sample names on rows
abund_table<-t(abund_table)
meta_table<-read.csv("ENV_pitlatrine.csv",row.names=1,check.names=FALSE)

#Filter out samples with fewer counts
abund_table<-abund_table[rowSums(abund_table)>200,]

#Extract the corresponding meta_table for the samples in abund_table
meta_table<-meta_table[rownames(abund_table),]


#Load vegan library
library(vegan)

# Calculate species richness
N <- rowSums(abund_table)
S <- specnumber(abund_table)
S.rar <-rarefy(abund_table, min(N))

# Regression of S.rar against meta_table
S.lm <- lm(S.rar ~ ., data = meta_table)
summary(S.lm)

#Format the data for ggplot

df_S<-NULL
for (i in 1:dim(meta_table)[2]){
  tmp<-data.frame(row.names=NULL,Sample=names(S.rar),Richness=S.rar,Env=meta_table[,i],Label=rep(colnames(meta_table)[i],length(meta_table[,i])))
  if (is.null(df_S)){
    df_S=tmp
  }else{
    df_S<-rbind(df_S,tmp)
  }
}

#Get grouping information
grouping_info<-data.frame(row.names=rownames(df_S),t(as.data.frame(strsplit(as.character(df_S[,"Sample"]),"_"))))
colnames(grouping_info)<-c("Countries","Latrine","Depth")

#Merge this information with df_S
df_S<-cbind(df_S,grouping_info)

# > head(df_S)
# Sample  Richness  Env Label Countries Latrine Depth
# 1  T_2_1  8.424693 7.82    pH         T       2     1
# 2 T_2_12  4.389534 8.84    pH         T       2    12
# 3  T_2_2 10.141087 6.49    pH         T       2     2
# 4  T_2_3 10.156289 6.46    pH         T       2     3
# 5  T_2_6 10.120478 7.69    pH         T       2     6
# 6  T_2_9 11.217727 7.60    pH         T       2     9


#We now make sure there are no factors in df_S
df_S$Label<-as.character(df_S$Label)
df_S$Countries<-as.character(df_S$Countries)
df_S$Latrine<-as.character(df_S$Latrine)
df_S$Depth<-as.character(df_S$Depth)

#We need a Pvalue formatter
formatPvalues <- function(pvalue) {
  ra<-""
  if(pvalue <= 0.1) ra<-"."
  if(pvalue <= 0.05) ra<-"*"
  if(pvalue <= 0.01) ra<-"**"
  if(pvalue <= 0.001) ra<-"***"
  return(ra)
}


#Now use ddply to get correlations
library(plyr)
cors<-ddply(df_S,.(Label), summarise, cor=round(cor(Richness,Env),2), sig=formatPvalues(cor.test(Richness,Env)$p.value))

# > head(cors)
# Label   cor sig
# 1      Carbo -0.59 ***
# 2       CODs -0.46 ***
# 3       CODt -0.08    
# 4        NH4 -0.43 ***
# 5 perCODsbyt -0.60 ***
# 6         pH  0.05

#We have to do a trick to assign labels as facet_wrap doesn't have an explicit labeller
#Reference 1: http://stackoverflow.com/questions/19282897/how-to-add-expressions-to-labels-in-facet-wrap
#Reference 2: http://stackoverflow.com/questions/11979017/changing-facet-label-to-math-formula-in-ggplot2
facet_wrap_labeller <- function(gg.plot,labels=NULL) {
  #works with R 3.0.1 and ggplot2 0.9.3.1
  require(gridExtra)
  
  g <- ggplotGrob(gg.plot)
  gg <- g$grobs      
  strips <- grep("strip_t", names(gg))
  
  for(ii in seq_along(labels))  {
    modgrob <- getGrob(gg[[strips[ii]]], "strip.text", 
                       grep=TRUE, global=TRUE)
    gg[[strips[ii]]]$children[[modgrob$name]] <- editGrob(modgrob,label=labels[ii])
  }
  
  g$grobs <- gg
  class(g) = c("arrange", "ggplot",class(g)) 
  g
}

#Now convert your labels
wrap_labels<-do.call(paste,c(cors[c("Label")],"(",cors[c("cor")]," ",cors[c("sig")],")",sep=""))


p<-ggplot(df_S,aes(Env,Richness)) + 
  geom_point(aes(colour=Countries)) +
  geom_smooth(,method="lm", size=1, se=T) +
  facet_wrap( ~ Label , scales="free", ncol=3) +theme_bw() 
p<-facet_wrap_labeller(p,labels=wrap_labels)
pdf("Richness.pdf",width=14,height=14)
print(p)
dev.off()