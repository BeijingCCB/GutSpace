library(tidyverse)
library(reshape2)
library(patchwork)
library(investr)
library(scales)
df <- read.csv('SpeciesSample.csv',row.names = 1)
df <- log(df+1)
exp_index <- as.numeric(colSums(df))
#df1 <- df[,1:20]
#df2 <- df[,21：23]
mean((exp_index))

get_p0 <- function(i){
  get_i_data <- function(i){
    df2 <- t(rbind(df[i,],as.numeric(colSums(df))))%>% data.frame
    df3 <- melt(df2,id.vars=c("X2"))
    df3[,2] <- c(rep("UC",20),rep("HC",3))
    colnames(df3)[2] <- c("Condition")
    return(df3)
  }
  df1 <- get_i_data(i)
  
  p1 <- ggplot() + 
    geom_point(df1,mapping=aes(x=as.numeric(X2),y=as.numeric(value),shape=Condition,color=Condition),size=4)+ 
    scale_shape_manual(values = c(1,2,3))+scale_color_manual(values=c("#00B0F6", "#F8766D", "#39B600"))+
    geom_smooth(df1,mapping = aes(x=as.numeric(X2),y=as.numeric(value)),
                method = "nls",color="#ab00d4", size=1,level = 0.95, se = FALSE, formula = y ~ a*x^b,
                method.args = list(start = list(a=0.25, b=-0.1),
                                   control = nls.control(maxiter = 100000,minFactor = 1e-100))) +
    theme_bw()+theme(panel.grid =element_blank())+
    scale_x_log10(breaks = trans_breaks(n=3,"log10", function(x) 10^x), 
                  labels = trans_format("log10", scales::math_format(10^.x)))+
    scale_y_continuous(limits = c(0,14),breaks=c(0,log(10^2),log(10^4),log(10^6)),
                       labels = expression(0,10^2,10^4,10^6))+
    annotate('text',x=160,y=14,label=rownames(df)[i],size=5)+xlab("")+ylab("")+ guides(fill=FALSE)+
    theme(plot.margin = unit(c(0,0,0,0),"lines"))+theme(axis.title.x=element_blank(),
                                                        axis.text.x=element_blank(),
                                                        axis.ticks.length.x = unit(-0.1,"cm"))
  return(p1)
}
get_p0(1)
