get_ind_mean <- function(i){
  tmp <- all_net[[i]]
  get_ind <- function(i){
    tmp1 <- tmp[[i]][[5]]
    return(as.numeric(tmp1[1]))
  }
  return(mean(abs(sapply(1:length(tmp),function(c)get_ind(c)))))
}

get_up_effect <- function(i){
  tmp <- all_net[[i]]
  get_ind <- function(i){
    tmp1 <- tmp[[i]][[6]]
    return(mean(tmp1[tmp1>=0]))
  }
  n <- nrow(df_par)
  return(sum(na.omit(sapply(1:length(tmp),function(c)get_ind(c))))/n)
}

get_down_effect <- function(i){
  tmp <- all_net[[i]]
  get_ind <- function(i){
    tmp1 <- tmp[[i]][[6]]
    return(mean(tmp1[tmp1<0]))
  }
  n <- nrow(df_par)
  return(sum(na.omit(sapply(1:length(tmp),function(c)get_ind(c))))/n)
}

darken <- function(color, factor=1.4){
  col <- col2rgb(color)
  col <- col/factor
  col <- rgb(t(col), maxColorValue=255)
  col
}

df3 <- data.frame(matrix(NA,10,4))
colnames(df3) <- c('name','ind','up','down')
df3$name <- c('Lumen','Caecum','Ileum','Transverse Colon','Descending Colon',
              'Sigmoid Colon','Rectum','HC_Caecum','HC_Ileum','HC_Transverse Colon')
df3$ind <- sapply(1:10,function(c)get_ind_mean(c))
df3$up <- sapply(1:10,function(c)get_up_effect(c))
df3$down <- sapply(1:10,function(c)get_down_effect(c))

df3$name  <- factor(df3$name,levels=c("Lumen","Caecum","Ileum","Transverse Colon",
                                      "Descending Colon","Sigmoid Colon","Rectum"))

df4 <- df3[8:10,]
df4$name <- c('Caecum','Ileum','Transverse Colon')
df4$name  <- factor(df4$name,levels=c("Caecum","Ileum","Transverse Colon"))
df3 <- df3[1:7,]
df3 <- df3[order(df3$name),]
df4 <- df4[order(df4$name),]

b=0.3;a=0.1
k=(b-a)/(max(df3$ind)-min(df3$ind))

d1up=data.frame(x1=c(1:7-k*(df3$ind-min(df3$ind))-0.05), 
                x2=c(1:7+k*(df3$ind-min(df3$ind))+0.05),
                y1=c(df3$ind), 
                y2=c(df3$ind+df3$up))
d1down=data.frame(x1=c(1:7-k*(df3$ind-min(df3$ind))-0.05), 
                  x2=c(1:7+k*(df3$ind-min(df3$ind))+0.05),
                  y1=c(df3$ind), 
                  y2=c(df3$ind+df3$down))
k=(b-a)/(max(df4$ind)-min(df4$ind))
d2up <- data.frame(x1=c(c(2,3,4)-k*(df4$ind-min(df4$ind)-0.05)), 
                   x2=c(c(2,3,4)+k*(df4$ind-min(df4$ind))+0.05),
                   y1=c(df4$ind), 
                   y2=c(df4$ind+df4$up))
d2down <- data.frame(x1=c(c(2,3,4)-k*(df4$ind-min(df4$ind)-0.05)), 
                     x2=c(c(2,3,4)+k*(df4$ind-min(df4$ind))+0.05),
                     y1=c(df4$ind), 
                     y2=c(df4$ind+df4$down))
