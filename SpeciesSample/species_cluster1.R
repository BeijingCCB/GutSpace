#! /path/to/Rscript
args <- commandArgs(T)
k = as.numeric(args[1])

df <- read.csv("SpeciesSample.csv",row.names = 1)
df <- log(df+1)

get_init_par <- function(data,k){
  f1 <- function(y){
    power_equation <- function(par,x){
      y=par[1]*x^par[2]
      y
    }
    tmp <- c(0.1,0.1)
    par_est <- function(par,x,y){
      sum( (y - power_equation(par,x))^2 )
    }
    r1 <- optim(tmp,par_est,x=as.numeric(colSums(df)),y=as.numeric(y), method = "Nelder-Mead")
    return(r1$par)
  }
  f2 <- function(y){
    x <-as.numeric(colSums(df));y <- as.numeric(y)
    lm_mod <- lm(log(y)~log(x))
    a = exp(lm_mod$coefficients[1])
    b = lm_mod$coefficients[2]
    return(c(a,b))
  }
  f3 <- function(y){
    x <- as.numeric(colSums(df))
    y <- as.numeric(y)
    model <- try(nls(y~a*x^b,start = list(a =0.3, b = 0.1),
                     control = nls.control(maxiter = 100000,minFactor = 1e-200)))
    if ('try-error' %in% class(model)) {
      model <- try(nls(y~a*x^b,start = list(a =3, b = 0.1),
                       control = nls.control(maxiter = 100000,minFactor = 1e-200)))
    }
    if ('try-error' %in% class(model)) {
      model <- try(nls(y~a*x^b,start = list(a =0.1, b = 0.1),
                       control = nls.control(maxiter = 100000,minFactor = 1e-200)))
    }
    if ('try-error' %in% class(model)) {
      model <- try(nls(y~a*x^b,start = list(a = -0.1, b = 0.1),
                       control = nls.control(maxiter = 100000,minFactor = 1e-200)))
    }
    if ('try-error' %in% class(model)) {
      model <- try(nls(y~a*x^b,start = list(a = 0.8, b = -0.1),
                       control = nls.control(maxiter = 100000,minFactor = 1e-200)))
    }
    if ('try-error' %in% class(model)) {
      model <- try(nls(y~a*x^b,start = list(a =-0.1, b = -0.1),
                       control = nls.control(maxiter = 1000000,minFactor = 1e-200)))
    }
    if ('try-error' %in% class(model)) {
      result <- c(NA,NA)
    }
    else{
      result <- coef(model)
    }
    return(result)
  } 
  init_cluster <- kmeans(data,centers = k,iter.max = 100)
  pro <- table(init_cluster$cluster)/nrow(data) 
  
  cuM <- init_cluster$centers
  cusd <- diag(cov(df))
  
  init_curve_para <-  t(apply(cuM, 1, f3))
  init_sd_para <- c(mean(cusd),0.4) #SAD1
  init_pro <- pro
  
  return_object <- list(init_sd_para,init_curve_para,init_pro)
  names(return_object)<-c("init_sd_par","init_curve","init_pro")
  return(return_object)
}

get_cluster <- function(data,k,input){
  requiredPackages = c("mvtnorm","reshape2","ggplot2")
  for(packages in requiredPackages){
    if(!require(packages,character.only = TRUE)) install.packages(packages)
    require(packages,character.only = TRUE)
  }
  Delta <- 10000; iter <- 0; itermax <- 100;
  SAD1_get_matrix <- function(par,data){
    p <-  ncol(data)
    v2 <- par[1]
    phi <- par[2]
    tmp <- (1-phi^2)
    sigma <- array(dim=c(p,p))
    for(i in 1:p){
      sigma[i,i:p] <- phi^( c(i:p) - i ) * (1-phi^(2*i ))/tmp
      sigma[i:p,i] <- sigma[i,i:p]}
    sigma <- sigma*abs(v2)
    return(sigma)
  } 
  AR1_get_matrix <- function(par,data){
    n <- ncol(data)
    rho <- par[2]
    exponent <- abs(matrix(1:n - 1, nrow = n, ncol = n, byrow = TRUE) - 
                      (1:n - 1))
    return(par[1]*rho^exponent)
  }
  power_equation2 <- function(par,x){
    a=par[1]; b=par[2]; x <- as.numeric(x)
    return(y = a*x^b)
  }
  mle <- function(par,data,prob){
    par1 <- par[1:2]
    par2 <- matrix(par[-c(1:2)],nrow = k,ncol = 2)
    temp_S <- sapply(1:k, function(c) dmvnorm(data,
                                              power_equation2(par2[c,],colSums(data)),
                                              SAD1_get_matrix(par1,data))*prob[c] )
    LL <- sum(-log(rowSums(temp_S)))
    return(LL)
  }
  while ( Delta > 0.1 && iter <= itermax ) {
    # initiation
    if(iter == 0){
      init_sd_para <- input[[1]]
      init_curve_para <- input[[2]]
      pro <- input[[3]]
    }
    #E step, calculate the posterior probability
    old_par <- c(init_sd_para,init_curve_para)
    LL_mem <- mle(old_par,data,pro)
    mvn.c <- sapply(1:k, function(c) dmvnorm(data,
                                             power_equation2(init_curve_para[c,],colSums(data)),
                                             SAD1_get_matrix(init_sd_para,data))*pro[c] )
    omega <- mvn.c/rowSums(mvn.c)
    #M step, calculate parameters
    pro <- colSums(omega)/sum(omega)
    new_par <- try(optim(old_par, mle, data=data, prob=pro, method = "BFGS"))
    if ('try-error' %in% class(new_par))
      break
    L_Value <- new_par$value
    init_sd_para <- new_par$par[1:2]
    init_curve_para <- matrix(new_par$par[-c(1:2)],nrow = k)
    Delta <- abs(L_Value-LL_mem)
    if (Delta > 500)
      break
    cat('\n',"iter=",iter,"LL=",L_Value,'\n')
    iter <- iter+1; LL_mem <- L_Value
  } 
  
  BIC <- 2*(L_Value)+log(nrow(data))*length(old_par)
  #plot-----------
  cluster <- apply(omega,1,which.max)
  clustered_df <- cbind(row.names(data),data,cluster)
  colnames(clustered_df) <- c("row.names(data)",as.numeric(colSums(df)),"cluster")
  long_df <- melt(clustered_df,id.vars=c("row.names(data)","cluster"))
  colnames(long_df) <- c("gene","cluster","time","fpkm")
  p <-  ggplot()+geom_line(long_df,mapping=aes(as.numeric(as.character(time)),fpkm,group=gene,
                                               colour= as.character(long_df$cluster)))+
    facet_wrap(long_df$cluster,scales = "fixed")+ 
    theme(legend.position="none") + xlab("Total_fpkm")+ylab("individual_fpkm")
  
  clustered_df <- clustered_df[,-1]
  return_object <- list(init_sd_para,init_curve_para,pro,LL_mem,BIC,clustered_df,p)
  names(return_object)<-c("sd_par", "curve_par", "pro", "LL", "BIC", "clustered_data","plot")
  return(return_object)
}

get_BIC <- function(rep,k){
  requiredPackages = c("pbapply","parallel")
  for(packages in requiredPackages){
    if(!require(packages,character.only = TRUE)) install.packages(packages)
    require(packages,character.only = TRUE)
  }
  core.number <- detectCores()
  cl <- makeCluster(getOption("cl.cores", core.number))
  clusterExport(cl, c("df","get_init_par","get_cluster"),envir = environment())
  
  input <- lapply(1:rep, function(c) get_init_par(df,k) )  
  output <- pblapply(1:rep, function(c) get_cluster(df,k,input[[c]]),cl=cl)
  
  out_df <- matrix(NA,nrow = 3,ncol = rep)
  out_df[1,] <- sapply(1:rep,function(c)length(table(output[[c]]$clustered_data[,(length(df)+1)])))
  out_df[2,] <- sapply(1:rep,function(c)output[[c]]$BIC)
  out_df[3,] <- sapply(1:rep,function(c)output[[c]]$LL)
  rownames(out_df) <- c('rep','BIC','LL')
  return(list(t(data.frame(out_df)),output))
}

result <- get_BIC(80,7)
save.image(paste0('k','.Rdata'))
