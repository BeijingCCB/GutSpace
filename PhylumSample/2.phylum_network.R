library(pbapply)
library(parallel)
library(glmnet)
library(orthopolynom)
legendre_order=4
nt=30


df <- read.csv('PhylumSample.csv',row.names = 1)
df <- log(df+1)
exp_index <- as.numeric(colSums(df))


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
}
df_par <- t(apply(df,1,f3))
exp_index1 <- list(exp_index[1:4],exp_index[5:8],exp_index[9:10],
                   exp_index[11:14],exp_index[15],exp_index[16],exp_index[17:20],
                   exp_index[21],exp_index[22],exp_index[23])

add_time <- function(i){
  if ( length(exp_index[[i]])==1 ) {
    tmp <- seq((exp_index[[i]]-10),(exp_index[[i]]+10),length=nt)
  }
  else
    tmp <- seq(min(exp_index[[i]]),max(exp_index[[i]]),length=nt)
  return(tmp)
}
exp_index2 <- lapply(1:length(exp_index1), function(c)add_time(c))

#1.calculation---------------------------------------------------------------------------------------------------
get_legendre_par <- function(times,order) {
  get_interaction <- function(data,col){
    n <- nrow(data)
    clean_data <- data
    gene_list <- list()
    m <- clean_data[,col]
    M <- clean_data[,-col]
    x_matrix <- M
    x_matrix <- as.matrix(x_matrix)
    name <- colnames(clean_data)
    ridge1_cv <- cv.glmnet(x = x_matrix, y = m,type.measure = "mse", 
                           family="gaussian",nfold = 3,alpha = 0)
    best_ridge_coef <- abs(as.numeric(coef(ridge1_cv, s = ridge1_cv$lambda.min))[-1])
    
    fit_res <- cv.glmnet(x = x_matrix, y = m,type.measure = "mse", family="gaussian",
                         nfold = 3,alpha = 1,
                         penalty.factor = 1/best_ridge_coef,
                         keep = TRUE)
    best_alasso_coef1 <- coef(fit_res, s = fit_res$lambda.min)
    
    gene_list_one <- list()
    gene_list_one[[1]] <- name[col]
    gene_list_one[[2]] <- best_alasso_coef1@Dimnames[[1]][best_alasso_coef1@i[-1]+1]
    gene_list_one[[3]] <- best_alasso_coef1@x[-1]
    gene_list[[col]] <- gene_list_one
    
    return(gene_list_one)
  }
  power_equation <- function(par,x){
    y=par[1]*x^par[2]
    y
  }
  cluster_mean <- t(sapply(1:nrow(df_par),function(c)power_equation(as.numeric(df_par[c,]),times)))
  rownames(cluster_mean) <- rownames(df_par)
  
  module_relationship <- pblapply(1:nrow(df_par),function(c) get_interaction(t(cluster_mean),c))
  #----------------------
  get_effect <- function(pars,effect,times,order){
    if ( length(pars) != order ) {warning("legendre_pars != legendre_order")}
    LOP <-  legendre.polynomials(order, normalized=F) #Legendre polynomials
    d_LOP_fit <-  sapply(1:length(pars),function(c)
      pars[c]*polynomial.derivatives(LOP)[[c+1]])
    h <- scaleX(times,u=-1,v=1)[2]-scaleX(times,u=-1,v=1)[1] #per step h
    #rk4 for legendre with step=h
    LOP_rk4 <- function(x0,y0){
      f <- function(x,y){dy=do.call(sum,polynomial.values(polynomials=d_LOP_fit,x=x));dy}
      k1 <- f(x0,y0) 
      k2 <- f(x0+h/2,y0+h/2*k1)
      k3 <- f(x0+h/2,y0+h/2*k2)
      k4 <- f(x0+h,y0+h*k3)
      y <- y0+h/6*(k1+2*(1-1/sqrt(2))*k2+2*(1+1/sqrt(2))*k3+k4)
      return(y)
    }
    #dy_LOP, the increasment of each step
    dy <- sapply(1:length(times),function(c)LOP_rk4(scaleX(times,u=-1,v=1)[c],0))
    #dy_LOP*y= main or sub effect
    dy_fit <- effect*c(0,dy[1:(length(times)-1)])
    return(cumsum(dy_fit))
  }
  
  ode_optimize <- function(pars,ind,dep,times,data,order){
    ind_pars <- matrix(pars,ncol=order)[1,]
    dep_pars <- matrix(pars,ncol=order)[-1,]
    ind_effect <- get_effect(ind_pars,data[,ind],times,order)
    if ( is.null(nrow(dep_pars)) ) {
      dep_effect <- get_effect(dep_pars,data[,dep],times,order)
      y <- ind_effect+dep_effect+data[,ind][1]
    }else{
      dep_effect <- sapply(1:length(dep), function(c)
        get_effect(dep_pars[c,],data[,dep[c]],times,order))
      y <- ind_effect+rowSums(dep_effect)+data[,ind][2]
    }
    ssr <- sum((data[,ind]-y)^2)
    #add penalty
    alpha=1e-6
    ridge <- sum((data[,ind]-y)^2+alpha*(sum(ind_pars^2)+sum(dep_pars^2)))
    return(ridge)
  }
  
  get_value <- function(effect,data,times,order){
    #input
    ind <- data[[1]]
    dep <- data[[2]]
    ind_no <- as.numeric(which(colnames(effect)==ind))
    dep_no <- as.numeric(sapply(1:length(dep), function(c) which(colnames(effect)==dep[c])))
    init_pars <- rep(0.001,(length(ind_no)+length(dep_no))*order)
    result <- optim(init_pars,ode_optimize,ind=ind_no,dep=dep_no,
                    times=times,data=effect,order=order,
                    method = "BFGS", control=list(maxit=50,trace=T))
    par_after <- matrix(result$par,length(ind)+length(dep),order)
    return(par_after)
  }
  
  core.number <- detectCores()
  cl <- makeCluster(getOption("cl.cores", core.number))
  clusterEvalQ(cl, {library(orthopolynom)})
  clusterExport(cl, c("get_value","ode_optimize","get_effect","times","get_interaction",
                      "cluster_mean","module_relationship","order"),envir=environment())
  lop_par <- pblapply(1:nrow(df_par),function(c)get_value(t(cluster_mean),
                                                          module_relationship[[c]],times,order),cl=cl)
  stopCluster(cl)
  return(list(lop_par,module_relationship))
}

all_lop_par <- pblapply(1:length(exp_index2),function(c)
  get_legendre_par(exp_index2[[c]],order=legendre_order))

#2.output for result---------------------------------------------------------------------------------------------
get_output <- function(relationship,par,effect,times,order){
  get_effect <- function(pars,effect,times,order){
    if ( length(pars) != order ) {warning("legendre_pars != legendre_order")}
    LOP <-  legendre.polynomials(order, normalized=F) #Legendre polynomials
    d_LOP_fit <-  sapply(1:length(pars),function(c)
      pars[c]*polynomial.derivatives(LOP)[[c+1]])
    h <- scaleX(times,u=-1,v=1)[2]-scaleX(times,u=-1,v=1)[1] #per step h
    #rk4 for legendre with step=h
    LOP_rk4 <- function(x0,y0){
      f <- function(x,y){dy=do.call(sum,polynomial.values(polynomials=d_LOP_fit,x=x));dy}
      k1 <- f(x0,y0) 
      k2 <- f(x0+h/2,y0+h/2*k1)
      k3 <- f(x0+h/2,y0+h/2*k2)
      k4 <- f(x0+h,y0+h*k3)
      y <- y0+h/6*(k1+2*(1-1/sqrt(2))*k2+2*(1+1/sqrt(2))*k3+k4)
      return(y)
    }
    #dy_LOP, the increasment of each step
    dy <- sapply(1:length(times),function(c)LOP_rk4(scaleX(times,u=-1,v=1)[c],0))
    #dy_LOP*y= main or sub effect
    dy_fit <- effect*c(0,dy[1:(length(times)-1)])
    return(cumsum(dy_fit))
  }
  output <- list()
  output[[1]] <- relationship[[1]]  
  output[[2]] <- relationship[[2]]  
  output[[3]] <- par[1,]
  output[[4]] <- par[2:nrow(par),]
  ind_no <- as.numeric(which(colnames(effect)==output[[1]]))
  dep_no <- as.numeric(sapply(1:length(output[[2]]), 
                              function(c) which(colnames(effect)==output[[2]][c])))
  inital_value <- effect[,ind_no][1]/(length(ind_no)+length(dep_no))
  ind_effect <- get_effect(as.numeric(output[[3]]),effect[,ind_no],times,order)+inital_value
  if (length(dep_no)==1) {
    dep_effect <- get_effect(as.numeric(output[[4]]),effect[,dep_no],times,order)+inital_value
  }else{
    dep_effect <- sapply(1:length(dep_no), function(c)
      get_effect(as.numeric(output[[4]][c,]),effect[,dep_no[c]],times,order))+inital_value
    colnames(dep_effect) <- dep_no
  }
  #------------
  all_effect <- cbind(ind_effect,dep_effect)
  #effect_mean <- all_effect[5,]
  effect_mean <- apply(all_effect,2,mean)
  output[[5]] <- effect_mean
  output[[6]] <- all_effect
  return(output)
}

get_net <- function(i){
  times <- exp_index2[[i]]
  power_equation <- function(par,x){
    y=par[1]*x^par[2]
    y
  }
  cluster_mean <- t(sapply(1:nrow(df_par),function(c)power_equation(as.numeric(df_par[c,]),times)))
  rownames(cluster_mean) <- rownames(df_par)
  module_relationship <- all_lop_par[[i]][[2]]
  net <- pblapply(1:nrow(df_par),function(c)
    get_output(module_relationship[[c]],all_lop_par[[i]][[1]][[c]],
               t(cluster_mean),times=times,order=legendre_order))
  return(net)
}

all_net <- pblapply(1:length(exp_index2),function(c)get_net(c))

get_net_output <- function(j){
  get_after <- function(i){
    temp <- matrix(NA,nrow = length(i[[2]]),ncol=3)
    temp[,1] <- i[[2]]
    temp[,2] <- i[[1]]
    temp[,3] <- i[[5]][2:(length(i[[2]])+1)]
    colnames(temp) <- c('from','to','dep_effect')
    temp <- data.frame(temp)
    temp[,3] <- as.numeric(as.character(temp[,3]))
    return(temp)
  }
  links <- do.call(rbind,lapply(j, get_after))
  get_link_color <- function(i){
    tmp <- links$dep_effect[i]
    if (tmp >= 0 ) {
      tmp2 <- '+'
    } else {
      tmp2 <- '-'
    }
    return(tmp2)
  }
  links$effect_type <- sapply(1:nrow(links),function(c)get_link_color(c))
  
  get_ind <- function(i){
    temp <- i[[5]][1]
    return(temp)
  }
  nodes <- data.frame(unique(links[,2]),paste0('P',1:length(unique(links[,2]))),sapply(j,get_ind))
  colnames(nodes) <- c("id","name","received_effect")
  nodes$influence <- aggregate(dep_effect ~ to, data = links, sum)[,2]
  
  links$dep_effect <- abs(links$dep_effect)
  return(list(links,nodes))
}
result <- lapply(1:10,function(c)get_net_output(all_net[[c]]))

#output UC_Lumen net
write.csv(result[[1]][[1]],file='UC_Lumen1.csv')
write.csv(result[[1]][[2]],file='UC_Lumen2.csv')
