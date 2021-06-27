load('species_cluster.Rdata')
library(pbapply)
library(parallel)
library(ggplot2)
library(glmnet)
library(orthopolynom)
library(igraph)

get_module_net <- function(){
  legendre_order=4
  nt=30
  m=9
  #0.input---------------------------------------------------------------------------------------------------------
  df <- result[[2]][[m]]$clustered_data[,-24]
  colnames(df) <- NULL
  exp_index <- as.numeric(colSums(df))
  
  df_par <- result[[2]][[m]]$curve_par
  rownames(df_par) <- 1:nrow(df_par)
  
  exp_index1 <- list(exp_index[1:20],exp_index[21:23])
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
      #vec <- sapply(1:length(M[1,]),function(c)cor(m,M[,c]))
      #x_matrix <- M[,which( vec %in% -sort(-vec)[1:(n/log(n))] )]
      #x_matrix <- as.matrix(x_matrix)
      name <- colnames(clean_data)
      ridge1_cv <- cv.glmnet(x = x_matrix, y = m,type.measure = "mse", 
                             family="gaussian",nfold = 3,alpha = 0)
      best_ridge_coef <- abs(as.numeric(coef(ridge1_cv, s = ridge1_cv$lambda.min))[-1])
      get_coef <- function(input){
        if (input==0) {
          a <- 0
        }
        else{
          a <- log(1/input)
        }
        a
      }
      coef <- sapply(1:length(best_ridge_coef),function(c)get_coef(best_ridge_coef[c]))
      fit_res <- cv.glmnet(x = x_matrix, y = m,type.measure = "mse", family="gaussian",
                           nfold = 10,alpha = 1,
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
        y <- ind_effect+rowSums(dep_effect)+data[,ind][1]
      }
      ssr <- sum((data[,ind]-y)^2)
      #add penalty
      alpha=0.1
      ridge <- sum((data[,ind]-y)^2+alpha*(sum(ind_pars^2)+sum(dep_pars^2)))
      return(ssr)
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
  
  all_lop_par <- pblapply(1:length(exp_index1),function(c)
    get_legendre_par(exp_index1[[c]],order=legendre_order))
  
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
    times <- seq(min(exp_index1[[i]]),max(exp_index1[[i]]),length=nt)
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
  
  all_net <- pblapply(1:length(exp_index1),function(c)get_net(c))
  
  #for position
  get_uc_position <- function(i){
    tmp <- all_net[[1]][[i]]
    uc_position <- list(c(1:4),c(5:8),c(9:10),c(11:14),c(15),c(16),c(17:20))
    get_position <- function(j){
      tmp2 <- tmp
      if ( length(uc_position[[j]])==1 )  {
        tmp2[[5]] <- tmp[[6]][uc_position[[j]],]
      }else{
        tmp2[[5]] <- colMeans(tmp[[6]][uc_position[[j]],])}
      return(tmp2)
    }
    temp <- lapply(1:length(uc_position), function(c)get_position(c))
    return(temp)
  }
  uc_all_net <- lapply(1:7,function(c)get_uc_position(c))
  
  uc_all_net2 <- list(list(),list(),list(),list(),list(),list(),list())
  for (i in 1:7) {
    for (j in 1:7) {
      uc_all_net2[[i]][[j]] <- uc_all_net[[j]][[i]]
    }
  }
  
  get_hc_position <- function(i){
    tmp <- all_net[[2]][[i]]
    hc_position <- list(c(1),c(2),c(3))
    get_position <- function(j){
      tmp2 <- tmp
      if ( length(hc_position[[j]])==1 )  {
        tmp2[[5]] <- tmp[[6]][hc_position[[j]],]
      }else{
        tmp2[[5]] <- colMeans(tmp[[6]][hc_position[[j]],])}
      return(tmp2)
    }
    temp <- lapply(1:length(hc_position), function(c)get_position(c))
    return(temp)
  }
  hc_all_net <- lapply(1:7,function(c)get_uc_position(c))
  
  hc_all_net2 <- list(list(),list(),list())
  for (i in 1:3) {
    for (j in 1:7) {
      hc_all_net2[[i]][[j]] <- hc_all_net[[j]][[i]]
    }
  }
  
  all_net2 <- c(uc_all_net2,hc_all_net2)
  
  return(all_net2)
}

module_net <- get_module_net()

get_sub_net <- function(cluster){
  legendre_order=4
  nt=30
  m=9
  #0.input---------------------------------------------------------------------------------------------------------
  df <- result[[2]][[m]]$clustered_data
  df <- df[df$cluster==cluster,-24]
  colnames(df) <- NULL
  exp_index <- as.numeric(colSums(df[,-24]))
  
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
  exp_index1 <- list(seq(min(exp_index[1:20]),max(exp_index[1:20]),length=nt),
                     seq(min(exp_index[21:23]),max(exp_index[21:23],length=nt)))
  
  exp_index1 <- list(exp_index[1:20],exp_index[21:23])
  
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
      #vec <- sapply(1:length(M[1,]),function(c)cor(m,M[,c]))
      #x_matrix <- M[,which( vec %in% -sort(-vec)[1:(n/log(n))] )]
      #x_matrix <- as.matrix(x_matrix)
      name <- colnames(clean_data)
      ridge1_cv <- cv.glmnet(x = x_matrix, y = m,type.measure = "mse", 
                             family="gaussian",nfold = 3,alpha = 0)
      best_ridge_coef <- abs(as.numeric(coef(ridge1_cv, s = ridge1_cv$lambda.min))[-1])
      get_coef <- function(input){
        if (input==0) {
          a <- 0
        }
        else{
          a <- log(1/input)
        }
        a
      }
      coef <- sapply(1:length(best_ridge_coef),function(c)get_coef(best_ridge_coef[c]))
      fit_res <- cv.glmnet(x = x_matrix, y = m,type.measure = "mse", family="gaussian",
                           nfold = 10,alpha = 1,
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
    if ( nrow(df) == 2 ) {
      module_relationship <- list(list(),list())
      module_relationship[[1]][[1]] <- rownames(df)[1]
      module_relationship[[1]][[2]] <- rownames(df)[2]
      module_relationship[[1]][[3]] <- 1
      module_relationship[[2]][[1]] <- rownames(df)[2]
      module_relationship[[2]][[2]] <- rownames(df)[1]
      module_relationship[[2]][[3]] <- 1
    }else{
      module_relationship <- pblapply(1:nrow(df_par),function(c) get_interaction(t(cluster_mean),c))}
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
        y <- ind_effect+rowSums(dep_effect)+data[,ind][1]
      }
      ssr <- sum((data[,ind]-y)^2)
      #add penalty
      alpha=0.1
      ridge <- sum((data[,ind]-y)^2+alpha*(sum(ind_pars^2)+sum(dep_pars^2)))
      return(ssr)
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
  
  all_lop_par <- pblapply(1:length(exp_index1),function(c)
    get_legendre_par(exp_index1[[c]],order=legendre_order))
  
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
    times <- seq(min(exp_index1[[i]]),max(exp_index1[[i]]),length=nt)
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
  
  all_net <- pblapply(1:length(exp_index1),function(c)get_net(c))
  
  #for position
  get_uc_position <- function(i){
    tmp <- all_net[[1]][[i]]
    uc_position <- list(c(1:4),c(5:8),c(9:10),c(11:14),c(15),c(16),c(17:20))
    get_position <- function(j){
      tmp2 <- tmp
      if ( length(uc_position[[j]])==1 )  {
        tmp2[[5]] <- tmp[[6]][uc_position[[j]],]
      }else{
        tmp2[[5]] <- colMeans(tmp[[6]][uc_position[[j]],])}
      return(tmp2)
    }
    temp <- lapply(1:length(uc_position), function(c)get_position(c))
    return(temp)
  }
  uc_all_net <- lapply(1:nrow(df_par),function(c)get_uc_position(c))
  
  uc_all_net2 <- list(list(),list(),list(),list(),list(),list(),list())
  for (i in 1:7) {
    for (j in 1:nrow(df_par)) {
      uc_all_net2[[i]][[j]] <- uc_all_net[[j]][[i]]
    }
  }
  
  get_hc_position <- function(i){
    tmp <- all_net[[2]][[i]]
    hc_position <- list(c(1),c(2),c(3))
    get_position <- function(j){
      tmp2 <- tmp
      if ( length(hc_position[[j]])==1 )  {
        tmp2[[5]] <- tmp[[6]][hc_position[[j]],]
      }else{
        tmp2[[5]] <- colMeans(tmp[[6]][hc_position[[j]],])}
      return(tmp2)
    }
    temp <- lapply(1:length(hc_position), function(c)get_position(c))
    return(temp)
  }
  hc_all_net <- lapply(1:nrow(df_par),function(c)get_uc_position(c))
  
  hc_all_net2 <- list(list(),list(),list())
  for (i in 1:3) {
    for (j in 1:nrow(df_par)) {
      hc_all_net2[[i]][[j]] <- hc_all_net[[j]][[i]]
    }
  }
  
  all_net2 <- c(uc_all_net2,hc_all_net2)
  return(all_net2)
}

sub_net <- lapply(1:7,function(c)get_sub_net(c))

#------------------------plot--------------------------------
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

get_max_effect <- function(k){
  after <- do.call(rbind,lapply(k, get_after))
  max_dep_effect <- max(abs(after$dep_effect))
  
  temp <- aggregate(dep_effect ~ to, data = after, sum)
  all_dep_effect <- max(abs(temp$dep_effect))
  return(c(max_dep_effect,all_dep_effect))
}

get_all_uc_net <- function(){
  tmp1 <- module_net[1:7]
  tmp2 <- sapply(1:7,function(c)sub_net[c][1:7][[1]][1:7])
  tmp <- c(tmp1,tmp2)
  return(tmp)
}
all_net <- get_all_uc_net()

get_all_hc_net <- function(){
  tmp1 <- module_net[8:10]
  tmp2 <- sapply(1:7,function(c)sub_net[c][1:7][[1]][8:10])
  tmp <- c(tmp1,tmp2)
  return(tmp)
}
all_net2 <- get_all_hc_net()

  
max_effect <- t(sapply(1:length(all_net),function(c)get_max_effect(all_net[[c]])))
max_effect2 <- sapply(1:8,function(c)rep(list(max_effect[seq(1,56,7)[c]:seq(7,56,7)[c],]),7))

max_effect_hc <- t(sapply(1:length(all_net2),function(c)get_max_effect(all_net2[[c]])))
max_effect_hc2 <-  sapply(1:8,function(c)rep(list(max_effect_hc[seq(1,24,3)[c]:seq(3,24,3)[c],]),3))

network_plot1 <- function(k,effect){
  get_extra <- function(i){
    temp <- i[[5]][1]
    return(temp)
  }
  extra <- sapply(k,get_extra)
  
  after <- do.call(rbind,lapply(k, get_after))
  
  colfunc <- colorRampPalette(c("#619CFF", #ggplot blue
                                "#ffdead", 
                                "#F8766D"))#ggplot red
  #unchange-weight-data
  #links
  #colour_edge
  edge_number <- round(max(effect[,1]))
  edge_col <- data.frame(colfunc(2*edge_number+1),seq(-edge_number,edge_number,1))
  
  get_edge_colour <- function(i){
    temp <-  round(i)
    temp2 <- adjustcolor(edge_col[which(edge_col[,2]==temp),1], alpha=0.5)
    return(temp2)
  }
  links <- after
  colnames(links) <- c("from","to","weight")
  
  get_link_color <- function(i){
    tmp <- links$weight[i]
    if (tmp >= 0 ) {
      tmp2 <- adjustcolor("#F8766D", alpha=1)
    } else {
      tmp2 <- adjustcolor("#619CFF", alpha=1)
    }
    return(tmp2)
  }
  
  links$edge.colour <- sapply(1:nrow(links),function(c)get_link_color(c))
  
  links <- after
  colnames(links) <- c("from","to","weight")
  links$edge.colour <- pbsapply(links$weight,get_edge_colour) #add colour for links
  #nodes
  node_number <- round(max(effect[,2]))
  node_col <- data.frame(colfunc(2*node_number+1),seq(-node_number,node_number,1))
  get_vertex_colour <- function(i){
    temp <-  round(i)
    temp2 <- adjustcolor(node_col[which(node_col[,2]==temp),1], alpha=1)
    return(temp2)
  }
  
  nodes <- data.frame(unique(links[,2]),paste0('M',1:7),extra)
  colnames(nodes) <- c("id","name","ind_effect")
  nodes <- nodes[order(nodes[,1]),]
  nodes$influence <- aggregate(dep_effect ~ to, data = after, sum)[,2]
  nodes$colour <- pbsapply(nodes$influence,get_vertex_colour) #add colour for links
  
  #normalization
  normalization <- function(x){(x-min(x))/(max(x)-min(x))*1.5+1} 
  #normalization2
  normalization2 <- function(x){log(abs(x)+1)+1}
  
  #final plot
  links[,3] <- normalization2(abs(links[,3]))
  nodes[,3:4] <- normalization2(nodes[,3:4])
  net <- graph_from_data_frame( d=links,vertices = nodes,directed = T ) 
  
  #layout
  set.seed(1)
  l <- layout_in_circle(net)
  
  plot.igraph(net,
               vertex.label=V(net)$name,
               vertex.label.color="black",
               vertex.shape="circle", 
               vertex.label.cex=V(net)$ind_effect,
               vertex.size=V(net)$ind_effect*18,
               edge.curved=0.05,
               edge.color=E(net)$edge.colour,
               edge.frame.color=E(net)$edge.colour,
               edge.width=E(net)$weight+1,
               vertex.color=V(net)$colour,
               layout=l,
               margin=c(-.15,-.15,-.15,-.15)
  )
  box("figure")
}

network_plot2 <- function(k,effect){
  get_extra <- function(i){
    temp <- i[[5]][1]
    return(temp)
  }
  extra <- sapply(k,get_extra)
  
  after <- do.call(rbind,lapply(k, get_after))
  
  colfunc <- colorRampPalette(c("#619CFF", #ggplot blue
                                "#ffdead", 
                                "#F8766D"))#ggplot red
  #unchange-weight-data
  #links
  #colour_edge
  edge_number <- round(max(effect[,1]))
  edge_col <- data.frame(colfunc(2*edge_number+1),seq(-edge_number,edge_number,1))
  
  get_edge_colour <- function(i){
    temp <-  round(i)
    temp2 <- adjustcolor(edge_col[which(edge_col[,2]==temp),1], alpha=0.5)
    return(temp2)
  }
  links <- after
  colnames(links) <- c("from","to","weight")
  
  get_link_color <- function(i){
    tmp <- links$weight[i]
    if (tmp >= 0 ) {
      tmp2 <- adjustcolor("#F8766D", alpha=1)
    } else {
      tmp2 <- adjustcolor("#619CFF", alpha=1)
    }
    return(tmp2)
  }
  i=1
  links$edge.colour <- sapply(1:nrow(links),function(c)get_link_color(c))
  
  links <- after
  colnames(links) <- c("from","to","weight")
  links$edge.colour <- pbsapply(links$weight,get_edge_colour) #add colour for links
  #nodes
  node_number <- round(max(effect[,2]))
  node_col <- data.frame(colfunc(2*node_number+1),seq(-node_number,node_number,1))
  get_vertex_colour <- function(i){
    temp <-  round(i)
    temp2 <- adjustcolor(node_col[which(node_col[,2]==temp),1], alpha=1)
    return(temp2)
  }
  
  nodes <- data.frame(unique(links[,2]),unique(links[,2]),extra)
  colnames(nodes) <- c("id","name","ind_effect")
  nodes <- nodes[order(nodes[,1]),]
  nodes$influence <- aggregate(dep_effect ~ to, data = after, sum)[,2]
  nodes$colour <- pbsapply(nodes$influence,get_vertex_colour) #add colour for links
  
  #normalization
  normalization <- function(x){(x-min(x))/(max(x)-min(x))*1.5+1} 
  #normalization2
  normalization2 <- function(x){log(abs(x)+1)+1}
  
  #final plot
  links[,3] <- normalization2(abs(links[,3]))
  nodes[,3:4] <- normalization2(nodes[,3:4])
  net <- graph_from_data_frame( d=links,vertices = nodes,directed = T ) 
  
  #layout
  set.seed(1)
  l <- layout_in_circle(net)
  
  plot.igraph(net,
               vertex.label=V(net)$name,
               vertex.label.color="black",
               vertex.shape="circle", 
               vertex.label.cex=V(net)$ind_effect*0.8,
               vertex.size=V(net)$ind_effect*18,
               edge.curved=0.05,
               edge.color=E(net)$edge.colour,
               edge.frame.color=E(net)$edge.colour,
               edge.width=E(net)$weight+1,
               vertex.color=V(net)$colour,
               layout=l,
               margin=c(-.25,-.25,-.25,-.25)
  )
  box("figure")
}

network_plot3 <- function(k,effect){
  get_extra <- function(i){
    temp <- i[[5]][1]
    return(temp)
  }
  extra <- sapply(k,get_extra)
  
  after <- do.call(rbind,lapply(k, get_after))
  
  colfunc <- colorRampPalette(c("#619CFF", #ggplot blue
                                "#ffdead", 
                                "#F8766D"))#ggplot red
  #unchange-weight-data
  #links
  #colour_edge
  edge_number <- round(max(effect[,1]))
  edge_col <- data.frame(colfunc(2*edge_number+1),seq(-edge_number,edge_number,1))
  
  get_edge_colour <- function(i){
    temp <-  round(i)
    temp2 <- adjustcolor(edge_col[which(edge_col[,2]==temp),1], alpha=0.5)
    return(temp2)
  }
  links <- after
  colnames(links) <- c("from","to","weight")
  
  get_link_color <- function(i){
    tmp <- links$weight[i]
    if (tmp >= 0 ) {
      tmp2 <- adjustcolor("#F8766D", alpha=1)
    } else {
      tmp2 <- adjustcolor("#619CFF", alpha=1)
    }
    return(tmp2)
  }
  
  links$edge.colour <- sapply(1:nrow(links),function(c)get_link_color(c))
  
  links <- after
  colnames(links) <- c("from","to","weight")
  links$edge.colour <- pbsapply(links$weight,get_edge_colour) #add colour for links
  #nodes
  node_number <- round(max(effect[,2]))
  node_col <- data.frame(colfunc(2*node_number+1),seq(-node_number,node_number,1))
  get_vertex_colour <- function(i){
    temp <-  round(i)
    temp2 <- adjustcolor(node_col[which(node_col[,2]==temp),1], alpha=1)
    return(temp2)
  }
  
  nodes <- data.frame(unique(links[,2]),unique(links[,2]),extra)
  colnames(nodes) <- c("id","name","ind_effect")
  nodes <- nodes[order(nodes[,1]),]
  nodes$influence <- aggregate(dep_effect ~ to, data = after, sum)[,2]
  nodes$colour <- pbsapply(nodes$influence,get_vertex_colour) #add colour for links
  
  #normalization
  normalization <- function(x){(x-min(x))/(max(x)-min(x))*1.5+1} 
  #normalization2
  normalization2 <- function(x){log(abs(x)+1)+1}
  
  #final plot
  links[,3] <- normalization2(abs(links[,3]))
  nodes[,3:4] <- normalization2(nodes[,3:4])
  net <- graph_from_data_frame( d=links,vertices = nodes,directed = T ) 
  
  #layout
  set.seed(1)
  l <- layout_in_circle(net)
  l <- layout_on_sphere(net)
  plot.igraph(net,
               vertex.label=V(net)$name,
               vertex.label.color="black",
               vertex.shape="circle", 
               vertex.label.cex=V(net)$ind_effect*0.8+0.2,
               vertex.size=V(net)$ind_effect*13,
               edge.curved=0.05,
               edge.color=E(net)$edge.colour,
               edge.frame.color=E(net)$edge.colour,
               edge.width=E(net)$weight+1,
               vertex.color=V(net)$colour,
               layout=l,
               margin=c(-.25,-.25,-.25,-.25)
  )
  box("figure")
}

network_plot4 <- function(k,effect){
  get_extra <- function(i){
    temp <- i[[5]][1]
    return(temp)
  }
  extra <- sapply(k,get_extra)
  
  after <- do.call(rbind,lapply(k, get_after))
  
  colfunc <- colorRampPalette(c("#619CFF", #ggplot blue
                                "#ffdead", 
                                "#F8766D"))#ggplot red
  #unchange-weight-data
  #links
  #colour_edge
  edge_number <- round(max(effect[,1]))
  edge_col <- data.frame(colfunc(2*edge_number+1),seq(-edge_number,edge_number,1))
  
  get_edge_colour <- function(i){
    temp <-  round(i)
    temp2 <- adjustcolor(edge_col[which(edge_col[,2]==temp),1], alpha=0.5)
    return(temp2)
  }
  links <- after
  colnames(links) <- c("from","to","weight")
  
  get_link_color <- function(i){
    tmp <- links$weight[i]
    if (tmp >= 0 ) {
      tmp2 <- adjustcolor("#F8766D", alpha=1)
    } else {
      tmp2 <- adjustcolor("#619CFF", alpha=1)
    }
    return(tmp2)
  }
  
  links$edge.colour <- sapply(1:nrow(links),function(c)get_link_color(c))
  
  links <- after
  colnames(links) <- c("from","to","weight")
  links$edge.colour <- pbsapply(links$weight,get_edge_colour) #add colour for links
  #nodes
  node_number <- round(max(effect[,2]))
  node_col <- data.frame(colfunc(2*node_number+1),seq(-node_number,node_number,1))
  get_vertex_colour <- function(i){
    temp <-  round(i)
    temp2 <- adjustcolor(node_col[which(node_col[,2]==temp),1], alpha=1)
    return(temp2)
  }
  
  nodes <- data.frame(unique(links[,2]),unique(links[,2]),extra)
  colnames(nodes) <- c("id","name","ind_effect")
  nodes <- nodes[order(nodes[,1]),]
  nodes$influence <- aggregate(dep_effect ~ to, data = after, sum)[,2]
  nodes$colour <- pbsapply(nodes$influence,get_vertex_colour) #add colour for links
  
  #normalization
  normalization <- function(x){(x-min(x))/(max(x)-min(x))*1.5+1} 
  #normalization2
  normalization2 <- function(x){log(abs(x)+1)+1}
  
  #final plot
  links[,3] <- normalization2(abs(links[,3]))
  nodes[,3:4] <- normalization2(nodes[,3:4])
  net <- graph_from_data_frame( d=links,vertices = nodes,directed = T ) 
  
  #layout
  set.seed(1)
  l <- layout_in_circle(net)
  l <- layout_on_sphere(net)
  plot.igraph(net,
               vertex.label=V(net)$name,
               vertex.label.color="black",
               vertex.shape="circle", 
               vertex.label.cex=V(net)$ind_effect*1.2,
               vertex.size=V(net)$ind_effect*16,
               edge.curved=0.05,
               edge.color=E(net)$edge.colour,
               edge.frame.color=E(net)$edge.colour,
               edge.width=E(net)$weight+1,
               vertex.color=V(net)$colour,
               layout=l,
               margin=c(-.25,-.25,-.25,-.25)
  )
  box("figure")
}

network_plot5 <- function(k,effect){ 
  get_extra <- function(i){
    temp <- i[[5]][1]
    return(temp)
  }
  extra <- sapply(k,get_extra)
  
  after <- do.call(rbind,lapply(k, get_after))
  
  colfunc <- colorRampPalette(c("#619CFF", #ggplot blue
                                "#ffdead", 
                                "#F8766D"))#ggplot red
  #unchange-weight-data
  #links
  #colour_edge
  edge_number <- round(max(effect[,1]))
  edge_col <- data.frame(colfunc(2*edge_number+1),seq(-edge_number,edge_number,1))
  
  get_edge_colour <- function(i){
    temp <-  round(i)
    temp2 <- adjustcolor(edge_col[which(edge_col[,2]==temp),1], alpha=0.5)
    return(temp2)
  }
  links <- after
  colnames(links) <- c("from","to","weight")
  
  get_link_color <- function(i){
    tmp <- links$weight[i]
    if (tmp >= 0 ) {
      tmp2 <- adjustcolor("#F8766D", alpha=1)
    } else {
      tmp2 <- adjustcolor("#619CFF", alpha=1)
    }
    return(tmp2)
  }
  
  links$edge.colour <- sapply(1:nrow(links),function(c)get_link_color(c))
  
  links <- after
  colnames(links) <- c("from","to","weight")
  links$edge.colour <- pbsapply(links$weight,get_edge_colour) #add colour for links
  #nodes
  node_number <- round(max(effect[,2]))
  node_col <- data.frame(colfunc(2*node_number+1),seq(-node_number,node_number,1))
  get_vertex_colour <- function(i){
    temp <-  round(i)
    temp2 <- adjustcolor(node_col[which(node_col[,2]==temp),1], alpha=1)
    return(temp2)
  }
  
  nodes <- data.frame(unique(links[,2]),unique(links[,2]),extra)
  colnames(nodes) <- c("id","name","ind_effect")
  nodes <- nodes[order(nodes[,1]),]
  nodes$influence <- aggregate(dep_effect ~ to, data = after, sum)[,2]
  nodes$colour <- pbsapply(nodes$influence,get_vertex_colour) #add colour for links
  
  #normalization
  normalization <- function(x){(x-min(x))/(max(x)-min(x))*1.5+1} 
  #normalization2
  normalization2 <- function(x){log(abs(x)+1)+1}
  
  #final plot
  links[,3] <- normalization2(abs(links[,3]))
  nodes[,3:4] <- normalization2(nodes[,3:4])
  net <- graph_from_data_frame( d=links,vertices = nodes,directed = T ) 
  
  #layout
  set.seed(1)
  l <- layout_in_circle(net)
  
  plot.igraph(net,
               vertex.label=V(net)$name,
               vertex.label.color="black",
               vertex.shape="circle", 
               vertex.label.cex=V(net)$ind_effect*1.5,
               vertex.size=V(net)$ind_effect*25,
               edge.curved=0.05,
               edge.color=E(net)$edge.colour,
               edge.frame.color=E(net)$edge.colour,
               edge.width=E(net)$weight+1,
               vertex.color=V(net)$colour,
               layout=l,
               margin=c(-.15,-.15,-.15,-.15)
  )
  box("figure")
}

network_plot8 <- function(k,effect){
  #extra <- as.numeric(table(df$cluster))
  get_extra <- function(i){
    temp <- i[[5]][1]
    return(temp)
  }
  extra <- sapply(k,get_extra)
  
  after <- do.call(rbind,lapply(k, get_after))
  
  colfunc <- colorRampPalette(c("#619CFF", #ggplot blue
                                "#ffdead", 
                                "#F8766D"))#ggplot red
  #unchange-weight-data
  #links
  #colour_edge
  edge_number <- round(max(effect[,1]))
  edge_col <- data.frame(colfunc(2*edge_number+1),seq(-edge_number,edge_number,1))
  
  get_edge_colour <- function(i){
    temp <-  round(i)
    temp2 <- adjustcolor(edge_col[which(edge_col[,2]==temp),1], alpha=0.5)
    return(temp2)
  }
  links <- after
  colnames(links) <- c("from","to","weight")
  
  get_link_color <- function(i){
    tmp <- links$weight[i]
    if (tmp >= 0 ) {
      tmp2 <- adjustcolor("#F8766D", alpha=1)
    } else {
      tmp2 <- adjustcolor("#619CFF", alpha=1)
    }
    return(tmp2)
  }
  
  links$edge.colour <- sapply(1:nrow(links),function(c)get_link_color(c))
  
  links <- after
  colnames(links) <- c("from","to","weight")
  links$edge.colour <- pbsapply(links$weight,get_edge_colour) #add colour for links
  #nodes
  node_number <- round(max(effect[,2]))
  node_col <- data.frame(colfunc(2*node_number+1),seq(-node_number,node_number,1))
  get_vertex_colour <- function(i){
    temp <-  round(i)
    temp2 <- adjustcolor(node_col[which(node_col[,2]==temp),1], alpha=1)
    return(temp2)
  }
  
  nodes <- data.frame(unique(links[,2]),unique(links[,2]),extra)
  colnames(nodes) <- c("id","name","ind_effect")
  nodes <- nodes[order(nodes[,1]),]
  nodes$influence <- aggregate(dep_effect ~ to, data = after, sum)[,2]
  nodes$colour <- pbsapply(nodes$influence,get_vertex_colour) #add colour for links
  
  #normalization
  normalization <- function(x){(x-min(x))/(max(x)-min(x))*1.5+1} 
  #normalization2
  normalization2 <- function(x){log(abs(x)+1)+1}
  
  #final plot
  links[,3] <- normalization2(abs(links[,3]))
  nodes[,3:4] <- normalization2(nodes[,3:4])
  net <- graph_from_data_frame( d=links,vertices = nodes,directed = T ) 
  
  #layout
  set.seed(1)
  l <- layout_in_circle(net)
  
  plot.igraph(net,
               vertex.label=V(net)$name,
               vertex.label.color="black",
               vertex.shape="circle", 
               vertex.label.cex=V(net)$ind_effect*0.6+0.5,
               vertex.size=V(net)$ind_effect*12,
               edge.curved=0.05,
               edge.color=E(net)$edge.colour,
               edge.frame.color=E(net)$edge.colour,
               edge.width=E(net)$weight+1,
               vertex.color=V(net)$colour,
               layout=l,
               margin=c(-.15,-.15,-.15,-.15)
  )
  box("figure")
}

pdf("Fig5.pdf",width=35,height=40)
layout(matrix(1:56, 8, 7, byrow = TRUE))
#1
sapply(1:7,function(c)network_plot1(all_net[[c]],max_effect2[[c]]))
#2
sapply(8:14,function(c)network_plot2(all_net[[c]],max_effect2[[c]]))
#3
sapply(15:21,function(c)network_plot3(all_net[[c]],max_effect2[[c]]))
#4
sapply(22:28,function(c)network_plot4(all_net[[c]],max_effect2[[c]]))
#5
sapply(29:35,function(c)network_plot5(all_net[[c]],max_effect2[[c]]))
mtext(paste("         1         ","         2         ","         3         ","         4         ",
            "         5         ","         6         ","         7         "), 
      line = 0,side = 2, col = 'black', font = 1, cex = 5)
#6
sapply(36:42,function(c)network_plot2(all_net[[c]],max_effect2[[c]]))
#7
sapply(43:49,function(c)network_plot5(all_net[[c]],max_effect2[[c]]))
#8
sapply(50:56,function(c)network_plot8(all_net[[c]],max_effect2[[c]]))

dev.off()



layout(matrix(1:24, 8, 3, byrow = TRUE))
#1
sapply(1:3,function(c)network_plot1(all_net2[[c]],max_effect_hc2[[c]]))
#2
sapply(4:6,function(c)network_plot2(all_net2[[c]],max_effect_hc2[[c]]))
#3
sapply(7:9,function(c)network_plot3(all_net2[[c]],max_effect_hc2[[c]]))
#4
sapply(10:12,function(c)network_plot4(all_net2[[c]],max_effect_hc2[[c]]))
#5
sapply(13:15,function(c)network_plot5(all_net2[[c]],max_effect_hc2[[c]]))
#6
sapply(16:18,function(c)network_plot2(all_net2[[c]],max_effect_hc2[[c]]))
#7
sapply(19:21,function(c)network_plot5(all_net2[[c]],max_effect_hc2[[c]]))
#8
sapply(22:24,function(c)network_plot8(all_net2[[c]],max_effect_hc2[[c]]))



