data_generate <- function(n = 400, ntest = 1, model = "C1") {
    
    require(MASS)
    N <- n + ntest
    rho <- 0.5
    
    Sigma23 <- matrix(rho, ncol = 2, nrow = 2)
    Sigma47 <- matrix(rho, ncol = 4, nrow = 4)
    diag(Sigma23) <- 1
    diag(Sigma47) <- 1    
    
    
    x1 <- rnorm(N, 0, 1)
    v23 <- MASS::mvrnorm(N, rep(0, 2), Sigma = Sigma23)
    v47 <- MASS::mvrnorm(N, rep(0, 4), Sigma = Sigma47)
    x2 <- v23[, 1]
    x3 <- v23[, 2]
    x4 <- v47[, 1]
    x5 <- v47[, 2]
    x6 <- v47[, 3]
    x7 <- v47[, 4]
    x8 <- rexp(N, 1)
    x9 <- rbinom(N, 1, 0.5)
    x10 <- sample(0:9, N, replace = TRUE)
    eps <- rnorm(N, mean = 0, sd = 0.6)
    
    z <- rbinom(N, 1, 0.5)
    
    sub <- rep(FALSE, N)
    if(model == "C00"){
        trteff <- rep(0,N)
        y <- eps
    } else if(model == "C02"){
        trteff <- rep(0,N)
        y <- 0.5 * (x1 + x2) + eps
    } else if (model == "C01") {
        trteff <- rep(0, N)
        y <- x1 + eps
    } else if(model == "C1"){
        sub <- x1 < 0
        trteff <- 0.5 * ifelse(x1 < 0, 1, -1)
        y <-  trteff*z + eps
    } else if(model == "C2"){
        sub <- abs(x1) < 0.65
        trteff <- 0.5 * ifelse(abs(x1) < 0.65, 1, -1)
        y <- trteff*z + eps
    } else if(model == "C3"){
        sub <- x9 == 0
        trteff <- 0.5 * ifelse(x9 == 0, 1, -1)
        y <-  0.5 * (x1 + x2) + trteff*z + eps
    } else if(model == "C4"){
      	trteff <- 0.5 * ifelse(x3 < 0, 1, -1)
        sub <- x3 < 0
        y <- x2 + trteff*z + eps
    } else if(model == "C5"){
        trteff <- 0.5 * ifelse(x10 %% 2 == 1, 1, -1)
        sub <- x10 %% 2 == 1
        y <- x1 + trteff*z + eps
    } else if (model == "C6") {
        trteff <- 0.5 * ifelse(x1 < 0, 1, -1)
        sub <- x1 < 0
        y <- 0.5 * (x2 + x2^2) + trteff*z + eps
    }
    
    data <- data.frame(x1, x2, x3, x4, x5, x6, x7, 
                       x8, x9 = x9, x10 = factor(x10), 
                       z = as.factor(z), 
                       Treat = ifelse(z == 1, "Treatment", "Placebo"), 
                       y = y, trteff = trteff, sub = sub)
    list(train = data[1:n, ], test = data[(n+1):N, ])
}

mean.se <- function(sxx,sx,n){
    m <- NA
    se <- NA
    if(n == 1){
        m <- sx
        se <- 0
    } else if(n > 1){
        m <- sx/n
        se <- sqrt((sxx-n*m*m)/(n*(n-1)))
    }
    return(c(round(m,3),round(se,3)))
}

find.means <- function(est.mean, data_test, index, model){
    ## This function only call, when 
    ## 1. there is a subgroup in the train: est.mean > 0
    ## 2. Find a subgroup in the test: sum(index) > 0

    gpsize <- NA
    true.mean <- NA
    bias <- NA
    relbias <- NA
    idratio <- gpratio <- NA    

    if( est.mean <= 0 | sum(index) <= 0) {
        return(c(gpsize,est.mean,true.mean,bias,relbias))
    }

    n <- nrow(data_test)
    sub_test <- data_test[index,]
    n_s <- nrow(sub_test)
    gpsize <- n_s / n
    idratio <- sum(sub_test[, "sub"]) / sum(data_test[, "sub"])
    gpratio <- mean(sub_test[, "sub"])
    
    if(model == "C00" | model == "C01" | model == "C02"){
        true.mean <- 0
        relbias <- NA
    } else {
        true.mean <- mean(sub_test[, "trteff"])
        if(true.mean <= 0){
            if(abs(est.mean) <= 0){
                relbias <- 1
            } else {
                relbias <- Inf         
            }
        } else {
            relbias <- est.mean/true.mean
        }
    }
    bias <- est.mean - true.mean
    
    if(is.na(true.mean)) gpsize <- NA
    return(c(gpsize, est.mean,true.mean,bias,relbias, idratio, gpratio))
}

get_info <- function(file, model, method) {
    if (grepl('*.RDS',file)) {
        results <- readRDS(file)
    } else {
        load(file)
    }
    return(results[[model]][[method]])
}


summary_res <- function(files, methods, disname, models_1, models_2) {
  models <- c(models_1, models_2)
  summary_results <- list()
  conditions <- models
  for(cond in conditions) {
    summary_results[[cond]] <- list()
    for(meth in methods) {
      info <- lapply(files, FUN = function(x) {get_info(x, cond, meth)})
      info <- do.call("rbind", info)
      time <- as.numeric(info$time)
      
      if(meth %in% c("ROWSi", "FindIt", "FROWSiO", "Chen")) {
        var <- sapply(info, FUN = function(x) {x[["var"]]})
        tmp <- apply(var, 1 , mean)
        dist <- tmp / sum(tmp)
      } else {
        var <- info$var
        dist <- table(var) / length(var)
      }

      find_sub <- info$find_sub
      ## --- Further processing --- ##
      p <- mean(find_sub)
      se <- round(sqrt(p*(1-p) / NROW(find_sub)), 5)

      summary_results[[cond]][[meth]][["p"]] <- c(p, se) 
      summary_results[[cond]][[meth]][["time"]] <- mean.se(sum(time^2), sum(time), NROW(time))
      summary_results[[cond]][[meth]][["dist"]] <- dist
      
      quans <- c("gpsize", "est.mean","idratio", "gpratio", "true.mean", "bias", "relbias")
      gpsize <- info$gpsize
      index <- !is.na(gpsize)
      for (quan in quans) {
        tmp <- info[[quan]]
        tmp <- tmp[index]
        ind2 <- tmp != Inf
        tmp <- tmp[ind2]
        notNA <- sum(!is.na(tmp))
        if(quan == "relbias") {
          summary_results[[cond]][[meth]][[quan]] <- c(round(median(tmp), 3), 1)
        } else {
          tmp2 <- sum(tmp^2, na.rm = T)
          tmp <- sum(tmp, na.rm = T)
          summary_results[[cond]][[meth]][[quan]] <- mean.se(tmp2, tmp, notNA)
        }
      }
    } 
  }

  ## Variable selection matrix
  mresults <- NULL
  conditions = models
  var_names <-c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10")
  mresults <- data.frame(matrix(0, length(conditions) * length(methods), length(var_names) + 2))
  colnames(mresults) <- c("Conditions", "Method", var_names)
  for(cond in seq_along(conditions)) {
    for(met in seq_along(methods)) {
      tmp_dist <- summary_results[[conditions[cond]]][[methods[met]]][["dist"]]
      index <- (cond - 1) * length(methods) + met
      for(name in names(tmp_dist)) mresults[index, name] <- tmp_dist[name]
      mresults[index, "Conditions"] <- conditions[cond]
      mresults[index, "Method"] <- disname[met]
    }
  }
  var_select_matrix <- mresults

  conditions = models_1
  quans <- c("p", "gpsize",  "bias", "time")
  mresults <- list()
  for(cond in conditions) {
    mean.results <- matrix(NA, length(methods), length(quans))
    se.results <- matrix(NA, length(methods), length(quans))
    for(met in seq_along(methods)) {
        for(quan in seq_along(quans)) {
            quat <- quans[quan]
            mean.results[met, quan] <- summary_results[[cond]][[met]][[quat]][1]
            se.results[met, quan] <- summary_results[[cond]][[met]][[quat]][2]
        }
    }
    colnames(mean.results) <- colnames(se.results) <- quans
    rownames(mean.results) <- rownames(se.results) <- methods

    results <- data.frame(mean.results, Condition = cond)
    mresults[[cond]][["mean"]] <- results
    
    results <- data.frame(se.results, Condition = cond)
    mresults[[cond]][["se"]] <- results
  }

  mean.full <- NULL
  se.full <- NULL
  if(!is.null(models_1)) {
    for(i in mresults) mean.full <- rbind(mean.full, i[["mean"]])
    mean.full <- as.data.frame(mean.full)
  
    mean.full$Method <- rep(disname, nrow(mean.full)/length(disname))
    mean.full <- mean.full[, c("Condition","Method", quans)]
    
    for(i in mresults) se.full <- rbind(se.full, i[["se"]])
    se.full$Method <- rep(disname, nrow(mean.full)/length(disname))
    se.full <- se.full[, c("Condition","Method", quans)]
  }
  
  ## --- Summary Result 2 --- ##
  conditions <- models_2
  quans <- c("p", "gpsize", "idratio", "gpratio","est.mean", "true.mean", "bias", "relbias", "time")
  
  mresults <- list()
  for(cond in conditions) {
    mean.results <- matrix(NA, length(methods), length(quans))
    se.results <- matrix(NA, length(methods), length(quans))
    for(met in seq_along(methods)) {
        for(quan in seq_along(quans)) {
            quat <- quans[quan]
            meth <- methods[met]
            mean.results[met, quan] <- summary_results[[cond]][[meth]][[quat]][1]
            se.results[met, quan] <- summary_results[[cond]][[meth]][[quat]][2]
        }
    }
    colnames(mean.results) <- colnames(se.results) <- quans
    
    results <- data.frame(mean.results, Condition = cond, Method = methods)
    mresults[[cond]][["mean"]] <- results
    
    results <- data.frame(se.results, Condition = cond, Method = methods)
    mresults[[cond]][["se"]] <- results
  }
  
  mean.full2 <- NULL
  se.full2 <- NULL

  if(!is.null(models_2)) {
  for(i in mresults) mean.full2 <- rbind(mean.full2, i[["mean"]])
  mean.full2$Method <- rep(disname, nrow(mean.full2)/length(disname))
  mean.full2 <- mean.full2[, c("Condition","Method", quans)]
  
  for(i in mresults) se.full2 <- rbind(se.full2, i[["se"]])
  se.full2$Method <- rep(disname, nrow(mean.full2)/length(disname))
  se.full2 <- se.full2[, c("Condition", "Method", quans)] }

  return(list(var_m = var_select_matrix, model1 = mean.full, model1.se = se.full,
              model2 = mean.full2, model2.se = se.full2, summary_results = summary_results))
  
}


guide_summary <- function(guide_res, models = paste0("C", c("00", "01", "02", 1:6)),
                          methods = c("con", "lin", "mul")) {

    mat_sum <- function(sum_mat) {
        ## variable distribution
        var_dist <- rep(0, 10)
        names(var_dist) <- paste0("x", 1:10)
        var_tmp <- table(sum_mat$var) / nrow(sum_mat)
        for(var in names(var_tmp)) {
            var_dist[var] = var_tmp[var]
        }
        
        ## C00, C01, p, gpsize, bias, time
        
        p <- mean(sum_mat$find_sub)
        gpsize <- mean(sum_mat$gpsize, na.rm = T)
        gpratio <- mean(sum_mat$gpratio, na.rm = T)
        est.mean <- mean(sum_mat$est.mean, na.rm = T)
        true.mean <- mean(sum_mat$true.mean, na.rm = T)
        bias <- mean(sum_mat$bias, na.rm = T)

        list(var = var_dist,
             quan = c(p = p, gpsize = gpsize, 
                      gpratio = gpratio, est.mean = est.mean,
                      true.mean = true.mean, bias = bias))
    }

    nm <- length(models)
    nme <- length(methods)

    var_dist <- matrix(NA, nm * nme, 10)
    colnames(var_dist) <- paste0("x", 1:10)
    quan <- c("p", "gpsize", "gpratio", "est.mean", "true.mean", "bias")
    summary_s <- matrix(NA, nm * nme, length(quan))
    colnames(summary_s) <- quan

    model <- matrix(NA, nm * nme, 2)
    colnames(model) <- c("Conditions", "Method")
    j = 1
    for (mod in models) {
        for (meth in methods) {
            tmp <- mat_sum(guide_res[[mod]][[meth]])
            var_dist[j, ] <- tmp$var
            summary_s[j, ] <- tmp$quan

            model[j, ] <- c(mod, paste0("G", meth)) 
            j = j + 1
        }
    }

    var_dist <- data.frame(model, var_dist)
    summary_s <- data.frame(model, summary_s)

    model1 <- summary_s[summary_s$Conditions %in% c("C00", "C01", "C02"), c("Conditions", "Methods", "p", "bias")]
    model2 <- summary_s[!summary_s$Conditions %in% c("C00", "C01", "C02"), ]

    model1$true.mean <- NULL
    model2$relbias <- model2$est.mean / model2$true.mean
    list(var_m = var_dist, model1 = model1, model2 = model2)
}

