mrs_tres <- function(data_train,
                     method = "MrCon",
                     maxDepth = 10, prune, index) {
  require(MrS)
  keep_vars <- c(paste0("x", 1:10), "z", "y")
  train <- data_train[, keep_vars]

  role <- ifelse(sapply(train, class) == 'numeric', 'n', 'c')
  role['z'] = 'r'
  role['y'] = 'd'

  data_tmp <- rbind(train, train)
  data_tmp$z <- factor(rep(c(0, 1), each = nrow(train)))
  pla_ind <- 1:nrow(train)
  trt_ind <- (nrow(train)+1):(2*nrow(train))

  if (method == 'MrCon') {
    bestK = 0
  } else {
    bestK = 2
  }

  if (prune) {
    CVFolds = 10
    CVSE = 0.5
  } else {
    CVFolds = 0
    CVSE = 0
  }

  time <- system.time(fit <- MrSFit(dataframe = train, role = role, bestK = bestK,
                                    minData = 30,
                maxDepth = maxDepth, CVFolds = CVFolds, CVSE = CVSE,
                bootNum = 0, display = FALSE, remove=FALSE,
                treeName = paste0("tree_",index,".yaml"),
                nodeName = paste0("node_",index,".txt"),
                bootName = paste0("boot_",index,".txt"),
                impName = paste0("imp_",index,".txt")))
  kvar <- fit$treeRes$SplitVar
  node_v = predict(fit, data_tmp)
  nodeMap <- fit$nodeMap
  respond_v <- rep(0, NROW(data_tmp))
  for (node in unique(node_v)) {
    idx <- which(node_v == node)
    dat_id <- data_tmp[idx,]
    mod <- nodeMap[[paste0("term", node)]][['model']][['y']]
    respond_v[idx] = predict(mod, dat_id)
  }
  est.trt = respond_v[trt_ind] - respond_v[pla_ind]
  node_stat <- data.frame(node_v = node_v, est.trt = est.trt) %>%
    group_by(node_v) %>%
    summarise(est.trt = mean(est.trt),
              size = n(), pval = 1) %>% as.data.frame()

  list(fit = fit, var = kvar, time = time, node_stat = node_stat)
}

mrs_subans <- function(data_test, mrs_obj, model_name) {
  ## Fit both treatment effect and X main effect in node model
  ## redefine the meaning of a "selected subgroup", which is a subgroup that is non-empty,
  ## has required minimum sample size, and has the largest positive estimated treatment effect

  require(MrS)
  tree = 0
  find_sub = 0
  est.mean <- NA
  gpsize <- NA
  idratio <- gpratio <- NA
  true.mean <- NA
  bias <- NA
  relbias <- NA

  model <- mrs_obj$fit
  node_stat <- mrs_obj$node_stat

  ## --- Step 1 --- ##
  ## Check result tree is not trival tree
  if(nrow(node_stat) == 1) { # NULL means trivial tree
    trivial = TRUE
  } else {
    trivial = FALSE
    treatment = node_stat[["est.trt"]]
    mti <- which.max(treatment)
    best_node <- node_stat[mti, "node_v"]
    node_size <- node_stat[mti, "size"]
    node_pval <- node_stat[mti, "pval"]
    est.mean <- treatment[mti]
    ntest <- nrow(data_test)
    node_all <- MrS::predict.guide(model, data_test, type = "node")

    index <- which(node_all == best_node)
    gpsize <- length(index) / ntest ## Subgroup size in test data

    if (est.mean > 0 & sum(index) > 0) {
      tree = 1
      find_sub = 1
      res <- find.means(est.mean, data_test, index, model_name)
      gpsize = res[1]
      est.mean = res[2]
      true.mean = res[3]
      bias = res[4]
      relbias = res[5]
      idratio = res[6]
      gpratio = res[7]
    }

  }

  list(tree = tree, find_sub = find_sub,
       gpsize = gpsize, est.mean = est.mean,
       true.mean = true.mean, bias = bias,
         relbias = relbias, idratio = idratio, gpratio = gpratio)
}
