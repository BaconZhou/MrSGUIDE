#' MrS Multiple responses subgroup identification
#'
#' @title MrS
#' @author Peigen Zhou
#'
#' @name MrS
#'
#' @description Multiple responses subgroup identification use GUIDE gi option
#'
#' @param dataframe train data frame
#' @param role role follows GUIDE role
#' @param bestK number of covariates in the regression model
#' @param maxDepth maximum tree depth
#' @param minTrt minimum treatment and placebo sample in each node
#' @param minData minimum sample in each node
#' @param bathNum related with exhaustive search for numerical split variable
#' @param CVFolds cross validataion times
#' @param CVSE cross validation SE
#' @param bootNum bootstrap number
#' @param alpha desire alpha levels for confidence interval with respect to treatment parameters
#' @param faster related with tree split searching
#' @param display Whether display tree in the end
#' @param treeName yaml file for save the tree
#' @param nodeName file same for each node
#' @param bootName boostrap calibrate alpha
#' @param seed random seed control cross-validation
#' @param writeTo debug option reserve for author...
#' @param remove whether to remove extra files
#' @importFrom utils read.table write.table
#' @export
MrSFit <- function(dataframe, role, bestK = 1,
                        maxDepth = 5,
                        minTrt = 5, minData = max(c(minTrt * maxDepth, NROW(Y) / 20)),
                        batchNum = 1L, CVFolds = 10L, CVSE = 0.0,
                        bootNum = 0L, alpha = 0.05, faster = FALSE, display = FALSE,
                        treeName = paste0("tree_", format(Sys.time(), "%m%d"), ".yaml"),
                        nodeName = paste0("node_", format(Sys.time(), "%m%d"), ".txt"),
                        bootName = paste0("boot_", format(Sys.time(), "%m%d"), ".txt"),
                        seed = 123, writeTo = FALSE, remove = FALSE) {
    t1 = Sys.time()

    if(display) cat("Start data processing: \n")
    check(NCOL(dataframe) == length(role),
          "column number of data frame is not same as role length.")
    varName = colnames(dataframe)

    dr = which(role == "d")

    check(length(dr) > 0, "No dependent variable in role.")

    rr = which(role == "r")

    check(length(rr) > 0, "No treatment variable in role.")
    check(length(rr) < 2, paste("Current version can only deal with one treatment variable. Find ", length(rr), "in role."))

    sr = which(role %in% c("n", "s"))
    fr = which(role %in% c("n", "f"))
    hr = which(role %in% c("h"))

    nr = sort(union(union(sr, fr), hr))
    splitIndex = which(nr %in% sr) - 1
    fitIndex = which(nr %in% fr) - 1
    holdIndex = which(nr %in% hr) - 1

    # if (display) {
    #     cat(' SplitIndex: ', splitIndex)
    #     cat(' fitIndex: ', fitIndex)
    #     cat(' holdIndex: ', holdIndex)
    # }

    cr = which(role %in% "c")
    xr = which(role == "x")

    check(length(sr) + length(cr) > 0, "No split variable in role.")

    for (i in c(sr, fr, hr)) {
        check(!class(dataframe[, i]) %in% c("character", "factor"),
              paste(varName[i], "seems a categorical variable? please change the role vector") )
    }

    for (i in cr) {
        check(class(dataframe[, i]) != "numeric",
              paste(varName[i], "seems not a categorical variable. Please change the role vector"))
    }

    for (i in dr) {
        if (class(dataframe[, i]) %in% c("character", "factor"))
            warning(paste(varName[i], "seems a categorical variable? Will force to treat as numerical variable"))
    }

    Yori = as.matrix(dataframe[, dr])
    missInd <- !is.na(Yori)

    if (NCOL(missInd) > 1) {
        non_miss <- as.logical(apply(missInd, 1, prod))
    } else {
        non_miss <- missInd
    }

    if (sum(non_miss) < NROW(Yori)) {
        warning(paste(" Dependent variables have missing value.\n Only use non-missing record for subgroup identification.\n Total is: ", sum(non_miss), "Original: ", NROW(Yori)))
    }

    Y = as.matrix(Yori[non_miss, ])
    cLevels = lapply(dataframe[non_miss, cr], .charLevels)
    cXL = characterDict(dataframe[non_miss, cr], cLevels)
    nX = dataFramToNumeric(dataframe[non_miss, nr])

    tLevels = list(.charLevels(dataframe[non_miss, rr]))
    TrtL = characterDict(dataframe[non_miss, rr], tLevels)
    numVarName = varName[nr]
    catVarName = varName[cr]
    newVar = c(numVarName, catVarName)

    if (writeTo) {
        write.table(nX, file = "nX", row.names = F, col.names = F)
        write.table(cXL$intX, file = "cX", row.names = F, col.names = F)
        write.table(Y, file = "Y", row.names = F, col.names = F)
        write.table(TrtL$intX, file = "Trt", row.names = F, col.names = F)
        write.table(splitIndex, file = "splitIndex", row.names = F, col.names = F)
        write.table(fitIndex, file = "fitIndex", row.names = F, col.names = F)
        write.table(holdIndex, file = "holdIndex", row.names = F, col.names = F)
    }

    if(display) cat("Finish processing, start call main function. ", Sys.time() - t1, "s\n")
    t2 = Sys.time()
    stopifnot(NROW(Y) > 0)
    treeRes = mrs.pure(nX = nX, cX = cXL$intX,
                     Y = Y, Trt = TrtL$intX,
                     splitIndex = splitIndex, fitIndex = fitIndex,
                     holdIndex = holdIndex, bk = bestK, maxDepth = maxDepth,
                     minTrt = minTrt, minData = minData, batchNum = batchNum,
                     CVFolds = CVFolds, CVSE = CVSE, bootNum = bootNum,
                     alpha = alpha, faster = faster, display = FALSE, varName = newVar,
                     treeName = treeName, nodeName = nodeName, bootName = bootName, seed = seed
                     )

    node <- read.table(nodeName, header = TRUE)$node
    treeRes <- .yamlpretty(treeRes, cLevels, node)

    ynames = varName[dr]
    trtname = varName[rr]
    nodeMap = .node.guide(treeRes, node, dataframe[non_miss, ], ynames, trtname)

    res <- list(treeRes = treeRes,
                node = node,
                cLevels = cLevels, tLevels = tLevels,
                yp = NCOL(Y),
                tp = length(tLevels),
                role = role, varName = varName, numName = numVarName,
                catName = catVarName, ynames = ynames, trtname = trtname, nodeMap = nodeMap, TrtL = TrtL)
    if(display) {
        cat("Finish tree build. ", difftime(Sys.time(), t2, units = "secs"), "s\n")
        # print.node(node = treeRes, yName = ynames, trtName = trtname, tlevels = tLevels[[1]], clevels = cLevels)
    }
    if(bootNum > 10) {
        bootAlpha = read.table(bootName, header = FALSE)
        colnames(bootAlpha) = c("gamma", "theta")
        res$bootAlpha = bootAlpha

        if(remove) file.remove(bootName)
    }
    if (remove) file.remove(nodeName, treeName)
    class(res) <- c('guide', 'mrs')
    return(res)
}

#' Pure GUIDE function in subgroup identification use GI method.
#'
#' Perform GUIDE gi option for subgroup identification
#' @importFrom yaml read_yaml
mrs.pure <- function(nX, cX, Y, Trt,
                          splitIndex, fitIndex, holdIndex, bk,
                          maxDepth = 10L, minTrt = 5L,
                          minData = max(c(minTrt * maxDepth, NROW(Y) / 20)),
                          batchNum = 1L, CVFolds = 10L, CVSE = 0.5,
                          bootNum = 500L, alpha = 0.05, faster = FALSE, display = FALSE,
                          varName = NULL,
                          treeName = paste0("tree_", format(Sys.time(), "%m%d"), ".yaml"),
                          nodeName = paste0("node_", format(Sys.time(), "%m%d"), ".txt"),
                          bootName = paste0("boot_", format(Sys.time(), "%m%d"), ".txt"),
                          seed = 123) {
    if(display) cat("Tree file write in :", treeName, ".\n")
    set.seed(seed)

    stopifnot(is.matrix(nX))
    stopifnot(is.integer(cX), is.matrix(cX))
    stopifnot(is.matrix(Y))
    stopifnot(is.integer(Trt))
    stopifnot(length(splitIndex) <= NCOL(nX))
    stopifnot(length(fitIndex) <= NCOL(nX))
    stopifnot(!is.na(nX))
    stopifnot(bk >= 0)
    stopifnot(maxDepth > 0L)
    stopifnot(minTrt > 0L)
    stopifnot(minData > 0L)
    stopifnot(batchNum > 0L)
    stopifnot(CVFolds >= 0L)
    stopifnot(CVSE >= -1e-8)

    if (is.logical(faster)) faster = TRUE
    if (bootNum < 0) bootNum = 0

    if (is.null(varName)) {varName = c(colnames(nX), colnames(cX))}
    if (is.null(treeName)) { treeName = paste0("tree_", format(Sys.time(), "%m%d"), ".yaml")}

    GiStepWisePure(nX, cX, Y, Trt,
               splitIndex, fitIndex, holdIndex,
               as.integer(bk), as.integer(maxDepth),
               as.integer(minData), as.integer(minTrt),
               as.integer(batchNum), as.integer(CVFolds),
               CVSE, as.integer(bootNum), alpha, faster, display, varName,
               treeName = treeName, nodeName = nodeName, bootName = bootName)

    treeRes <- yaml::read_yaml(treeName)

    return(treeRes)
}

#' Mr.S
#'
#' Mr.S identification use GUIDE algorithm
#'
#' @docType package
#' @author Peigen Zhou <pzhou9@wisc.edu>
#' @import Rcpp
#' @useDynLib MrS, .registration = TRUE
#' @importFrom Rcpp evalCpp
#' @exportPattern "^[[:alpha:]]+"
#' @name MrS
NULL
