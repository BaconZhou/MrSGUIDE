.predictNode <- function(node, dataframe, cLevels) {
    if (node$Type == 'Terminal') {
        return(node$ID)
    } else {
        val = dataframe[[node$SplitVar]]
        if (node$Role == 'num') {
            if (is.na(val)) {
                if (node$MisDirection %in% c('A', 'L')) {
                    return(.predictNode(node$Left, dataframe, cLevels))
                } else {
                    return(.predictNode(node$Right, dataframe, cLevels))
                }
            } else {
                if (val <= node$Threshold) {
                    return(.predictNode(node$Left, dataframe, cLevels))
                } else {
                    return(.predictNode(node$Right, dataframe, cLevels))
                }
            }
        } else {
            if (val %in% node$ThreshSet) {
                return(.predictNode(node$Left, dataframe, cLevels))
            } else {
                return(.predictNode(node$Right, dataframe, cLevels))
            }
        }
    }
}

#' Predict the node id of MrS regression tree
#'
#' @param mrsobj MrS object
#' @param dataframe data used for prediction
#' @param type node id
#'
#' @export
predictTree <- function(mrsobj, dataframe, type = 'nodeid') {
    n = NROW(dataframe)
    yp = mrsobj$yp
    tp = mrsobj$tp
    if (is.null(dataframe)) return(mrsobj[['node']])
    node <- sapply(1:n, FUN = function(i) {.predictNode(mrsobj$treeRes, dataframe[i, ], mrsobj$cLevels)})
    return(node)
}
