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
            if (val %in% cLevels[[node$SplitVar]][node$ThreshSet]) {
                return(.predictNode(node$Left, dataframe, cLevels))
            } else {
                return(.predictNode(node$Right, dataframe, cLevels))
            }
        }
    }
}

predict.subguide <- function(subguideobj, dataframe, type = 'response') {
    n = NROW(dataframe)
    yp = subguideobj$yp
    tp = subguideobj$tp
    if (is.null(dataframe)) return(subguideobj[['node']])
    node <- sapply(1:n, FUN = function(i) {.predictNode(subguideobj$treeRes, dataframe[i, ], subguideobj$cLevels)})
    return(node)
}
