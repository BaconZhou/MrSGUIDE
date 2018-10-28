.charLevels <- function(x) {
    tmp = as.character(x)
    tmp[which(is.na(x))] = "12345679"
    sort(unique(tmp))
}

.dictRename <- function(dictList, cvarName = NULL) {
    if (!is.null(cvarName) && is.null(names(dictList))) names(dictList) = cvarName
    .naTchar <- function(x) {
        if (any(is.na(x))) {
            ind = which(is.na(x))
            x[ind] = "12345679"
        }
        return(x)
    }
    
    lapply(dictList, .naTchar)
}

check <- function(logicstatus, message) {
    if (!logicstatus) 
        stop(message, call. = FALSE)
}