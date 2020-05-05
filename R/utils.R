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


.getAlpha <- function(bootAlpha) {
    res <- c(0.025, 0.025, 0.025)
    if (!is.null(bootAlpha)) {
        alphaVec <- bootAlpha[, 1]
        ind <- bootAlpha[, 2]
        ove <- bootAlpha[, 3]
        alpha <- max(alphaVec) * 2
        .helper <- function(gamma, alphaVec, alpha = 0.05) {
            p <- which(gamma < 1 - alpha)
            if (p[1] == 1) return(alpha)
            else {
                p = p[1]
                f = (gamma[p - 1] - 1 + alpha) / (gamma[p - 1] - gamma[p])
                alpha = (1 - f) * alphaVec[p - 1] + f * alphaVec[p]
            }
            return(alpha)
        }
        indAlpha = .helper(ind, alphaVec, alpha)
        oveAlpha = .helper(ove, alphaVec, alpha)
        res <- c(alpha / 2, indAlpha, oveAlpha)
    }
}

#' Check the condition and print message
#' @param logicstatus whehter it is true
#' @param message message want to print
check <- function(logicstatus, message) {
    if (!logicstatus)
        stop(message, call. = FALSE)
}

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`
