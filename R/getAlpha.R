#' Get bootstarp alpha based on previous result.
.getAlpha <- function(bootAlpha) {
    res <- c(0.05, 0.05, 0.05)
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
        res <- c(alpha, indAlpha, oveAlpha)
    }
}
