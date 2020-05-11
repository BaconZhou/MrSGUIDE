context("MrSFit")

test_that("test MrSFit for single responses", {
    set.seed(1234)
    N = 200
    np = 3

    numX <- matrix(rnorm(N * np), N, np) ## numerical features
    gender <- sample(c('Male', 'Female'), N, replace = TRUE)
    country <- sample(c('US', 'UK', 'China', 'Japan'), N, replace = TRUE)

    z <- sample(c(0, 1), N, replace = TRUE) # Binary treatment assignment

    y1 <- numX[, 1] + 1 * z * (gender == 'Female') + rnorm(N)
    y2 <- numX[, 2] + 2 * z * (gender == 'Female') + rnorm(N)

    train <- data.frame(numX, gender, country, z, y1, y2)
    role <- c(rep('n', 3), 'c', 'c', 'r', 'd', 'd')

    mrsobj <- MrSFit(dataframe = train, role = role)
})
