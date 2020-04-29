index <- Sys.getenv('SLURM_ARRAY_TASK_ID')
index <- as.numeric(index)

filename <- paste0("Data/result_", index, ".RData")

.libPaths(c("/workspace/peigen/R/3.6.1", "/workspace/software/R-3.6.1/lib/R/library"))

library(pacman)
p_load(dplyr, MASS, MrS)
source('data_generate.R')
source('mrs_utils.R')

sim_num = 50
n <- 400
ntest <- 5000

models <-c(paste0("C0", 0:2),  paste0("C", 1:6))
models <- paste0("C0", 0:2)
methods <- c("MrCon", "MrLin")

quant <- c("tree", "find_sub", "gpsize",
           "est.mean", "true.mean", "bias", "relbias","idratio", "gpratio")
extquan <- c("var", "time")

results <- list()
check = 0
models <- paste0("C0", 2)
models = 'C3'
methods = 'MrLin'

for(mods in models) {
    results[[mods]] <- list()
    for(meth in methods) {
        results[[mods]][[meth]] <- data.frame(matrix(NA, sim_num, length(c(quant, extquan))))
    	colnames(results[[mods]][[meth]]) <- c(quant, extquan)
    	cat(paste0("Current model: ", mods, ", Current method: ", meth, "\n"))
    	cat("Iter    model    method\n")
        time_m <- Sys.time()

    	for(i in seq_len(sim_num)) {
            cat(paste0(i, "    ", mods, "    ", meth, "\n"))
            set.seed(i + index * sim_num)
            data <- data_generate(model = mods, n = n, ntest = ntest)

            ## Var_fit used to check split variables, it will force to split
            var_fit <- mrs_tres(data$train, method = meth,
                                maxDepth = 2, prune = FALSE, index=index)
            tmp <- mrs_tres(data$train, method = meth,
                             maxDepth = 2, prune = FALSE, index=index)
            if(tmp$fit$treeRes$Type != "Terminal") {
                print(i)
                break
            }
            tmp2 <- mrs_subans(data$test, tmp, model_name = mods)
            tmp2$time <- tmp$time
            var <- ifelse(is.null(tmp$var), var_fit$var, tmp$var)
            tmp2$var <- var[1]
            results[[mods]][[meth]][i, quant] <- tmp2[quant]
            results[[mods]][[meth]][i, extquan] <- c(tmp2$var, tmp$time)
        }
        cat(paste0("Finished model: ", mods, ", method: ", meth, ", time: ", round(Sys.time() - time_m, 3), "\n"))
    }
}
check / sim_num
save(results, file = filename)

data$train %>%
    group_by(x9, z) %>%
    summarise(mu = mean(y))

lackoffit <- function(x, t, y) {
    if (class(x) == 'numeric') {
        cx = factor(cut(x, breaks = quantile(x)))
    } else {
        cx = factor(x)
    }

    fit1 <- lm(y ~ cx + t)
    fit2 <- lm(y ~ cx + t + cx:t)
    ans = anova(fit1, fit2)
    return(ans$`Pr(>F)`[2])
}
dat <- data$train
trt = dat$z
y = dat$y
for (i in 1:10) {
    print(i)
    print(lackoffit(dat[,i], trt, y))
}
