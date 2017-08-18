# lmer looking at different ways of getting intervals on predictions
# CJ Brown 2017-08-18

library(dplyr)
library(lme4)
library(purrr)
# ---------------
# Generate data
# ---------------

set.seed(42)
n <- 200
x <- rnorm(200)
a <- 5
int <- -5
ngrps <- 10
igrp <- rep(1:ngrps, each = n/ngrps)
beta <- rnorm(ngrps, sd = 5)
y <- int + a*x + beta[igrp] + rnorm(n, sd = 3)
din <- data.frame(y = y, x = x, grp = igrp)

#
#  Predcition df
#

nrep <- 10
dpred <- expand.grid(y = NA, x = seq(min(din$x), max(din$x), length = nrep), grp = NA)
dpred <- dpred[, names(din)]

dmod <- rbind(din, dpred)
ipred <- (nrow(din) + 1): nrow(dmod)

# ---------------
# Fit model
# ---------------

mlmer <- lmer(y ~ x + (1|grp), data = din)

# ---------------
# Predictions
# ---------------
#prediction fucntion for bootMer
predfun1 <- function(model){
    predict(model, newdata = dpred, re.form = NA)
}

#Marginal CIs
bout1 <- bootMer(mlmer, predfun1, nsim = 100, use.u = FALSE)
lmerquants1 <- apply(bout1$t, 2,quantile, probs = c(0.025, 0.5, 0.975)) %>%
    t()

#Conditional CIs note use.u = TRUE
#Also note that in dpred (prediction dataframe) the grp is set to NA. If you have a specific value, e.g 1, then you get conditional predictions for that group (in this model a different intercept)

bout2 <- bootMer(mlmer, predfun1, nsim = 100,  use.u = TRUE)
lmerquants2 <- apply(bout2$t, 2,quantile, probs = c(0.025, 0.5, 0.975)) %>%
    t()

# ---------------
# Plot
# ---------------

lwdt <- 2

png(filename = "~/Code/uncertainty-intervals/lmer-example.png", width = 300 * 8, height = 300*8, res = 300)
plot(dpred$x, lmerquants1[,2], type = 'l', ylim = c(-30, 20), lwd = lwdt)
lines(dpred$x, lmerquants1[,1], lty = 2, lwd = lwdt)
lines(dpred$x, lmerquants1[,3], lty = 2, lwd = lwdt)

lines(dpred$x, lmerquants2[,2], lty = 1, col = "purple", lwd = lwdt)
lines(dpred$x, lmerquants2[,1], lty = 2, col = "purple", lwd = lwdt)
lines(dpred$x, lmerquants2[,3], lty = 2, col = "purple", lwd = lwdt)

legend('topleft', legend = c("Marginal", "Conditional", "Median", "95% CI"),
    lty =c(1, 1, 1, 2), col = c("black", "purple", "black", "black"), lwd = lwdt)
dev.off()
