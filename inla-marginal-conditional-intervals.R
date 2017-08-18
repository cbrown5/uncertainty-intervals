# INLA looking at different ways of getting intervals on predictions
# CJ Brown 2017-08-18

library(dplyr)
library(INLA)
library(purrr)

# ---------------
# Make data
# ---------------

set.seed(42)
n <- 200
x <- rnorm(200) #covarite
a <- 5 #slope
int <- -5
ngrps <- 10 #number groups
igrp <- rep(1:ngrps, each = n/ngrps)
beta <- rnorm(ngrps, sd = 5) #Randome effects
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

m1 <- inla(y ~ x +
            f(grp, model = "iid"),
    data = dmod, family = "gaussian",
    control.predictor = list(link = 1),
    control.compute = list(config = TRUE))
summary(m1)

# ---------------
# Generate predictions using posterior samples
# ---------------

nsamps <- 1000
psamps <- inla.posterior.sample(nsamps, m1)
rownams <- row.names(psamps[[1]]$latent)
coefnams <- c("Intercept", "x")
irow <- map_dbl(coefnams, ~grep(pattern = .x, rownams))
pars <- map(psamps, function(x) x$latent[irow,])

calclinpred <- function(mcoef, x){
    mcoef[1] +  mcoef[2] * x
}

linpredout <- map(pars, ~calclinpred(.x, dpred$x))
linpredout <- do.call(rbind, linpredout)
linpredquants <- apply(linpredout, 2, quantile, probs = c(0.025, 0.5, 0.975)) %>% t()

# ---------------
# Plot
# ---------------

dpred$linpred <- m1$summary.linear.predictor[ipred,4]
dpred$lwr <- m1$summary.linear.predictor[ipred,3]
dpred$upr <- m1$summary.linear.predictor[ipred,5]

png(filename = "~/Code/uncertainty-intervals/inla-example.png", width = 300 * 8, height = 300*8, res = 300)
plot(dpred$x, dpred$linpred, type = 'l', ylim = c(-30, 30), main = "Median and 95% CIs for two methods")
lines(dpred$x, dpred$lwr, lty = 2)
lines(dpred$x, dpred$upr, lty = 2)

lines(dpred$x, linpredquants[,2], lty = 1, col = "red")
lines(dpred$x, linpredquants[,1], lty = 2, col = "red")
lines(dpred$x, linpredquants[,3], lty = 3, col = "red")

legend('topleft', legend = c("INLA linear predictor", "INLA posterior samples"),
    lty =c(1, 1), col = c("black", "red"))


dev.off()
