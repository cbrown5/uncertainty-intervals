# An examination of different methods for generating uncertainty intervals in mixed effects models

## CJ Brown

2017-08-18


## inla-example.png
Intervals using the `INLA` package
My intuition was that posterior samples would be conditional on RE, whereas the linear predictor that INLA outputs would be mariginal. However, both are are marginal (basically identical CIs)

## lmer-example.png
Intervals using the `lmer` package
Use `bootMer` to get itnervals. You can change the `use.u` argument to get conditional vs marginal CIs. (note the marginals are almost identical to inla for this example data).
The conditionals are tighter because we are not bootstrapping over RE variance. 
