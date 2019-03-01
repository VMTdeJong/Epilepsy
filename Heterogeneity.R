source("cox model.R")

#### The function MHRnormal is adapted from:
# Austin PC, Wagner P, Merlo J. The median hazard ratio: a useful measure of variance and general contextual 
# effects in multilevel survival analysis. Stat Med. November 2016. doi:10.1002/sim.7188

# Let var.re denote the estimate variance of the random effects, following a normal distribution.
MHRnormal <- function(var.re) exp(sqrt(var.re) * qnorm(0.75) * sqrt(2))


#### Statistics in manuscript:
# Drug-only model:
MHRnormal(cox.drug.ri.re$vcoef[["TRIAL"]]["DRUG","DRUG"])

# Model with type:
MHRnormal(cox.drug.ri.re.cov$vcoef[["TRIAL"]]["DRUG","DRUG"])
