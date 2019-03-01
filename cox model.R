source("epi data.R")
library(coxme)

cox.drug.ri.re <- coxme(Surv(SEZTIME, SCENS) ~ DRUG + (1 + DRUG |TRIAL), data = epi)

se <- function(object) 
  sqrt(diag(vcov(object)))


confint.coxme <- function(object, level = .95, digits = 2) {
  z <- qnorm(1 - (1 - level)/2) 
  b <- coef(object)
  s <- se(object)
  
  ci.lb <- b - z * s
  ci.ub <- b + z * s
  
  out <- data.frame(b, ci.lb, ci.ub, s, exp(b), exp(ci.lb), exp(ci.ub))
  out <- round(out, digits = digits)
  colnames(out) <- c("coef", "ci.lb(coef)", "ci.ub(coef)", "se(coef)",
                     "exp(coef)", "ci.lb(exp(coef))", "ci.ub(exp(coef))")
  out$`Wald p` <- round(pnorm(b/s, lower.tail = F) * 2, digits + 1)

  out$CI <- paste(out$`ci.lb(exp(coef))`, " to ", out$`ci.ub(exp(coef))`, sep = "", collapse = NULL)
  out
}



confint(cox.drug.ri.re)
write.csv(confint(cox.drug.ri.re), file = "manuscript/confint cox 1.csv")


cox.drug.ri.re.cov <- coxme(Surv(SEZTIME, SCENS) 
                            ~ DRUG 
                            + (1 + DRUG |TRIAL)
                            + EPTYPE.center 
                            + EPTYPE.trialmean
                            + EPTYPE.center : DRUG
                            , data = epi)

  
confint(cox.drug.ri.re.cov)
write.csv(confint(cox.drug.ri.re.cov), file = "manuscript/confint cox 2.csv")


