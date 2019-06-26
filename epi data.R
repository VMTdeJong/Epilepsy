library(coxme)
library(survminer)

source("load data.R")

# Centering and trial mean
Center <- function(x, trial.id) {
  for (trial in sort(unique(trial.id)))
  {
    selection.id <- trial.id == trial
    selection <- x[selection.id]
    x[selection.id] <- selection - mean(selection, na.rm = T)
  }
  x
}

TrialMean  <- function(x, trial.id) {
  for (trial in sort(unique(trial.id)))
  {
    selection.id <- trial.id == trial
    x[selection.id] <- mean(x[selection.id], na.rm = T)
  }
  x
}

epi$EPTYPE.trialmean  <- TrialMean(epi$EPTYPE,  epi$TRIAL)
epi$EPTYPE.center     <- Center(epi$EPTYPE,  epi$TRIAL)


# Kaplan meier
km <- survfit(Surv(SEZTIME, SCENS) ~  Drug + Epilepsy, data = epi)
ggsurvplot(km)

cairo_pdf("km.pdf")
ggsurvplot(km)
dev.off()
