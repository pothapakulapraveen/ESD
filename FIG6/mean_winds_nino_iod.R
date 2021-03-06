rm(list=ls())
x11(width=8,height=6)
par(mfrow=c(2,2),mar=c(4,3,4,4))
setwd("/gpfs/ahrenshsmfs/user/pothapak/PAPER-2/PLOTS/WINDS/")
source("meanwind_cli_prec_nino3.4_obs_ind.R")
source("meanwind_cli_prec_IODP_obs_ind.R")
#source("cli_prec_ano_nino3.4_obs_ind.R")
source("mean_cli_prec_lanina_obs_ind.R")
#source("cli_prec_ano_IODP_obs_ind.R")
source("mean_cli_prec_IODN_obs_ind.R")
setwd("/gpfs/ahrenshsmfs/user/pothapak/PAPER-2/FINAL_PLOT")
dev.print(postscript,'meanwinds_nino_iod.eps',width=8,height=6)
