#Requirerements:
#git
#R >=4.1.1
#connection to internet
# Clone the github reporistory
system("git clone https://github.com/DanielBonnery/ensae-macro.git")
# Change working directory
setwd("ensae-macro")
# Install packages targets and renv if not already installed
c("targets","renv")|>
  setdiff(installed.packages()[,"Package"])|>
  sapply(install.packages)
# load package targets
library(targets)
# run the code:
# This will 
#1. install other necessary packages if not already available 
#(exact list of installed packages is in requirements.txt)
#2. create a subdirectory named outputs
#3. create a subdirectory named "_targets"
#4. run the var mcmc, irfs save  graphics into outputs and objects into _targets
tar_make()