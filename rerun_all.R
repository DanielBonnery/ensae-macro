#Requirerements:
#git
#R >=4.1.1
#connection to internet

# Clone the github reporistory
system("git clone https://github.com/DanielBonnery/ensae-macro.git")

# Change working directory
setwd("ensae-macro")

# Install packages targets and renv
if(!is.element("targets",installed.packages()[,"Package"])){install.packages("targets")}
if(!is.element("renv",installed.packages()[,"Package"])){install.packages("renv")}


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