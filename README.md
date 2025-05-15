# VegShiny
========

### Getting started

*RStudio*
Start a new RStudio project from version control
This is the url that you should clone: https://github.com/NCRN/VegShiny

*terminal*
Confirm that RStudio and Git know what github repo to push/pull
`git remote -v`
Check what branch you are on. You should start your project by branching from the `ncrn` branch
`git status`
Confirm that you have the latest version of the `ncrn` branch
`git pull`
Create a new branch for your ticket
`git checkout -b <ticket_name_here>`

*RStudio*
In your project directory, make a new folder `Data`
Copy the contents of the folder below, (c('colors.csv', 'NCRN')), into your `Data` folder
`OneDrive - DOI\Documents - NPS-NCRN-Forest  Veg\11_VISUALIZER\dev\Data`

*R*
### set up your development environment
##### if you don't already have renv, install it
##### install.packages('renv')
##### there is already a renv.lock file in the `ncrn` branch
renv::activate()
renv::install()

*R*
### confirm that your environment works
##### make a sandbox.R file, and run these commands there
library(NPSForVeg)
mydata <- NPSForVeg::importNCRN('Data/NCRN')
