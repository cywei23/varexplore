#varexplore 
#### *- Variable Exploratory Analysis & Shiny Dashboard Output* 
=============

#### Install
```
install.packages("devtools")
library(devtools)
install_github("cywei23/varexplore/varexplore")
```
##### If install from local source cod:
'''
install.packages(path_to_file, repos = NULL, type="source")
'''

#### Example
```eval_rst
#Package loading and apply function
library(varexplore)  # Load package and its data
data(lending)  # Load sample data
str(lending)  # Quick description of the data
table(lending$Bad)  # Tabulate target variable
lending <- subset(lending, select = -c(id)) # Remove ID

# Package application
varexplore(lending, "Bad")
```

#### Data Dictionary
"Lending.rda" data dictionary stores in path: varexplore/data/LCDataDictionary.xlsx
