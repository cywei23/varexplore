#varexplore 
### *- Variable Exploratory Analysis & Shiny Dashboard Output* 
=============

## Installation:
```
install.packages("devtools")
library(devtools)
install_github("cywei23/varexplore/varexplore")
```
## Example:
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

### Data Dictionary
Lending data stores in path: varexplore/data/LCDataDictionary.xlsx
