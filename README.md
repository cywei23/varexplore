#varexplore

Variable Exploratory Analysis & Shiny Dashboard Output* 

#### Install from Github
```
install.packages("devtools")
library(devtools)
install_github("cywei23/varexplore/varexplore")
```
#### Install from local source code:
```
#Install dependent packages manually
install.packages("caret")
install.packages("ClustOfVar")
install.packages("data.table")
install.packages("reshape")
install.packages("ROCR")
install.packages("shiny")
install.packages("shinydashboard")

#Install from local source code
install.packages(path_to_file, repos = NULL, type="source")
```

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
