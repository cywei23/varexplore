# varexplore
#### Variable Exploratory Analysis in Shiny Dashboard

## Installation:
install.packages("devtools")
library(devtools)
install_github("cywei23/varexplore")

## Example:
'''{r}
#Package loading and apply function
library(varexplore)  # Load package and its data
data(lending)  # Load sample data
str(lending)  # Quick description of the data
table(lending$Bad)  # Tabulate target variable
lending <- subset(lending, select = -c(id)) # Remove ID

# Package application
varexplore(lending, "Bad")
'''
