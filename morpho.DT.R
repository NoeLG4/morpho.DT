## INITIAL TESTS TO SEE HOW DECISION TREES PERFORM

## import data and install packages
path_to_my_data <- "path_to_data"
setwd(path_to_my_data)

#install.packages("tree")
library(tree)


## check the data
morpho.data <- read.csv("morpho.example.DT.csv", header=T, sep=";")
head(morpho.data) ## check if the matrix is correctly loaded
morpho.data<-na.omit(morpho.data) 

col_info <- 3 ## number of columns which are NOT morphometric data

names(morpho.data) ## check variables names
unique(morpho.data$SP) ## check number of species


## generate the formula
vars <- paste(names(morpho.data[(col_info+1):ncol(morpho.data)]), collapse="+")
vars
formula <- paste("SP", vars, sep="~") ## SP is the name of the column of species (dependent variable)


## split the dataset into training/test sets
percent     <- 0.6 # percentage of training set
Training   <- sample(nrow(morpho.data), 
                     percent * nrow(morpho.data))
TrainSet <- morpho.data[Training,]
TestSet <- morpho.data[-Training,]

## generate the tree with the training data 
tree.train <- tree(formula, data = TrainSet)

## check the model with the test data
pred <- predict(tree.train, TestSet) # probability of belonging to a class

## assign to the class that shows the highest probability
AssignToAClass <- function(x) {
  return(which(x == max(x)))
}

ClassResult <- apply(pred, 1, AssignToAClass) ## by rows

## calculate the misclassification rate
CalculateMCR <- function(y) {
  MCR.vector<-as.numeric(y) - as.numeric(TestSet[,2]) ## column 2: information about the species, changee if necessary
  misclassified.cases <- length(MCR.vector[MCR.vector != 0])
  
  print(paste("total number of cases in the test set",nrow(TestSet), sep= " = "))
  print(paste("number of misclassified cases",misclassified.cases, sep= " = "))
  result <- round(misclassified.cases/nrow(TestSet),3) ## number of decimals
  return(paste("MCR = ", result))
}


CalculateMCR(ClassResult)
## come back to line 25 and repeat any times needed



## GENERATES A DECISION TREE WITH ALL THE CASES 
###############################################

## load data and libraries
library(tree)
setwd(path_to_my_data)

morpho.data <- read.csv("morpho.example.DT.csv", header=T, sep=";")
col_info <- 3 ## number of columns which are NOT morphometric data

names(morpho.data) ## check variables names
unique(morpho.data$SP) ## check number of species

## create the formula
vars <- paste(names(morpho.data[(col_info+1):ncol(morpho.data)]), collapse="+")
vars
formula <- paste("SP", vars, sep="~") ## SP is the name of the column of species (dependent variable)

## create the PERFECT TREE
DT <- tree(formula, data=morpho.data, mindev = 0, minsize = 2)
summary(DT)

## save the perfect tree into a pdf document
pdf("PerfectT.pdf", width=12, height=12)
plot(DT)
text(DT, cex=.9, srt=90, adj=c(0,1))
dev.off()


## PRUNED TREE 

num_final_nodes <- 4 ## establish the number of final nodes desired

DT_final <- prune.tree(DT, best = num_final_nodes)
summary(DT_final) ## obtain the MCR

## save the pruned tree into a pdf document
pdf("PruneT.pdf", width=12, height=12)
plot(DT_final)
text(DT_final, cex=.9, adj=c(0,1) )
dev.off()

## END