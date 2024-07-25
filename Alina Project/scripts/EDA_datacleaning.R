getwd()

## read in dataset
fileName <- list.files("data")
data <- read.csv(paste0(data",fileName))

head(data)
