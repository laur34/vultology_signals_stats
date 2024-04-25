# Create our PCA using the full database export CSV, for greater power.

setwd("/home/laur/Desktop/vultology_signals_pcs/full_DB/")

data <- read.csv("FULL_DB_EXPORT.csv", header=T, sep="\t")
head(data)
data[is.na(data)] <- 0

# Put in long format
library(data.table)
long <- melt(setDT(data), id.vars = "subject_name")


library(stringr)
str_split_fixed(long$variable, "_", 2)
long_all <- cbind.data.frame(long$subject_name, str_split_fixed(long$variable, "_", 2)[,1], str_split_fixed(long$variable, "_", 2)[,2], long$value)
names(long_all) <- c("subject_name", "signal_code", "signal_name", "signal_count")

long_new <- cbind.data.frame(long$subject_name, long$variable, long$value)
mynames <- c("subject_name", "signal", "signal_count")
names(long_new) <- mynames

# Try transposing the data frame, so it will work more like the iris data
#(from example here: https://plotly.com/ggplot2/pca-visualization/)

# Start with long_all
# Reshape into desired wide format, and then transpose
datal <- long_all[,c(1,2,4)]
# reshape
data_wide <- reshape(datal, idvar="subject_name", timevar = "signal_code", direction = "wide")
# make the first column into row names
my_names <- data_wide$subject_name
dw <- data_wide[,-1]
row.names(dw) = my_names
#Shorten column names of df, for plotting
newcolnames <- names(dw)
newcolnames <- sub("signal_count.", "", newcolnames)
names(dw) <- newcolnames

#Subset to only contain signals of concern
dw <- dw[,31:64]

tdw <- t(dw)
variety <- c(rep("candid",9), rep("measured",9), rep("grounded",8), rep("suspended",8))

tdwdata <- cbind.data.frame(tdw, variety)

pca_res <- prcomp(tdw)
p <- autoplot(pca_res, data=tdwdata, colour='variety', label=TRUE, shape=TRUE)
p

#############

ggplotly(p)

#######
