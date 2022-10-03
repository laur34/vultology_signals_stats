# Plot signal clusters from 32 people in new (2022) database

library(ggbiplot)
library(plotly)
library(ggfortify)


setwd("/home/laur/vultology_signals_pcs/new_db") #Change accordingly

data <- read.csv("master_tally_sheet_2_one_per_person_axes_only.csv", header=T, sep="\t")
dataw <- reshape(data, idvar="SubjectNr", timevar = "SignalCode", direction="wide")

data <- data[,c(1,2,4)]
dataw <- reshape(data, idvar="SubjectNr", timevar = "SignalCode", direction="wide")

my_names <- dataw$SubjectNr
dw <- dataw[,-1]
row.names(dw) = my_names

#newcolnames <- names(dw)
#names(dw) <- newcolnames

prcomp(dw)
pca_res <- prcomp(dw, scale.=TRUE)
p <- autoplot(pca_res)
ggplotly(p) #Not really sure how that looks, tbh.

# Transpose the data frame, so it will work more like the iris data
#(from example here: https://plotly.com/ggplot2/pca-visualization/)
tdw <- t(dw)
variety <- c(rep("candid",9), rep("grounded",8), rep("measured",9), rep("suspended",8) )

tdwdata <- cbind.data.frame(tdw, variety)

pca_res <- prcomp(tdw, scale. = TRUE)
p <- autoplot(pca_res, data=tdwdata, colour='variety', label=TRUE, shape=TRUE,
              loadings=TRUE, loadings.colour='mediumslateblue', loadings.label=TRUE)

p <- p + labs(title="Principal Component Analysis of Signals observed in Individuals")

ggplotly(p)

#save.image(filename="PCA_32_real_subjects_axis_fxns_only", type="png")
