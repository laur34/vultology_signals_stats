# Read in the Energetic part of the Vultology database and get a feeling for the data
# Some questions to explore are, what is the distribution of the energetic signals among people in the database?
# Do these "cluster"?

data <- read.csv("/home/laur/Desktop/vultology_signals_pcs/full_DB/full_DB_export-Energetic_part.csv", header=T, row.names = 1, sep="\t")
# Fill in any blank cells with zeroes
data[is.na(data)] <- 0
# Column names are the people's names
names(data)
# Create a histogram of the scores of the energetic signals.
# First, rigid signals.
hist(data$R1_Rigid_Posture_Copy) #(why does it say "copy" though?)
hist(data$R2_Face_Centric)
hist(data$R3_Punctuated_Motions)
hist(data$R4_Vertical_Movements)
hist(data$R5_Subordinate_Fluidity)
# Now Fluid
hist(data$F1_Fluid_Posture)
hist(data$F2_Eye_Centric)
hist(data$PR1_Head_Pushes) ##wait, this is wrong
hist(data$PF1_Restless_Momentum)
# There are really a lot of 0's in all of the above, more than any other score actually .

# Question:  How to get sample's "Rigid score" and "Fluid score", which would be sum of R1-5, and sum of F1-5, respectively.
## Use first person as example:
rigid_score_Coer <- sum(data[1,1:5])
fluid_score_Coer <- sum(data[1,6:10])
rigid_fluid_scores <- c(rigid_score_Coer, fluid_score_Coer)
df <- data.frame(name=c("rigid_score","fluid_score"), value=rigid_fluid_scores)
barplot(height=df$value, names=df$name, main="Coer de Pirate")

# So this is one persons rigid vs fluid energetic scores.
# How to visualize ....average difference in the two scores...like, variablity?
# First, maybe make like a percent, like PctRigid. For example, Coer would be negative (or below 1, because they are fluid, i.e. more fluid than Rigid)
# like, pctRigid =  12/(12+18) and then pctFluid = 18/(12+18)
rigid_score_Coer/(rigid_score_Coer+fluid_score_Coer)
fluid_score_Coer/(rigid_score_Coer+fluid_score_Coer)
# Try transposing the data frame
tdata <- t.data.frame(data)
as.data.frame(t(data))
#library(data.table)
#t_data <- transpose(data)
#rownames(t_data) <- colnames(data)
#colnames(t_data) <- rownames(data)
colSums(tdata[1:5,1:6])
rigid_scores <- colSums(tdata[1:5,])
hist(rigid_scores)
summary(rigid_scores)
boxplot(rigid_scores)
#
fluid_scores <- colSums(tdata[6:10, ])
#
percent_rigid <- rigid_scores/(rigid_scores + fluid_scores)
percent_fluid <- fluid_scores/(rigid_scores + fluid_scores)
hist(percent_fluid) ### these look good
hist(percent_rigid) ### these look good
summary(percent_rigid)
#I just realized that a lot of entries are blank (no info for the persons)
nz <- tdata[ ,colSums(tdata)!=0]
# Now redo stats for nz (cut of matrix for only non zero-sum people)
rigid_scores <- colSums(nz[1:5,])
hist(rigid_scores)
summary(rigid_scores)
boxplot(rigid_scores) #that looks a lot better
fluid_scores <- colSums(nz[6:10, ])
#
percent_rigid <- rigid_scores/(rigid_scores + fluid_scores)
percent_fluid <- fluid_scores/(rigid_scores + fluid_scores)
hist(percent_fluid) ### these look good
hist(percent_rigid) ### these look good
summary(percent_rigid)
boxplot(fluid_scores)
plot(rigid_scores, type="o")
# Now to color code
plot(percent_rigid, col=ifelse(percent_rigid < 0.5, 'red', 'green'), pch=18)
# yes, that looks like a little "clustering"...
# ...now, for the individual components of rigid and fluid (energetic signals)
row.names(nz)
hist(nz[1, ])
title("R1 Rigid Posture Copy")
hist(nz[2, ])
title(main="R2 Face centric")
nz[1:5,1:4]
nz[6:10,1:4]
#Actually, how about create stacked or paired bar plots for each person for their rigid and fluid scores
expl <- nz[1:10,1:4]
colnames(expl) <- gsub(' ', '.', colnames(expl))

df <- data.frame(person=colnames(expl), energetic=c("Rigid","Fluid"),
                 score=rigid_scores[1:4] ) # but ggplot needs long form df

df2 <- transform(data.frame(expl), Name=row.names(expl))



long_df2 <- reshape(df2, varying=colnames(expl), times=colnames(expl),
                    timevar = "x", v.names="value", direction="long" )

library(ggplot2)
#break down by rigid or fluid categorization of signals
long_df2$category <- rep(c(rep("R", 5), rep("F", 5)), 4)
p <- ggplot(long_df2, aes(fill = category, y=value, x=x)) + geom_bar(position="dodge", stat="identity")
##### Try whole dataset
row.names(data) <- gsub(" ", "_", row.names(data))
data[,-1]
#Add categorical variable for if signals or rigid or fluid
data$RigidScore <- rowSums(data[,1:5])
data$FluidScore <- rowSums(data[,6:10])
#Reshape to long form
reshape(data, direction="long", varying = list(names(data)[31:32]) )
###########
nz.pca <- prcomp(nz)
library(ggfortify)
nz.pca.plot <- autoplot(nz.pca, data=nz)
nz.pca.plot

variety <- c(rep("rigid",5), rep("fluid",5), rep("PF",5), rep("RF",5), rep("PR",5), rep("RR",5) )

nzvar <- cbind.data.frame(nz, variety)

pca_res <- prcomp(nz, scale=TRUE)
p <- autoplot(pca_res, data=nzvar, color='variety', label=TRUE, shape=TRUE)
p
##yes:))