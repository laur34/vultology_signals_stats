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
hist(data$F3_Gliding_Motions)
hist(data$F4_Horizontal_Movements)
# There are really a lot of 0's in all of the above, more than any other score actually .

# Objective:  Get a person's "Rigid score" and "Fluid score", which would be sum of R1-5, and sum of F1-5, respectively.
## Use first person (Coer de Pirate) as example:
rigid_score_Coer <- sum(data[1,1:5])
fluid_score_Coer <- sum(data[1,6:10])
rigid_fluid_scores <- c(rigid_score_Coer, fluid_score_Coer)
df <- data.frame(name=c("rigid_score","fluid_score"), value=rigid_fluid_scores)
barplot(height=df$value, names=df$name, main="Coer de Pirate")

# Visualize trends in tendencies for continuous trait distribution (i.e. bimodality)
# That is, show that persons tend to be either predominant in Rigid signals or in Fluid signals.
# First, make a percent, like PctRigid. For example, Coer would be negative (or below 1, because they are fluid, i.e. more fluid than Rigid)
# like, pctRigid =  12/(12+18) and then pctFluid = 18/(12+18)
rigid_score_Coer/(rigid_score_Coer+fluid_score_Coer)
fluid_score_Coer/(rigid_score_Coer+fluid_score_Coer)
# Transpose the data frame so that signals will be in columns, persons in rows
tdata <- t.data.frame(data)

colSums(tdata[1:5,1:6])
rigid_scores <- colSums(tdata[1:5,])
hist(rigid_scores)
summary(rigid_scores)
boxplot(rigid_scores)
fluid_scores <- colSums(tdata[6:10, ])
percent_rigid <- rigid_scores/(rigid_scores + fluid_scores)
percent_fluid <- fluid_scores/(rigid_scores + fluid_scores)
hist(percent_fluid) ### Clear bimodality here
hist(percent_rigid) ### Clear bimoadality here
summary(percent_rigid)
## The Summary stats and Boxplots clearly show that many entries are blank (no data for many persons in the database)
## Let nz be a cut of the dataframe for only non zero-sum people:
nz <- tdata[ ,colSums(tdata)!=0]

# Now redo stats for nz
rigid_scores <- colSums(nz[1:5,])
hist(rigid_scores)
summary(rigid_scores)
boxplot(rigid_scores) #that looks a lot better
fluid_scores <- colSums(nz[6:10, ])
#
percent_rigid <- rigid_scores/(rigid_scores + fluid_scores)
percent_fluid <- fluid_scores/(rigid_scores + fluid_scores)
hist(percent_fluid) ### still look good
hist(percent_rigid) ### still look good
summary(percent_rigid)
boxplot(fluid_scores)
# Now, to see "clusters", begin by plotting the rigid scores for people in a simple x-y
plot(rigid_scores, type="o")
# Color code to see better
plot(percent_rigid, col=ifelse(percent_rigid < 0.5, 'red', 'green'), pch=18)

# Now that we've seen that signals considered "rigid" and "fluid" do tend to co-occur in individual persons,
# Let's examine these energetic signals individually.
row.names(nz)
hist(nz[1, ], main = "Rigid Posture Copy", xlab="signal score")
hist(nz[2, ], main="R2 Face centric", xlab="signal score")
hist(nz[3, ], main="R3 Punctuated Motions", xlab="signal score")
# As we can see in these histograms, people are scored at either 0, 2, 4, or 7 for these signals (none, lo, med, hi?).
# Now, let's see the co-occurrence patterns of people's scores in each of the signals. 

# Paired bar plots for each person for their rigid and fluid scores might be insightful.
# First for a small sample:
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
p
# Try whole dataset (not done yet) #############
row.names(data) <- gsub(" ", "_", row.names(data))
data[,-1]
#Add categorical variable for if signals or rigid or fluid
data$RigidScore <- rowSums(data[,1:5])
data$FluidScore <- rowSums(data[,6:10])
#Reshape to long form
reshape(data, direction="long", varying = list(names(data)[31:32]) )
###########
# Principal Component Analysis, to explore co-occurrence of signals
nz.pca <- prcomp(nz)
library(ggfortify)
nz.pca.plot <- autoplot(nz.pca, data=nz)
nz.pca.plot

variety <- c(rep("rigid",5), rep("fluid",5), rep("PF",5), rep("RF",5), rep("PR",5), rep("RR",5) )

nzvar <- cbind.data.frame(nz, variety)

pca_res <- prcomp(nz, scale=TRUE)
p <- autoplot(pca_res, data=nzvar, color='variety', label=TRUE, shape=TRUE)
p
# Looks good. 
# TODO: Next do, one with only Rigid and Fluid, and also one with only PR, RR, PF, and RF.