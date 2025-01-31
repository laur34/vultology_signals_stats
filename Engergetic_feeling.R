# Read in the Energetic part of the Vultology database and get a feeling for the data
# Some questions to explore are, what is the distribution of the energetic signals among people in the database?
# Do these "cluster"?

data <- read.csv("/home/laur/Desktop/vultology_signals_pcs/full_DB/full_DB_export-Energetic_part.csv", header=T, row.names = 1, sep="\t")
data[is.na(data)] <- 0

names(data)
hist(data$R1_Rigid_Posture_Copy)
hist(data$R2_Face_Centric)
hist(data$R3_Punctuated_Motions)
hist(data$R4_Vertical_Movements)
hist(data$R5_Subordinate_Fluidity)

hist(data$F1_Fluid_Posture)
hist(data$F2_Eye_Centric)

hist(data$PR1_Head_Pushes)

hist(data$PF1_Restless_Momentum)

# Question:  How to get sample's "Rigid score" and "Fluid score", which would be sum of R1-5, and sum of F1-5, respectively.
## Use first person as example:
rigid_score_Coer <- sum(data[1,1:5])
fluid_score_Coer <- sum(data[1,6:10])
rigid_fluid_scores <- c(rigid_score_Coer, fluid_score_Coer)
barplot(rigid_fluid_scores)

# So this is one persons rigid vs fluid energetic scores.
# How to visualize ....average difference in the two scores...like, variablity?
# First, maybe make like a percent, like PctRigid. For example, Coer would be negative (or below 1, because they are fluid, i.e. more fluid than Rigid)
# like, pctRigid =  12/(12+18) and then pctFluid = 18/(12+18)
rigid_score_Coer/(rigid_score_Coer+fluid_score_Coer)
fluid_score_Coer/(rigid_score_Coer+fluid_score_Coer)
# Try transposing the df
tdata <- t.data.frame(data)
as.data.frame(t(data))
#library(data.table)
#t_data <- transpose(data)
#rownames(t_data) <- colnames(data)
#colnames(t_data) <- rownames(data)
colSums(tdata[1:5,1:6])
rigid_scores <- colSums(tdata[1:5,])
hist(rigid_scores)
