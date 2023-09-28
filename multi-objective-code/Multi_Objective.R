# Dependencies 
library(Rglpk)
library(readxl)


#####_____Obj fuction nature_____#####
# Import the spatial varying values for the objective function
load("D:/UCT 2018/Simulation and Optimization/Optimization/Project/spatialDSS_inputs3.RData")

# Create a 400*1 matrix for each land-use type and then column bind the 9 different matrices.
landUseType1 <- c(rep(4,400))
landUseType1 <- matrix(unlist(t(landUseType1)), byrow=T, 400, 1)
landUseType2 <- fig_3a   # Space varying
landUseType2 <- matrix(unlist(t(landUseType2)), byrow=T, 400, 1)
landUseType3 <- c(rep(3,400))
landUseType3 <- matrix(unlist(t(landUseType3)), byrow=T, 400, 1)
landUseType4 <- c(rep(1,400))
landUseType4 <- matrix(unlist(t(landUseType4)), byrow=T, 400, 1)
landUseType5 <- c(rep(5,400))
landUseType5 <- matrix(unlist(t(landUseType5)), byrow=T, 400, 1)
landUseType6 <- c(rep(5,400))
landUseType6 <- matrix(unlist(t(landUseType6)), byrow=T, 400, 1)
landUseType7 <- fig_3a   # Space varying
landUseType7 <- matrix(unlist(t(landUseType7)), byrow=T, 400, 1)
landUseType8 <- c(rep(7,400))
landUseType8 <- matrix(unlist(t(landUseType8)), byrow=T, 400, 1)
landUseType9 <- fig_3a   # Space varying 
landUseType9 <- matrix(unlist(t(landUseType9)), byrow=T, 400, 1)

# Col bind the 9 different matrices together

landUseTypeAll <- cbind(as.data.frame(landUseType1),as.data.frame(landUseType2),as.data.frame(landUseType3),
                        as.data.frame(landUseType4),as.data.frame(landUseType5),as.data.frame(landUseType6),
                        as.data.frame(landUseType7),as.data.frame(landUseType8),as.data.frame(landUseType9))

# Create the objective function - 3600*1 vector
my_obj_nature <- matrix(unlist(t(landUseTypeAll)), byrow=T, 3600, 1)



#####_____Obj fuction recreation_____#####
# Create a 400*1 matrix for each land-use type and then column bind the 9 different matrices.
landUseType1 <- c(rep(6,400))
landUseType1 <- matrix(unlist(t(landUseType1)), byrow=T, 400, 1)
landUseType2 <- fig_3a   # Space varying
landUseType2 <- matrix(unlist(t(landUseType2)), byrow=T, 400, 1)
landUseType3 <- c(rep(3,400))
landUseType3 <- matrix(unlist(t(landUseType3)), byrow=T, 400, 1)
landUseType4 <- c(rep(1,400))
landUseType4 <- matrix(unlist(t(landUseType4)), byrow=T, 400, 1)
landUseType5 <- fig_3b   # Space varying 
landUseType5 <- matrix(unlist(t(landUseType5)), byrow=T, 400, 1)
landUseType6 <- fig_3c   # Space varying 
landUseType6 <- matrix(unlist(t(landUseType6)), byrow=T, 400, 1)
landUseType7 <- c(rep(7,400))
landUseType7 <- matrix(unlist(t(landUseType7)), byrow=T, 400, 1)
landUseType8 <- fig_3b   # Space varying   
landUseType8 <- matrix(unlist(t(landUseType8)), byrow=T, 400, 1)
landUseType9 <- c(rep(1,400))   
landUseType9 <- matrix(unlist(t(landUseType9)), byrow=T, 400, 1)

# Col bind the 9 different matrices together

landUseTypeAll <- cbind(as.data.frame(landUseType1),as.data.frame(landUseType2),as.data.frame(landUseType3),
                        as.data.frame(landUseType4),as.data.frame(landUseType5),as.data.frame(landUseType6),
                        as.data.frame(landUseType7),as.data.frame(landUseType8),as.data.frame(landUseType9))

# Create the objective function - 3600*1 vector
my_obj_recreation <- matrix(unlist(t(landUseTypeAll)), byrow=T, 3600, 1)

#####_____Obj fuction cost_____#####
# import the original landuse types with the correct indexing
figure2C_new <- read_excel("D:/UCT 2018/Simulation and Optimization/Optimization/Project/Figure2C.xlsx")

# Import Table2 - The positive and negative revenues of land type conversion.
CostOfChanging <- read_excel("D:/UCT 2018/Simulation and Optimization/Optimization/Project/CostOfChanging.xlsx")
#CostOfChanging <- matrix(unlist(constraintsFixed), byrow=T, 1, 3600)
matrix(unlist(CostOfChanging[1,]), byrow=T, 9, 1)

# tranform figure2C into a 400*1 matrix 
#figure2C_400_1_matrix <- matrix(unlist(figure2C), byrow=T, 400, 1)  #The indexing was incorrect in this matrix
figure2C_400_1_matrix <- as.matrix(figure2C_new)

# Create an empty dataframe to fill with the following for loop. The dataframe will became the objective function
my_obj <- data.frame()
# rbind to this dataframe based on land use type 


# Create the objective function
for (row in 1:nrow(figure2C_400_1_matrix)) { # Row refers to the row index, not the value in the row
  if(figure2C_400_1_matrix[row,] == 1) {
    my_obj <- rbind(my_obj, matrix(unlist(CostOfChanging[1,]), byrow=T, 9, 1)) # cost of changing from type 1
  }
  else if (figure2C_400_1_matrix[row,] == 2) {
    my_obj <- rbind(my_obj, matrix(unlist(CostOfChanging[2,]), byrow=T, 9, 1)) # cost of changinf from type 2
  } 
  else if (figure2C_400_1_matrix[row,] == 3) {
    my_obj <- rbind(my_obj, matrix(unlist(CostOfChanging[3,]), byrow=T, 9, 1)) # cost of changinf from type 3
  } 
  else if (figure2C_400_1_matrix[row,] == 4) {
    my_obj <- rbind(my_obj, matrix(unlist(CostOfChanging[4,]), byrow=T, 9, 1)) # cost of changinf from type 4
  }
  else if (figure2C_400_1_matrix[row,] == 5) {
    my_obj <- rbind(my_obj, matrix(unlist(CostOfChanging[5,]), byrow=T, 9, 1)) # cost of changinf from type 5
  }
  else if (figure2C_400_1_matrix[row,] == 6) {
    my_obj <- rbind(my_obj, matrix(unlist(CostOfChanging[6,]), byrow=T, 9, 1)) # cost of changinf from type 6
  }
  else if (figure2C_400_1_matrix[row,] == 7) {
    my_obj <- rbind(my_obj, matrix(unlist(CostOfChanging[7,]), byrow=T, 9, 1)) # cost of changinf from type 7
  }
  else if (figure2C_400_1_matrix[row,] == 8) {
    my_obj <- rbind(my_obj, matrix(unlist(CostOfChanging[8,]), byrow=T, 9, 1)) # cost of changinf from type 8
  }
  else {
    my_obj <- rbind(my_obj, matrix(unlist(CostOfChanging[9,]), byrow=T, 9, 1)) # cost of changinf from type 9
  }
}

# Convert the my_obj dataframe to a matrix
my_obj_cost_of_changing <- as.matrix(my_obj)


###_____ PAYOFF MATRIX _____##

payoffMatrix <- data.frame(matrix(0, nrow = 4, ncol = 3))
payoffMatrix[1,1] <- solt1$optimum 
payoffMatrix[2,2] <- solt2$optimum 
payoffMatrix[3,3] <- solt3$optimum

payoffMatrix[2,1] <- sum(solt1$solution*my_obj_recreation) 
payoffMatrix[3,1] <- sum(solt1$solution*my_obj_cost_of_changing)
payoffMatrix[1,2] <- sum(solt2$solution*my_obj_nature)
payoffMatrix[3,2] <- sum(solt2$solution*my_obj_cost_of_changing)
payoffMatrix[1,3] <- sum(solt3$solution*my_obj_nature)
payoffMatrix[2,3] <- sum(solt3$solution*my_obj_recreation)
colnames(payoffMatrix) <- c("Nature","Recreation","Cost")
rownames(payoffMatrix) <- c("Nature","Recreation","Cost","Range")

#calculate the range
payoffMatrix[4,1] <- max(payoffMatrix[1:3,1]) - min(payoffMatrix[1:3,1])
payoffMatrix[4,2] <- max(payoffMatrix[1:3,2]) - min(payoffMatrix[1:3,2])
payoffMatrix[4,3] <- max(payoffMatrix[1:3,3]) - min(payoffMatrix[1:3,3])

###_________________________##

###_____ Calculate W_i _____##

# W_i = 1/range
W1 <- 1/payoffMatrix[4,1]
W2 <- 1/payoffMatrix[4,2]
W3 <- 1/payoffMatrix[4,3]

###_________________________##


# Create the rows for sparse matrix of constraints
# The constraints below represent the upper and lower limits for the land-use types.
landUseType1_constraintBottomLimit <- c(rep(c(1,0,0,0,0,0,0,0,0),400))
landUseType1_constraintBottomLimit <- matrix(unlist(landUseType1_constraintBottomLimit), byrow=T, 1, 3600)
landUseType1_constraintTopLimit <- c(rep(c(1,0,0,0,0,0,0,0,0),400))
landUseType1_constraintTopLimit <- matrix(unlist(landUseType1_constraintTopLimit), byrow=T, 1, 3600)

landUseType2_constraintBottomLimit <- c(rep(c(0,1,0,0,0,0,0,0,0),400))
landUseType2_constraintBottomLimit <- matrix(unlist(landUseType2_constraintBottomLimit), byrow=T, 1, 3600)
landUseType2_constraintTopLimit <- c(rep(c(0,1,0,0,0,0,0,0,0),400))
landUseType2_constraintTopLimit <- matrix(unlist(landUseType2_constraintTopLimit), byrow=T, 1, 3600)

landUseType3_constraintBottomLimit <- c(rep(c(0,0,1,0,0,0,0,0,0),400))
landUseType3_constraintBottomLimit <- matrix(unlist(landUseType3_constraintBottomLimit), byrow=T, 1, 3600)
landUseType3_constraintTopLimit <- c(rep(c(0,0,1,0,0,0,0,0,0),400))
landUseType3_constraintTopLimit <- matrix(unlist(landUseType3_constraintTopLimit), byrow=T, 1, 3600)

landUseType4_constraintBottomLimit <- c(rep(c(0,0,0,1,0,0,0,0,0),400))
landUseType4_constraintBottomLimit <- matrix(unlist(landUseType4_constraintBottomLimit), byrow=T, 1, 3600)
landUseType4_constraintTopLimit <- c(rep(c(0,0,0,1,0,0,0,0,0),400))
landUseType4_constraintTopLimit <- matrix(unlist(landUseType4_constraintTopLimit), byrow=T, 1, 3600)

landUseType5_constraintBottomLimit <- c(rep(c(0,0,0,0,1,0,0,0,0),400))
landUseType5_constraintBottomLimit <- matrix(unlist(landUseType5_constraintBottomLimit), byrow=T, 1, 3600)
landUseType5_constraintTopLimit <- c(rep(c(0,0,0,0,1,0,0,0,0),400))
landUseType5_constraintTopLimit <- matrix(unlist(landUseType5_constraintTopLimit), byrow=T, 1, 3600)

landUseType6_constraintBottomLimit <- c(rep(c(0,0,0,0,0,1,0,0,0),400))
landUseType6_constraintBottomLimit <- matrix(unlist(landUseType6_constraintBottomLimit), byrow=T, 1, 3600)
landUseType6_constraintTopLimit <- c(rep(c(0,0,0,0,0,1,0,0,0),400))
landUseType6_constraintTopLimit <- matrix(unlist(landUseType6_constraintTopLimit), byrow=T, 1, 3600)

landUseType7_constraintBottomLimit <- c(rep(c(0,0,0,0,0,0,1,0,0),400))
landUseType7_constraintBottomLimit <- matrix(unlist(landUseType7_constraintBottomLimit), byrow=T, 1, 3600)
landUseType7_constraintTopLimit <- c(rep(c(0,0,0,0,0,0,1,0,0),400))
landUseType7_constraintTopLimit <- matrix(unlist(landUseType7_constraintTopLimit), byrow=T, 1, 3600)

landUseType8_constraintBottomLimit <- c(rep(c(0,0,0,0,0,0,0,1,0),400))
landUseType8_constraintBottomLimit <- matrix(unlist(landUseType8_constraintBottomLimit), byrow=T, 1, 3600)
landUseType8_constraintTopLimit <- c(rep(c(0,0,0,0,0,0,0,1,0),400))
landUseType8_constraintTopLimit <- matrix(unlist(landUseType8_constraintTopLimit), byrow=T, 1, 3600)

landUseType9_constraintBottomLimit <- c(rep(c(0,0,0,0,0,0,0,0,1),400))
landUseType9_constraintBottomLimit <- matrix(unlist(landUseType9_constraintBottomLimit), byrow=T, 1, 3600)
landUseType9_constraintTopLimit <- c(rep(c(0,0,0,0,0,0,0,0,1),400))
landUseType9_constraintTopLimit <- matrix(unlist(landUseType9_constraintTopLimit), byrow=T, 1, 3600)

# Combine all the land-use types in a single dataframe
landUseType1_constraint_all <- rbind(landUseType1_constraintBottomLimit,
                                     landUseType1_constraintTopLimit,
                                     landUseType2_constraintBottomLimit,
                                     landUseType2_constraintTopLimit,
                                     landUseType3_constraintBottomLimit,
                                     landUseType3_constraintTopLimit,
                                     landUseType4_constraintBottomLimit,
                                     landUseType4_constraintTopLimit,
                                     landUseType5_constraintBottomLimit,
                                     landUseType5_constraintTopLimit,
                                     landUseType6_constraintBottomLimit,
                                     landUseType6_constraintTopLimit,
                                     landUseType7_constraintBottomLimit,
                                     landUseType7_constraintTopLimit,
                                     landUseType8_constraintBottomLimit,
                                     landUseType8_constraintTopLimit,
                                     landUseType9_constraintBottomLimit,
                                     landUseType9_constraintTopLimit)

# Create a constraint that forces you to select one landuse type per block.
# Start by creating a 400 by 3600 matrix filled with zeros, then wrap as.dataframe() around it.
sparseConstraintMatrix <- matrix(0, nrow = 400, ncol = 3600) 
sparseConstraintMatrix <- data.frame(sparseConstraintMatrix)

i <- 0 # I want to add one to the row number after each loop
j <- 0 # I want to add 9 to the column index values after each loop

for(x in 1:400) {
  row <- 1 + i
  colStart <- 1 + j
  colEnd <- 9 + j
  sparseConstraintMatrix[row,colStart:colEnd] <- 1
  i <- i + 1
  j <- j + 9
}

colnames(landUseType1_constraint_all) <- colnames(sparseConstraintMatrix) 
constraints_Final <- rbind(landUseType1_constraint_all,sparseConstraintMatrix)

# Fixed constraints
library(readxl)
constraintsFixed <- read_excel("D:/UCT 2018/Simulation and Optimization/Optimization/Project/Fixed_land_use_types_positions_test.xlsx")
constraintsFixed <- matrix(unlist(constraintsFixed), byrow=T, 1, 3600)

# rbind to the old constraints_Final matrics
colnames(constraintsFixed) <- colnames(constraints_Final)
constraints_Final <- rbind(constraints_Final,constraintsFixed)


#________________________ ADD NEW CONSTRAINTS FOR MULTIPLE OBJECTIVES ________________________#



## For Rows 3601, 3602, and 3603:

# the col represent d1+, d1-, d2+, d2-, d3+, d3-
Zn <- data.frame(matrix(0, nrow = 3, ncol =6))
Zn[1,1] <- 1 
Zn[2,3] <- 1 
Zn[3,5] <- 1
Zn[1,2] <- -1 
Zn[2,4] <- -1 
Zn[3,6] <- -1


# add 6 new rows to 419 by 3600 matrix
constraints_Final <- unname(constraints_Final)

my_obj_nature <- t(my_obj_nature)
my_obj_nature <- as.numeric(my_obj_nature)       # Need to convert it to numeric, otherwise R bind will give an error. 
constraints_Final <- rbind(constraints_Final,my_obj_nature)

my_obj_recreation <- t(my_obj_recreation)
my_obj_recreation <- as.numeric(my_obj_recreation)       # Need to convert it to numeric,
constraints_Final <- rbind(constraints_Final,my_obj_recreation)

my_obj_cost_of_changing <- t(my_obj_cost_of_changing)
my_obj_cost_of_changing <- as.numeric(my_obj_cost_of_changing)  # Need to convert it to numeric,
constraints_Final <- rbind(constraints_Final,my_obj_cost_of_changing)

#Create three 1 by 3600 vectors of 0's for the Wi's 
vector_of_zeros <- as.numeric(rep(0,3600))

constraints_Final <- rbind(constraints_Final,vector_of_zeros)
constraints_Final <- rbind(constraints_Final,vector_of_zeros)
constraints_Final <- rbind(constraints_Final,vector_of_zeros)

#_____________________________________________________________________________________________#






#________________________ ADD 7 NEW COLUMNS FOR Zi AND Wi ________________________#

# Each column needs to be 424 rows long
col3601 <- data.frame(matrix(0, nrow = 425, ncol = 1)) #Must contain 1 and W1
col3601[420,1] <- 1
col3601[423,1] <- W1

col3602 <- data.frame(matrix(0, nrow = 425, ncol = 1)) #Must contain -1 and W1
col3602[420,1] <- -1
col3602[423,1] <- W1

col3603 <- data.frame(matrix(0, nrow = 425, ncol = 1)) #Must contain 1 and W2
col3603[421,1] <- 1
col3603[424,1] <- W2

col3604 <- data.frame(matrix(0, nrow = 425, ncol = 1)) #Must contain -1 and W2
col3604[421,1] <- -1
col3604[424,1] <- W2

col3605 <- data.frame(matrix(0, nrow = 425, ncol = 1)) #Must contain 1 and W3
col3605[422,1] <- 1
col3605[425,1] <- W3

col3606 <- data.frame(matrix(0, nrow = 425, ncol = 1)) #Must contain -1 and W3
col3606[422,1] <- -1
col3606[425,1] <- W3

col3607 <- data.frame(matrix(0, nrow = 425, ncol = 1)) #Must contain -1, -1 and -1
col3607[423,1] <- -1
col3607[424,1] <- -1
col3607[425,1] <- -1

# Col bind this to the constraint matrix. 
constraints_Final <- cbind(constraints_Final,col3601,col3602,col3603,col3604,col3605,col3606,col3607)
#_________________________________________________________________#




#__________ New obj funciton __________#

D <- max((W1),(W2),(W3))
E <- 0.02

####
my_obj_multi <- rep(0, 3607)
my_obj_multi[3607] <- 1
my_obj_multi[3606] <- (D + (E * (W3)))
my_obj_multi[3605] <- (D + (E * (W3)))
my_obj_multi[3604] <- (D + (E * (W2)))
my_obj_multi[3603] <- (D + (E * (W2)))
my_obj_multi[3602] <- (D + (E * (W1)))
my_obj_multi[3601] <- (D + (E * (W1)))

#______________________________________#






#constraints_Final[1:3,1:20]

# Fixed constraints
library(readxl)
#constraintsFixed <- read_excel("D:/UCT 2018/Simulation and Optimization/Optimization/Project/Fixed_land_use_types_positions_test.xlsx")
#constraintsFixed <- matrix(unlist(constraintsFixed), byrow=T, 1, 3600)

# rbind to the old constraints_Final matrics
#colnames(constraintsFixed) <- colnames(constraints_Final)
#constraints_Final <- rbind(constraints_Final,constraintsFixed)

# signs of the constraints
my_dir <- c(">=", "<=", ">=", "<=", ">=", "<=", ">=", "<=", ">=", "<=",
            ">=", "<=", ">=", "<=", ">=", "<=", ">=", "<=",rep("==",400), "=="
            # New signs for the six new constraints:
            # The old objective funstions now form constraints and need to less than their previous optimal value. 
            , "<=", "<=", "<="
            , "<", "<", "<"
            )

# rhs of the constraints
my_rhs <- c(80, 150, 20, 65, 20, 45, 5, 15, 0, 70, 0, 35, 0, 30, 120,
            150, 0, 60, rep(1,400),44
            # New rhs of constraints
            , 2773, 3115, 11950, 0, 0, 0
            )
#my_types <- rep("B", length(my_obj_multi))
my_types <- c(rep("B", 3600), rep("I",6),"C")

# Execute function
# library(Rglpk)
solt <- Rglpk_solve_LP(obj = my_obj_multi, mat = constraints_Final, dir = my_dir, rhs = my_rhs, types = my_types, max = FALSE)

#_____Display the results_____#

nameOfLandUseType <- rep(c("1 intensive agriculture", "2 extensive agriculture", "3 residence", "4 industry",
                           "5 recreation (day trips)", "6 recreation (overnight)", "7 wet natural area", 
                           "8 water (recreational use)", "9 water (limited use)"), 400)

# Do a quick test
nameOfLandUseType[solt$solution == 1]
sum(solt$solution == 1) # There should be 400 ones/boxes

numberOfLandUseType <- rep(c(1, 2, 3, 4, 5, 6, 7, 8, 9), 400)

FinalAnswerNumerForm <- numberOfLandUseType[solt$solution == 1]
FinalAnswerNumerForm <- as.data.frame(FinalAnswerNumerForm)

FinalAnswerNumerFormMatrix <-matrix(unlist(FinalAnswerNumerForm), byrow=T, 20, 20)

dim(FinalAnswerNumerForm)

# Plot

image(FinalAnswerNumerFormMatrix, col=c("green4","green2","tomato4", "cadetblue4", "yellow", "magenta2", "navajowhite4","cyan1", "deepskyblue2"),
      xaxt='n', yaxt='n', main = "Objective 1 - Maximise the natural value")
grid(nx=20, ny=20, lty=1)

# Fix the indexing for the plot
fixIndex <- t(apply(FinalAnswerNumerFormMatrix, 2, rev))
image(fixIndex, col=c("green4","green2","tomato4", "cadetblue4", "yellow", "magenta2", "navajowhite4","cyan1", "deepskyblue2"),
      xaxt='n', yaxt='n', main = "Objective 1 - Maximise the natural value")
grid(nx=20, ny=20, lty=1)

### plot with ggplot ###
library(ggplot2)

# unlist fixIndex matrix # the cols are apended beneath eachother.
fixIndex_for_ggplot <- matrix(unlist(fixIndex), byrow=T, 400, 1)
col_index <- c(rep(1:20), rep(1:20), rep(1:20), rep(1:20), rep(1:20), rep(1:20), rep(1:20), rep(1:20), rep(1:20), rep(1:20),
               rep(1:20), rep(1:20), rep(1:20), rep(1:20), rep(1:20), rep(1:20), rep(1:20), rep(1:20), rep(1:20), rep(1:20))
col_index <- matrix(unlist(col_index), byrow=T, 400, 1)

row_index <- c(rep(1,20), rep(2,20), rep(3,20), rep(4,20), rep(5,20), rep(6,20), rep(7,20), rep(8,20), rep(9,20), rep(10,20),
               rep(11,20), rep(12,20), rep(13,20), rep(14,20), rep(15,20), rep(16,20), rep(17,20), rep(18,20), rep(19,20), rep(20,20))
row_index <- matrix(unlist(row_index), byrow=T, 400, 1)

fixIndex_for_ggplot_with_index <- cbind(fixIndex_for_ggplot,col_index,row_index)

colnames(fixIndex_for_ggplot_with_index) <- c("LandUseType", "Col_index", "Row_index")
fixIndex_for_ggplot_with_index <- as.data.frame(fixIndex_for_ggplot_with_index)

#I factored the landUseType using:
factor(fixIndex_for_ggplot_with_index$LandUseType, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9))


#Plot Function:

plotMap = function(fixIndex_for_ggplot_with_index, plotTitle) {
  
  ggplot(fixIndex_for_ggplot_with_index, aes(x = Col_index, y = Row_index, 
                                             fill = as.factor(LandUseType))) +
    
    #geom_raster(aes(fill = as.factor(landUseType))) +
    
    geom_tile(colour = "grey") +
    
    #geom_point(aes(size = value)) +
    
    #geom_text(label = fixIndex_for_ggplot_with_index['LandUseType'], size = 2.5, colour = "black") +
    
    scale_fill_manual(labels = c("1" = "Intensive Agriculture",
                                 
                                 "2" = "Extensive Agriculture",
                                 
                                 "3" = "Residence",
                                 
                                 "4" = "Industry",
                                 
                                 "5" = "Recreation (Day Trips)",
                                 
                                 "6" = "Recreation (Overnight)",
                                 
                                 "7" = "Wet Natural Area",
                                 
                                 "8" = "Water (Recreational Use)",
                                 
                                 "9" = "Water (Limited Access)"),
                      
                      values = c("1" = "#00830B", "2" = "#03FD03", "3" = "#7F0105",
                                 
                                 "4" = "#02807D", "5" = "#FFFE00", "6" = "#FE01FF",
                                 
                                 "7" = "#837C06", "8" = "#02FeFF", "9" = "#00ADE8")) +
    
    labs(x = "Columns", y = "Rows", title = plotTitle, fill = "Land-Use Type") #+
  
  #scale_x_continuous(breaks=seq(2, 20, 2)) +
  
  #scale_y_continuous(breaks=seq(2, 20, 2)) 
  
}


#Call function:
plotMap(fixIndex_for_ggplot_with_index, "Multi-Objective")

