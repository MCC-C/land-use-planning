library(ggplot2)

#my_initial_pop_zero <- my_initial_pop

####__________ STEP 1: Generate initial population __________#### 

# Create an empty dataframe to fill with the following for loop. 
my_initial_pop <- data.frame(matrix(NA, nrow = 3600, ncol =1))
positions <- c(rep(1,125),rep(2,25),rep(3,0),rep(4,0),rep(5,30),rep(6,15),rep(7,15),rep(8,130),rep(9,18))

size_of_initail_population <- 30

for (n in 1:size_of_initail_population) {  # the size of the population is specified here. 
  my_nth_chromosome <- data.frame()
  positions <- c(rep(1,125),rep(2,25),rep(3,0),rep(4,0),rep(5,30),rep(6,15),rep(7,15),rep(8,130),rep(9,18))
  #44 psitions are fixed. So positions only consist of 400-44 possible positions.
  #Remember that some of the constraints are already satisfied with the fixed land-use types. Therefore, we don't need to select from their 
  # positions. e.g. rep(3,0) and rep(4,0)
  
  for (j in 1:400) {
    
    if (any(as.numeric(j)==32 | as.numeric(j)==33 | as.numeric(j)==34 | as.numeric(j)==53)) {
      fixed1 <- c(0,0,0,0,0,0,1,0,0)
      fixed1 <- as.data.frame(fixed1)
      colnames(fixed1) <- 'x'
      my_nth_chromosome <- rbind(my_nth_chromosome, fixed1)
      my_nth_chromosome <- unname(my_nth_chromosome)
      colnames(my_nth_chromosome) <- 'x'
    }
    
    else if (as.numeric(j)==109 || as.numeric(j)==128 || as.numeric(j)==129 || as.numeric(j)==130) {
      fixed2 <- c(0,0,0,0,0,0,1,0,0)
      fixed2 <- as.data.frame(fixed2)
      colnames(fixed2) <- 'x'
      my_nth_chromosome <- rbind(my_nth_chromosome, fixed2)
      my_nth_chromosome <- unname(my_nth_chromosome)
      colnames(my_nth_chromosome) <- 'x'
    }
    
    else if (as.numeric(j)==145) {
      fixed3 <- c(0,0,0,0,0,1,0,0,0)
      fixed3 <- as.data.frame(fixed3)
      colnames(fixed3) <- 'x'
      my_nth_chromosome <- rbind(my_nth_chromosome, fixed3)
      my_nth_chromosome <- unname(my_nth_chromosome)
      colnames(my_nth_chromosome) <- 'x'
    }
    
    else if (as.numeric(j)==161 || as.numeric(j)==162 || as.numeric(j)==163 || as.numeric(j)==164 || as.numeric(j)==165
             || as.numeric(j)==183 || as.numeric(j)==290 || as.numeric(j)==310 || as.numeric(j)==311 || as.numeric(j)==312
             || as.numeric(j)==328 || as.numeric(j)==329 || as.numeric(j)==330 || as.numeric(j)==331 || as.numeric(j)==332
             || as.numeric(j)==348 || as.numeric(j)==349 || as.numeric(j)==350 || as.numeric(j)==351
             || as.numeric(j)==366 || as.numeric(j)==367 || as.numeric(j)==368 || as.numeric(j)==369 || as.numeric(j)==370 || as.numeric(j)==371 
             || as.numeric(j)==386 || as.numeric(j)==387 || as.numeric(j)==388) {
      fixed4 <- c(0,0,1,0,0,0,0,0,0)
      fixed4 <- as.data.frame(fixed4)
      colnames(fixed4) <- 'x'
      my_nth_chromosome <- rbind(my_nth_chromosome, fixed4)
      my_nth_chromosome <- unname(my_nth_chromosome)
      colnames(my_nth_chromosome) <- 'x'
    }
    
    else if (as.numeric(j)==372 || as.numeric(j)==373 || as.numeric(j)==389 || as.numeric(j)==390 || as.numeric(j)==391 || as.numeric(j)==392 || as.numeric(j)==393) {
      fixed5 <- c(0,0,0,1,0,0,0,0,0)
      fixed5 <- as.data.frame(fixed5)
      colnames(fixed5) <- 'x'
      my_nth_chromosome <- rbind(my_nth_chromosome, fixed5)
      my_nth_chromosome <- unname(my_nth_chromosome)
      colnames(my_nth_chromosome) <- 'x'
    }
    
    else {
      #for (j in 1:400) {
      x <- c(0,0,0,0,0,0,0,0,0) # change one of these values to '1'
      x <- as.data.frame(x)
      #positions <- c(rep(1,90),rep(2,30),rep(3,30),rep(4,10),rep(5,40),rep(6,20),rep(7,20),rep(8,140),rep(9,20))     # these correspond to the positions in variable x
      index <- sample(positions, 1, replace = FALSE)
      x[index,] <- 1        # replace the selected position with a '1'
      colnames(x) <- 'x'
      
      #Remove the selected landuse type from the 'position matrix
      value_to_remove <- sample(which(positions %in% index),1) # The variable value to remove contains the row index
      positions <- positions[-value_to_remove]   # notice the -
      
      # Next we need to append (rowbind) it to the empty dataframe               
      my_nth_chromosome <- rbind(my_nth_chromosome, x)
      my_nth_chromosome <- unname(my_nth_chromosome)
      #my_initial_pop <- rbind(my_initial_pop, x)
      colnames(my_nth_chromosome) <- 'x'
    }
  }
  my_initial_pop <- cbind(my_initial_pop,my_nth_chromosome)
  
}

my_initial_pop <- my_initial_pop[,-1]

sum(my_initial_pop[,2]) # check to see if it sums to 400 - it does


## Give a very good starting solution:
as.data.frame(solt$solution)
my_initial_pop <- cbind(my_initial_pop,as.data.frame(solt$solution))
my_initial_pop <- cbind(my_initial_pop,as.data.frame(solt$solution))
my_initial_pop <- my_initial_pop[,-1]

####__________ STEP 2: Calculate the fitness of all the chromosomes __________####
# The fitness function should quantitatively measure how fit a given solution is in solving the problem.

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
my_obj <- as.matrix(my_obj)

###FITNESS FUNCTION###
# Make the parameter 'final_selected_land_use_types' equal to the initial population when testing.

#ranking = data.frame() # The number of rows correspond to the number of initial chromosomes
FitnessValues = data.frame(matrix(NA, nrow = size_of_initail_population, ncol =1)) # The number of rows correspond to the number of initial chromosome
fitness = function(final_selected_land_use_types){
  # Define a variable that will contain the natural value's value. Start it at zero and add the value when 
  # you find a '1' in the first column of the 'population_and_natural_values' dataframe
  #natural_value <- 0   
  #rownumber = data.frame(matrix(NA, nrow = 10, ncol =1)) # The number of rows correspond to the number of initial chromosome
  for (c in 1:ncol(final_selected_land_use_types)) {
    natural_value <- 0  
    if (   sum(as.data.frame(final_selected_land_use_types[,c])[seq(1, 3600, 9),]) < 80
           || sum(as.data.frame(final_selected_land_use_types[,c])[seq(1, 3600, 9),]) > 150 
           || sum(as.data.frame(final_selected_land_use_types[,c])[seq(2, 3600, 9),]) < 20 
           || sum(as.data.frame(final_selected_land_use_types[,c])[seq(2, 3600, 9),]) > 65 
           || sum(as.data.frame(final_selected_land_use_types[,c])[seq(3, 3600, 9),]) < 20 
           || sum(as.data.frame(final_selected_land_use_types[,c])[seq(3, 3600, 9),]) > 45 
           || sum(as.data.frame(final_selected_land_use_types[,c])[seq(4, 3600, 9),]) < 5     
           || sum(as.data.frame(final_selected_land_use_types[,c])[seq(4, 3600, 9),]) > 15   
           || sum(as.data.frame(final_selected_land_use_types[,c])[seq(5, 3600, 9),]) > 70 
           || sum(as.data.frame(final_selected_land_use_types[,c])[seq(6, 3600, 9),]) > 35 
           || sum(as.data.frame(final_selected_land_use_types[,c])[seq(7, 3600, 9),]) > 30 
           || sum(as.data.frame(final_selected_land_use_types[,c])[seq(8, 3600, 9),]) < 120 
           || sum(as.data.frame(final_selected_land_use_types[,c])[seq(8, 3600, 9),]) > 150 
           || sum(as.data.frame(final_selected_land_use_types[,c])[seq(9, 3600, 9),]) > 60 
    ) {
      natural_value <- -100000000000
      print(natural_value)
      print('falied')
      FitnessValues[c,] <- natural_value
      #rownumber <- rbind(rownumber,natural_value)
    }
    
    else {
      print('Fitness Values')
      population_and_natural_values <- cbind(final_selected_land_use_types[,c], my_obj)
      
      for(r in 1:nrow(population_and_natural_values)) {
        if (population_and_natural_values[r,1] == 1) {
          natural_value <- natural_value + population_and_natural_values[r,2]
        } 
      }
      print(natural_value)
      FitnessValues[c,] <- natural_value
      print(FitnessValues)
      assign("FitnessValues", FitnessValues, envir = .GlobalEnv)
    }
  }
}

best_chromosome_in_population_list3 <- data.frame()   #### Create a dataframe to store the maximum values after each iterations. 

runs <- 32000
for (t in 1:runs) {
  
  fitness(my_initial_pop)  # Run the fitness function
  unname(FitnessValues)
  colnames(FitnessValues) <- "Fitness"
  
  names <- data.frame()
  for(i in 1:size_of_initail_population) {  # it was on 1:10 
    nam <- i 
    names <- rbind(names, nam)
  }
  
  ranking <- cbind(FitnessValues, names)
  colnames(ranking) <- c("Fitness","Chromosomes")
  
  
  ####__________ STEP 3: Create a new population __________####
  
  ####__________ STEP 3.1: Selection __________####
  
  # We want to select two parents from the population. Which will create two children
  # To do this we need to start by ranking the parents by their fitness value. (higher is better). 
  # The ranking wil be kept in a vector. The vector can contain the fitness values or the rank positions itself. Mine will contain the fitness values.
  # swop the vector around (We need to do this for the sample() function)
  
  ranked_chromosomes_by_fitness <- ranking[order(ranking[,1], decreasing = TRUE),]
  
  best_chromosome_in_population <- ranked_chromosomes_by_fitness[1,1]
  best_chromosome_in_population_list3 <- rbind(best_chromosome_in_population_list3, best_chromosome_in_population)
  
  #select twp parents based on fitness and probability
  p <- (c(size_of_initail_population:1))^2
  select_two_parents <- sample(ranked_chromosomes_by_fitness[,2],2,prob = p,replace = FALSE)
  
  parent1 <- my_initial_pop[,select_two_parents[1]]
  #parent1 <- data.frame(matrix(parent1, nrow = 3600, ncol =1))
  parent2 <- my_initial_pop[,select_two_parents[2]]
  #parent2 <- data.frame(matrix(parent2, nrow = 3600, ncol =1))
  
  ####__________ STEP 3.2: Crossover (one-point) __________####
  
  # Select a random crossover point
  crossover_point <- sample(2:399,1)*9
  #crossover_point <- 9
  
  all <- c(1:crossover_point)   # This indicates where the split is made. The last follow is where we split.   
  
  temp_1 <- parent1[all]
  temp_2 <- parent2[all]
  
  ##crossover
  parent2[all] <- temp_1
  parent1[all] <- temp_2
  
  ## Output
  child1 <- parent1
  child2 <- parent2
  
  fitness(as.data.frame(child1))
  
  ####__________ STEP 3.3: Mutation __________####
  
  # In the mutation step our goal is to create small changes (mutations to the children chromosomes)
  # We can either change 1% (4 landuse types) or 0.5% (2 landuse types)
  
  #400*0.01 = 4
  #400*0.005 = 2
  
  # Select one of the following land use types
  
  for (iter in 1:sample(c(2,4),1)) {
    one <- c(0,0,0,0,0,0,0,0,0) # change one of these values to '1'
    index2 <- sample(c(1,2,5,6,7,8,9), 1, replace = FALSE) # Randomly select a landuse type to change to.
    one[index2] <- 1
    
    #We can't mutate any of the fixed landuse types. Therefore they will be ignored for selection in the mutation step.
    blocks <- c(1:30,35:51,55:107,111:126,132:143,147:159,167:181,185:288,292:308,314:326,334:346,353:364,375:384,395:399)       # (400+1)*9 = 3609 is outside of the range   1:398
    random_block_to_change <- sample(blocks,1)
    start <- (random_block_to_change*9)+1
    end <- (random_block_to_change+1)*9   
    
    position_in_child <- c(start:end)
    child1[position_in_child] <- one
    child2[position_in_child] <- one
  }
  
  
  ####__________ STEP 4: Replacement __________####
  
  # Convert the children chromosomes back to a dataframe so that we can place them in the initial population.
  child1 <- data.frame(matrix(child1, nrow = 3600, ncol =1))
  child2 <- data.frame(matrix(child2, nrow = 3600, ncol =1))
  colnames(child1) <- c("child1")
  colnames(child2) <- c("child2")
  
  # We will replace the two poorest chromosomes in the initial population with the two new children. 
  poorest_2_chromosomes_from_initail_pop <- tail(ranked_chromosomes_by_fitness[,2],2)
  
  # Remove the two poorest chromosomes
  my_initial_pop <- my_initial_pop[,-c(poorest_2_chromosomes_from_initail_pop[1],poorest_2_chromosomes_from_initail_pop[2])]
  # Insert the children in the population
  my_initial_pop <- cbind(my_initial_pop, child1, child2)
  
}

# Plot the improvement of the solution over iteration. 

Number_of_iterations <- data.frame()
for(i in 1:runs) {    # runs is defined in line 188
  num <- i 
  Number_of_iterations <- rbind(Number_of_iterations, num)
}

Number_of_iterations <- cbind(Number_of_iterations, best_chromosome_in_population_list3)
colnames(Number_of_iterations) <- c("Iteration", "Cost_of_changing")

# Basic line plot with points
ggplot(data=Number_of_iterations, aes(x=Iteration, y=Cost_of_changing, group=1)) +
  geom_line(color="green")+
  geom_point()


########___ Plot the top solution ___########
Top <- ranked_chromosomes_by_fitness[1,2]   # top solution chromosome

nameOfLandUseType <- rep(c("1 intensive agriculture", "2 extensive agriculture", "3 residence", "4 industry",
                           "5 recreation (day trips)", "6 recreation (overnight)", "7 wet natural area", 
                           "8 water (recreational use)", "9 water (limited use)"), 400)

numberOfLandUseType <- rep(c(1, 2, 3, 4, 5, 6, 7, 8, 9), 400)

FinalAnswerNumerForm <- numberOfLandUseType[my_initial_pop[,Top] == 1]
FinalAnswerNumerForm <- as.data.frame(FinalAnswerNumerForm)

FinalAnswerNumerFormMatrix <-matrix(unlist(FinalAnswerNumerForm), byrow=T, 20, 20)

fixIndex <- t(apply(FinalAnswerNumerFormMatrix, 2, rev))

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
plotMap(fixIndex_for_ggplot_with_index, "Objective 3 - Minimize the cost of changing land use")
