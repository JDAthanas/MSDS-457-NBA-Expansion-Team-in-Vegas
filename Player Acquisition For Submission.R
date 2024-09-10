library(lpSolve)  # mathematical programming methods

# read comma-delimited text file to create working data frame
multiple_nba_player_production <- read.csv("(Final) Player Production.csv",
                      stringsAsFactors = FALSE)

# Look at player data only for current team and for players with 20 games or more
nba_singular_player_production <- subset(multiple_nba_player_production, !duplicated(Player, fromLast = TRUE))
nba_player_production <- nba_singular_player_production[nba_singular_player_production$G >= 20, ]


# select working data to retain for the optimization problem
nba_work <- 
  nba_player_production[,c("Player", "Pos", "Age", "PER")]

nba_work$Centers <- ifelse(nba_work$Pos == "C", 1, 0)

# set up arguments needed for solving the knapsack problem
# objective is to maximize player efficiency rating
# objective = nba_work$PER
# direction = "max"


# this is a knapsack problem so we have binary integers only
# int.vec = 1:50
# all.bin = TRUE  

# set up the constraint matrix using data for centers
constraint_matrix <- matrix(nba_work$Centers, nrow = 1)

                               
# solve the knapsack problem
knapsack_object <- 
  lp(const.mat = constraint_matrix,
     objective = nba_work$PER,
     direction = "max",
     const.rhs = c(10),
     const.dir = "<=",
     int.vec = 1:418, all.bin = TRUE)


# examine the structure of resulting list object 
cat("\n\nStructure of Mathematical Programming Object\n") 
print(str(knapsack_object))

# show the solution
cat("\n\nBest Centers\n")
print(nba_work[nba_work$Pos == "C" & as.logical(knapsack_object$solution),])




# NOW look at knap sack problem for point guards
nba_work$PointGuard <- ifelse(nba_work$Pos == "PG", 1, 0)

# set up the constraint matrix using data for point guards
constraint_matrix1 <- matrix(nba_work$PointGuard, nrow = 1)

# solve the knapsack problem
knapsack_object <- 
  lp(const.mat = constraint_matrix1,
     objective = nba_work$PER,
     direction = "max",
     const.rhs = c(10),
     const.dir = "<=",
     int.vec = 1:418, all.bin = TRUE)

# show the solution
cat("\n\nBest PG\n")
print(nba_work[nba_work$Pos == "PG" & as.logical(knapsack_object$solution),])



# NOW look at knap sack problem for shooting guards
nba_work$ShootingGuard <- ifelse(nba_work$Pos == "SG", 1, 0)

# set up the constraint matrix using data for shooting guards
constraint_matrix2 <- matrix(nba_work$ShootingGuard, nrow = 1)

# solve the knapsack problem
knapsack_object <- 
  lp(const.mat = constraint_matrix2,
     objective = nba_work$PER,
     direction = "max",
     const.rhs = c(10),
     const.dir = "<=",
     int.vec = 1:418, all.bin = TRUE)

# show the solution
cat("\n\nBest SG\n")
print(nba_work[nba_work$Pos == "SG" & as.logical(knapsack_object$solution),])



# NOW look at knap sack problem for power forwards
nba_work$PowerForward <- ifelse(nba_work$Pos == "PF", 1, 0)

# set up the constraint matrix using data for power forwards
constraint_matrix3 <- matrix(nba_work$PowerForward, nrow = 1)

# solve the knapsack problem
knapsack_object <- 
  lp(const.mat = constraint_matrix3,
     objective = nba_work$PER,
     direction = "max",
     const.rhs = c(10),
     const.dir = "<=",
     int.vec = 1:418, all.bin = TRUE)

# show the solution
cat("\n\nBest PF\n")
print(nba_work[nba_work$Pos == "PF" & as.logical(knapsack_object$solution),])




# NOW look at knap sack problem for small forwards
nba_work$SmallForward <- ifelse(nba_work$Pos == "SF", 1, 0)

# set up the constraint matrix using data for small forwards
constraint_matrix4 <- matrix(nba_work$SmallForward, nrow = 1)

# solve the knapsack problem
knapsack_object <- 
  lp(const.mat = constraint_matrix4,
     objective = nba_work$PER,
     direction = "max",
     const.rhs = c(10),
     const.dir = "<=",
     int.vec = 1:418, all.bin = TRUE)

# show the solution
cat("\n\nBest SF\n")
print(nba_work[nba_work$Pos == "SF" & as.logical(knapsack_object$solution),])



####### Create knapsack for one of each position under $96M salary cap

player_acquisition <- read.csv("Player Acquisition.csv",
                                           stringsAsFactors = FALSE)

player_acquisition$Centers <- ifelse(player_acquisition$Position == "C", 1, 0)
player_acquisition$PointGuards <- ifelse(player_acquisition$Position == "PG", 1, 0)
player_acquisition$ShootingGuards <- ifelse(player_acquisition$Position == "SG", 1, 0)
player_acquisition$PowerForwards <- ifelse(player_acquisition$Position == "PF", 1, 0)
player_acquisition$SmallForwards <- ifelse(player_acquisition$Position == "SF", 1, 0)

constraint_matrix6 <- as.matrix(rbind(player_acquisition$Centers,
                                      player_acquisition$Centers,
                                      player_acquisition$PointGuards,
                                      player_acquisition$PointGuards,
                                      player_acquisition$ShootingGuards,
                                      player_acquisition$ShootingGuards,
                                      player_acquisition$PowerForwards,
                                      player_acquisition$PowerForwards,
                                      player_acquisition$SmallForwards,
                                      player_acquisition$SmallForwards,
                                     t(rep(1, length = 50)),
                                     player_acquisition$Salary_2023.24))
dimnames(constraint_matrix6) <- 
  list(c("ThreeCenterMax",
         "ThreeCenterMin",
         "ThreePGMax",
         "ThreePGMin",
         "ThreeSGMax",
         "ThreeSGMin",
         "ThreePFMax",
         "ThreePFMin",
         "ThreeSFMax",
         "ThreeSFMin",
         "FifteenPlayerMax",
         "SalaryMax"),
       player_acquisition$Position)

# solve the knapsack problem
knapsack_object <- 
  lp(const.mat = constraint_matrix6,
     objective = player_acquisition$PER,
     direction = "max",
     const.rhs = c(3,3,3,3,3,3,3,3,3,3, 15, 93906000),
     const.dir = c("<=", ">=", "=", "<="),
     int.vec = 1:50, all.bin = TRUE)

# examine the structure of resulting list object 
cat("\n\nStructure of Mathematical Programming Object\n") 
print(str(knapsack_object))

# show the solution
cat("\n\nBest Roster\n")
print(player_acquisition[as.logical(knapsack_object$solution),])
