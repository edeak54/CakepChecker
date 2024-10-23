# Install necessary libraries
#install.packages("jpeg")
#install.packages("caret")
#install.packages("randomForest")
#install.packages("shiny")

library('jpeg')
library('caret')
library('randomForest')
#library('shiny')

#load training data
training_data <- read.csv("./training_data.csv")
print(training_data)



#preprocess############################################################

# Check for missing values
summary(training_data)

# imputation 
training_data <- transform(training_data, axisMin = ifelse(is.na(axisMin), mean(axisMin, na.rm = TRUE), axisMin))


# Scaling numeric features to a standard range 
training_data[, c("axisMin")] <- scale(training_data[, c("axisMin")])

# Check if 'category' is a categorical variable
if ("category" %in% colnames(training_data)) {
  training_data <- cbind(training_data, model.matrix(~ category - 1, data = training_data))
  training_data <- training_data[, -which(names(training_data) %in% c("category"))]
}

####################################

#imputation for the target variable
training_data$asymmetryMin <- ifelse(is.na(training_data$asymmetryMin), mean(training_data$asymmetryMin, na.rm = TRUE), training_data$asymmetryMin)

# Select features and target variable
features <- training_data[, c("axisMin")]
target <- training_data$asymmetryMin

# Train the random forest model
model <- randomForest(features, target)

set.seed(123)  # Set seed for reproducibility
train_indices <- createDataPartition(target, p = 0.8, list = FALSE)
train_data <- training_data[train_indices, ]
test_data <- training_data[-train_indices, ]


# Example: Creating a squared feature
training_data$squared_axisMin <- training_data$axisMin^2

# Visualize the distribution of the target variable
hist(target, main = "Distribution of Asymmetry", xlab = "Asymmetry")


#Calculate the asymmetry of a face
function.faceAsymmetry <- function(filename = stop("Please provide a filename"),
                                   model, axisSearch = 0, xmin = NA, xmax = NA, ymin = NA, ymax = NA, saveDirectory = "../History/") {

  
  vector.axis = c()
  vector.asymmetry = c()
  num.axisMin = Inf
  num.asymmetryMin = Inf
  
  ## Read the image
  image.face = readJPEG(filename)
  dim(image.face)
  
  ## Convert the image to grayscale
  image.face = ((image.face[, , 1] + image.face[, , 2] + image.face[, , 3]) / 3)
  
  ## Decide the correct limits for xmin, xmax, ymin, ymax
  if (is.na(xmin) || xmin < 1)
    xmin = 1
  
  if (is.na(xmax) || xmax > dim(image.face)[1])
    xmax = dim(image.face)[1]
  
  if (is.na(ymin) || ymin < 1)
    ymin = 1
  
  if (is.na(ymax) || ymax > dim(image.face)[2])
    ymax = dim(image.face)[2]
  
  # Checks if any element in axisSearch is NA or less than 1
  if (any(is.na(axisSearch)) || any(axisSearch < 1)) {
    axisSearch = 1
  }
  
  ## Find the symmetry line (xmin inclusive)
  symmetry.axis = xmin + ((xmax - (xmin + 1)) / 2)
  
  print(paste("X-Y boundaries:", xmin, xmax, ymin, ymax))
  
  ## Save the x-axis values that will be used in the calculations
  vector.axis = (symmetry.axis - axisSearch):(symmetry.axis + axisSearch)
  
  ## Applying the equation onto the image
  for (axis in vector.axis) {
    
    distance.x <- min(axis - xmin, xmax - axis)
    x.values <- setdiff((axis - distance.x):(axis + distance.x), axis)
    
    result <- 0
    for (x in x.values) {

      slice1 = image.face[axis - distance.x, ymin:ymax]

      slice2 = image.face[axis + distance.x, ymin:ymax]
      
      ## Calculate the mean squared distance
      result <- result + mean((slice1 - slice2)^2)
    }
    
    ## sqrt(result) and normalize by the number of x-slices to get the global mean:
    result <- sqrt(result) / distance.x
    
    ## store result:
    vector.asymmetry <- c(vector.asymmetry, result)
    
    ## Find the axis that has the minimum asymmetry
    if (result < num.asymmetryMin) {
      num.asymmetryMin = result
      num.axisMin = axis
    }
  }
  
  # Create new_data here, after num.asymmetryMin is defined
  new_data <- data.frame(axisMin = c(num.axisMin))  # Include only the required columns
  colnames(new_data) <- colnames(features)  # Ensure columns match the training data
  
  predicted_asymmetry <- predict(model, new_data)
  
  ## Save the processed image
  saveFilename <- paste(saveDirectory, basename(filename), sep = "")
  writeJPEG(image.face, saveFilename)
  
  ## Return the results
  return(list(imageName = filename, axisMin = num.axisMin, asymmetryMin = num.asymmetryMin, predicted_asymmetry = predicted_asymmetry))
}



#function.exportData: Applies all jpeg files to function.faceAsymmetry in a directory and exports to a CSV file.
#directory: The directory containing the images to be search for. Does not go into subdirectories to search.
#axisSearch: The axisSearch to be applied to all jpeg files
function.exportData <- function(directory = stop("Specify a directory to search for images"), model, axisSearch = NA) {
  #DEBUG: Prints all the files in that directory
  #print(list.files(directory))
  
  ##The collection of jpeg files to be applied the function.faceAsymmetry
  list.jpeg <- list.files(directory)[grep(pattern = ".jpg", x = list.files(directory))]
  
  ##The collection of data from all the jpg files
  asymmetry.data = data.frame(imageName = c(), axisMin = c(), asymmetryMin = c())
  
  # Print the amount of jpg files found
  print(paste("Found", length(list.jpeg), "jpg files."))
  
  # DEBUG: Prints the collection of jpeg files to be used
  #print(list.jpeg)
  
  # Checks if the axisSearch is correct
  if (is.na(axisSearch) || axisSearch < 1)
    axisSearch = 1
  
  # Maintain a list of processed image names to avoid duplicates
  processed_images <- vector("character", length(list.jpeg))
  
  # Process all of the images
  for (i in 1:length(list.jpeg)) {
    image.name = paste(directory, list.jpeg[i], sep = "")
    
    #if add history here
    
    # Check if the image has already been processed
    if (!(list.jpeg[i] %in% processed_images)) {
      print(paste(i, ":", image.name), sep = NA)
      image.data = c(function.faceAsymmetry(image.name, model, axisSearch, NA, NA, NA, NA))
      
      # Save the data into a data.frame format
      result.data <- data.frame(imageName = c(image.data$imageName), axisMin = c(image.data$axisMin), asymmetryMin = c(image.data$asymmetryMin) )
      
      # Combine result.data with asymmetry.data
      asymmetry.data <- rbind(asymmetry.data, result.data)
      
      # Add the processed image to the list
      processed_images[i] <- list.jpeg[i]
    }
  }
  
  # frame.asymmetry <- cbind(asymmetry.data)
  # print(frame.asymmetry)
  
  # Show the data collected from the images
  sorted_asymmetry.data <- asymmetry.data[order(asymmetry.data$asymmetryMin, decreasing = FALSE), ]
  #print(sorted_asymmetry.data)
  print(asymmetry.data)
  
  # Check if the file exists
  if (file.exists("History_checked.csv")) {
    # If the file exists, read the existing data
    existing_data <- read.csv("History_checked.csv")
    
    # Append new data to the existing data
    combined_data <- rbind(existing_data, asymmetry.data)
    
    # Remove duplicates based on imageName
    combined_data <- combined_data[!duplicated(combined_data$imageName), ]
    
    # Write the combined data to the CSV file
    write.csv(combined_data, "History_checked.csv", row.names = FALSE, eol = "\n")
    combined_data <- combined_data[order(combined_data$asymmetryMin), ]  # Sort by asymmetryMin
  } else {
    # If the file doesn't exist, write the new data directly
    write.csv(asymmetry.data, "History_checked.csv", row.names = FALSE, eol = "\n")
  }
  
}


# Get predictions on the training data
predicted_train <- predict(model, features)

#squared feature
#training_data$squared_axisMin <- training_data$axisMin^2

# Visualize the distribution of the target variable
#hist(target, main = "Distribution of Asymmetry", xlab = "Asymmetry")

# Calculate evaluation metrics
mse <- mean((predicted_train - target)^2)
mae <- mean(abs(predicted_train - target))


# Plotting Actual vs Predicted
plot(target, predicted_train, main = "Actual vs Predicted", xlab = "Actual Asymmetry", ylab = "Predicted Asymmetry")

abline(a = 0, b = 1, col = "red")

# Display the first few rows of the analysis data
analysis_data <- data.frame("Actual(Dataset)" = target, "Predicted(Program)" = predicted_train)
print(analysis_data)


cat("Mean Squared Error (MSE):", mse, "\n")
cat("Mean Absolute Error (MAE):", mae, "\n\n\n\n")

function.exportData("./Face/", model, axisSearch = 10)

