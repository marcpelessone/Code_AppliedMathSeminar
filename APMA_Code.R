# David Lin & Marc Pelessone 

#install.packages("ggplot2")
library(ggplot2)

# Randomly generating the z_initial vector
a = runif(1,-1,1)
b = runif(1,-1,1)
c = runif(1,-1,1)
z_int = c(a,b,c)
norm = norm(z_int,type="2")
z_int = z_int/norm

# Running fakedata on aforementioned z_int and storing output
n = 100
out = fakedata(z_int,n)
S_train = out[[1]]
y_int_train = out[[2]]

# Running my perceptron function on the output of fake data
output = perceptron(S_train,y_int_train)
z_percep = output[[1]]
Z_history = output[[2]]

# Using the z_percep output of my perceptron, I use it to classify
# the S_train data (an output of fakedata with z_int)
y_train = classify(S_train,z_percep)
# Checking to see if I classified correctly on TRAINING data
print("Number classified incorrectly: ")
print(sum(y_train != y_int_train))

# A function that outputs the intercept and slope of the line defined
# by the vector z (either z_int or z_percep)
interc_slope <- function(z) {
  
  # Extracting v_h vector from z = (v_h, -c)
  v_h = matrix(c(z[1],z[2]),2)
  # Calculating eucludian norm of v_h
  magn = norm(v_h, type="2")
  
  # Calculating the constant of shift w = c * v_h
  c = -z[3]/magn^2
  
  # calculating w = c * v_h
  w = c * v_h
  
  # m is slope and b is intercept
  m = -z[1]/z[2]
  b = -m*w[1] + w[2]
  
  return(list(intercept=b,slope=m))
}

# Plotting training data 
qplot(S_train[,1],S_train[,2], col=ifelse(y_int_train<0,3,6), 
      main="Training Data",
      xlab="x_1", ylab="x_2")

# Getting ready to plot Z_history 
hist_len = length(Z_history[,1])

# Preparing to store intercepts and slopes of all z vectors in Z_history
b_history = matrix(0,hist_len)
m_history = matrix(0,hist_len)

# Going through Z_history and extracting intercept and slope for each z
for (i in 1:hist_len) {
  params = interc_slope(Z_history[i,])
  b_history[i] = params$intercept
  m_history[i] = params$slope
}

b_m_history = data.frame(b_history, m_history)

# Plotting training data and the trajector of z in Z_history
qplot(S_train[,1],S_train[,2], col=ifelse(y_int_train<0,2,3), 
      main="Trajectory of Z",
      xlab="x_1", ylab="x_2") + 
  geom_abline(data=b_m_history[1:4,], aes(intercepts=b_history, slope=m_history ),
              linetype="dashed")

b_m_history = b_m_history[seq(1,hist_len-1,2),]

# Plotting training data and the trajector of z in Z_history
qplot(S_train[,1],S_train[,2], col=ifelse(y_int_train<0,2,3), 
      main="Trajectory of Z",
      xlab="x_1", ylab="x_2") + 
  geom_abline(data=b_m_history, aes(intercepts=b_history, slope=m_history ),
              linetype="dashed") +
  geom_abline(intercept=b_history[hist_len], slope=m_history[hist_len],
              size=1.1, color='red')


# Extracting the line given z_std
params = interc_slope(z_percep)
b = params$intercept
m = params$slope

# Plotting the final training data with the z_percep vector
qplot(S_train[,1],S_train[,2], col=ifelse(y_int_train<0,2,3),
      main="Training Data and Final z_std",
      xlab="x_1", ylab="x_2") + 
  geom_abline(intercept=b, slope=m, color='red')





# Case Study
data = read.csv("letter-recognition.csv")
data = data.frame(data)

# Renaming columns for easier access
colnames(data) = c(1:length(data[1,]))

# Extracting data for only two letters so we have 2-classes
data_sub = data[data[1] == "A" | data[1] == "B",]
feature_count = length(data_sub[1,]) - 1

data_sub = data.matrix(data_sub)

# Extracting a subset of data for quicker implementation
labels = data_sub[,1]

# Converting the A & B to 1 and -1 respectively 
for (i in 1:length(labels)) {
  if (labels[i] == 2) {
    labels[i] = -1
  }
}

labels_sub = labels[1:200]

features = data_sub[,2:17]/10
# Appending a column of 1 for the parametrization 
features = cbind(features,1)

features_sub = features[1:200,]
out = perceptron2(features_sub,labels_sub)

my_z = out[[1]]
print(my_z)
my_labels = classify(features, my_z)

# Checking if I classified correctly 
print("Number classified incorrectly: ")
print(sum(my_labels != labels))
print("Total number of data points: ")
print(length(data_sub[,1]))
