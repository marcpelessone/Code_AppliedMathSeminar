# David Lin & Marc Pelessone 



# Running my SGD function on the output of fake data
output = SGD(S_train,y_int_train)
z_sgd = output[[1]]
Z_history_sgd = output[[2]]

# Using the z_sgd output of my SGD, I use it to classify
# the S_train data (an output of fakedata with z_int)
y_train = classify(S_train,z_sgd)
# Checking to see if I classified correctly on TRAINING data
y_train == y_int_train

# A function that outputs the intercept and slope of the line defined
# by the vector z (either z_int or z_sgd)
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
hist_len = length(Z_history_sgd[,1])

# Preparing to store intercepts and slopes of all z vectors in Z_history
b_history = matrix(0,hist_len)
m_history = matrix(0,hist_len)

# Going through Z_history and extracting intercept and slope for each z
for (i in 1:hist_len) {
  params = interc_slope(Z_history_sgd[i,])
  b_history[i] = params$intercept
  m_history[i] = params$slope
}

b_m_history = data.frame(b_history, m_history)

# Plotting training data and the trajector of z in Z_history
qplot(S_train[,1],S_train[,2], col=ifelse(y_int_train<0,2,3), 
      main="Trajectory of Z",
      xlab="x_1", ylab="x_2") + 
  geom_abline(data=b_m_history[1:3,], aes(intercepts=b_history, slope=m_history ),
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
params = interc_slope(z_sgd)
b = params$intercept
m = params$slope

# Plotting the final training data with the z_sgd vector
qplot(S_train[,1],S_train[,2], col=ifelse(y_int_train<0,2,3),
      main="Training Data and Final z_std",
      xlab="x_1", ylab="x_2") + 
  geom_abline(intercept=b, slope=m, color='red')

print("Number of Iterations for Perceptron: ")
print(length(Z_history))

print("Number of Iterations for SGD: ")
print(length(Z_history_sgd))
