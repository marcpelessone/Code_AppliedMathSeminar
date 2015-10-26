# David Lin & Marc Pelessone 

#Inputs
#w:  w[1:d] is the normal vector of a hyperplane, 
#    w[d+1] = -c is the negative offset parameter. 
#n: sample size

#Outputs
#S: n by (d+1) sample matrix with last col 1
#y: vector of the associated class labels

fakedata <- function(w, n){
  
  if(! require(MASS))
  {
    install.packages("MASS")
  }
  if(! require(mvtnorm))
  {
    install.packages("mvtnorm")
  }
  
  require(MASS)
  require(mvtnorm)
  
  # obtain dimension
  d <- length(w)-1
  
  # compute the offset vector and a Basis consisting of w and its nullspace
  offset <- -w[length(w)] * w[1:d] / sum(w[1:d]^2)
  Basis <- cbind(Null(w[1:d]), w[1:d])	 
  
  # Create samples, correct for offset, and extend
  # rmvnorm(n,mean,sigme) ~ generate n samples from N(0,I) distribution
  S <- rmvnorm(n, mean=rep(0,d),sigma = diag(1,d)) %*%  t(Basis) 
  S <- S + matrix(rep(offset,n),n,d,byrow=T)
  S <- cbind(S,1)
  
  # compute the class assignments
  y <- as.vector(sign(S %*% w))
  
  # add corrective factors to points that lie on the hyperplane.
  S[y==0,1:d] <- S[y==0,1:d] + runif(1,-0.5,0.5)*10^(-4)
  y = as.vector(sign(S %*% w))
  return(list(S=S, y=y))
  
} # end function fakedata







# This function classifies datapoints given a n x (d+1)-matrix S of data
# points and a (d+1) x 1-vector normal to a hyperplane

# Input: S, z
# output: n x 1 class label vector y

classify <- function(S,z) {
  
  # Extracting the number of data points and creating y vector of that size
  n = length(S[,1])
  y = c(1:n)
  
  # Going through each point and taking dot product
  for (i in 1:n) {
    dot_product = z %*% S[i,]
    
    # Classifying based on sign of dot product
    if (dot_product < 0) {
      y[i] = -1
    }
    
    else {
      y[i] = 1
    }
  }
  
  return(y)
  
}






# This function computes the perceptron algorithm, which uses
# gradient descent to minimize the perceptron cost function 

# Inputs: A n x (d+1)-matrix S of data points, n x 1 vector y of class labels
# Output: (d+1) x 1 vector normal to hyperplane classfying the data

perceptron <- function(S,y) {
  
  # Extracting necessary lengths and creating z and Z_history matrix
  d = length(S[1,])
  n = length(S[,1])
  # the n+1 is to include the -c vector
  z = matrix(1,1,d)
  Z_history = z
  
  # Defining Perceptron cost function
  cost_fn <- function(z) {
    cost = 0
    # Going through all the data points
    for (i in 1:n) {
      # Calculating dot product b/t z and (1,x_i)
      dot_product = z %*% S[i,]
      
      # Labeling class based on dot product
      if (dot_product < 0) {
        class = -1
      } 
      
      else {
        class = 1
      }
      
      # Adding cost if misclassification 
      if (class != y[i]) {
        cost = cost + abs(dot_product)
      }
    }
    return(cost)
  } 
  
  # Calculating gradient of cost function
  gradientOfCost <- function(z) {
    
    gradient = matrix(0,1,d)
    
    # Going through data points
    for (i in 1:n) {
      
      # Same dot product process as in cost function
      dot_product = z %*% S[i,]
      
      if (dot_product < 0) {
        class = -1
      } 
      
      else {
        class = 1
      }
      
      # If misclassification, updating gradient
      if (class != y[i]) {
        gradient = gradient + (-y[i])*(S[i,])
      }
    }
    return(gradient)
  }
  
  k = 0
  margin = 0
  
  # Applying gradient descent while cost_fn > margin
  while (cost_fn(z) > margin) {
    # Updating iteration count for alpha(k)
    k = k + 1
    alpha_k = 1/k
    # Applying gradient descent
    z = z - alpha_k * gradientOfCost(z)
    
    # Appending each new z vector to Z_history
    Z_history = rbind(Z_history, z)
  }
  
  # Normalizing z vector in the end  
  norm = norm(z,type="2")
  z = z/norm
  
  return(list(z, Z_history[2:k+1,]))
  
}




# This function computes the perceptron algorithm, which uses
# gradient descent to minimize the perceptron cost function 

# Inputs: A n x (d+1)-matrix S of data points, n x 1 vector y of class labels
# Output: (d+1) x 1 vector normal to hyperplane classfying the data

perceptron2 <- function(S,y) {
  
  # Extracting necessary lengths and creating z and Z_history matrix
  d = length(S[1,])
  n = length(S[,1])
  # the n+1 is to include the -c vector
  z = matrix(1,1,d)
  Z_history = z
  
  # Defining Perceptron cost function
  cost_fn <- function(z) {
    cost = 0
    # Going through all the data points
    for (i in 1:n) {
      # Calculating dot product b/t z and (1,x_i)
      dot_product = z %*% S[i,]
      
      # Labeling class based on dot product
      if (dot_product < 0) {
        class = -1
      } 
      
      else {
        class = 1
      }
      
      # Adding cost if misclassification 
      if (class != y[i]) {
        cost = cost + abs(dot_product)
      }
    }
    return(cost)
  } 
  
  # Calculating gradient of cost function
  gradientOfCost <- function(z) {
    
    gradient = matrix(0,1,d)
    
    # Going through data points
    for (i in 1:n) {
      
      # Same dot product process as in cost function
      dot_product = z %*% S[i,]
      
      if (dot_product < 0) {
        class = -1
      } 
      
      else {
        class = 1
      }
      
      # If misclassification, updating gradient
      if (class != y[i]) {
        gradient = gradient + (-y[i])*(S[i,])
      }
    }
    return(gradient)
  }
  
  k = 0
  margin = 0
  
  # Applying gradient descent while cost_fn > margin
  while (cost_fn(z) > margin) {
    # Updating iteration count for alpha(k)
    k = k + 1
    alpha_k = 1/k
    # Applying gradient descent
    if (cost_fn(z) < 2) {
      z = z - alpha_k * gradientOfCost(z)
    }
    else {
      z = z - gradientOfCost(z)
    }
    # Appending each new z vector to Z_history
    Z_history = rbind(Z_history, z)
  }
  
  # Normalizing z vector in the end  
  norm = norm(z,type="2")
  z = z/norm
  
  return(list(z, Z_history[2:k+1,]))
  
}






# This function computes the perceptron algorithm, which uses
# gradient descent to minimize the perceptron cost function 

# Inputs: A n x (d+1)-matrix S of data points, n x 1 vector y of class labels
# Output: (d+1) x 1 vector normal to hyperplane classfying the data

SGD <- function(S,y) {
  
  # Extracting necessary lengths and creating z and Z_history matrix
  d = length(S[1,])
  n = length(S[,1])
  # the n+1 is to include the -c vector
  z = matrix(1,1,d)
  Z_history = z
  # Defining Perceptron cost function
  cost_fn <- function(z) {
    cost = 0
    # Going through all the data points
    for (i in 1:n) {
      # Calculating dot product b/t z and (1,x_i)
      dot_product = z %*% S[i,]
      
      # Labeling class based on dot product
      if (dot_product < 0) {
        class = -1
      } 
      
      else {
        class = 1
      }
      
      # Adding cost if misclassification 
      if (class != y[i]) {
        cost = cost + abs(dot_product)
      }
    }
    return(cost)
  } 
  
  # Calculating gradient of cost function
  gradientOfCost <- function(z) {
    
    # Same dot product process as in cost function
    dot_product = z %*% S[i,]
    
    if (dot_product < 0) {
      class = -1
    } 
    
    else {
      class = 1
    }
    
    # If misclassification, updating gradient
    if (class != y[i]) {
      gradient = (-y[i])*(S[i,])
    }
    else {
      gradient = 0
    }
    
    return(gradient)
  }
  
  k = 0
  margin = 0
  
  # Applying gradient descent while cost_fn > margin
  while (cost_fn(z) > margin) {
    print(cost_fn(z))
    rand_sample = sample(1:n)
    k = k + 1
    for (i in rand_sample) {
      # Updating iteration count for alpha(k)
      kk = k
      alpha_k = 1/kk
      # Applying gradient descent
      z = z - alpha_k * gradientOfCost(z)
    }
    # Appending each new z vector to Z_history
    Z_history = rbind(Z_history, z)
  }
  
  # Normalizing z vector in the end  
  norm = norm(z,type="2")
  z = z/norm
  
  return(list(z, Z_history[2:k+1,]))
  
}