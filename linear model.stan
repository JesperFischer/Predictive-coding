//This block defines the input that. So these things needs to go into the model
data {
  //Define the number of datapoints this is defined by an integer
  int N;
  //Define a vector called y that is as long as the number of datapoints (i.e. N)
  vector[N] y;
  // same as y but a variable called x
  vector[N] x;
}

// This is the parameters block. Here we define the free parameters which are the onces that STAN estimates for us
parameters {
  real a;
  real b;
  real sigma;
}

// This is the model block here we define how we believe the model is (of cause we simulated the data so we just put in the same thing as we did when simulating)
model {
  y ~ normal(a+b*x, sigma);
}