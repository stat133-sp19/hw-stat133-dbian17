
#Private Function check_prob() used to check if p is a valid probability
check_prob <- function(prob) {
  if (prob >= 0 && prob <= 1){
    return (TRUE)
  }
  else{
    stop("Invalid value for prob")
  }
}

#Private Function that checks if the number of trials is a valid input <- 
check_trials <- function(trials){
  if (trials > -1 && (trials == round(trials))){
    return (TRUE)  
  }
  else{
    stop("Invalid value for trials")
  }
}

#Private Function that checks if the number of successes is a valid input
check_success <- function(success, trials){
  if(any(success < 0) || any(success > trials) || trials != round(trials)){
    stop("Invalid success value")
  }
  else{
    return(TRUE)
  }
}

#Private Function that finds the mean
aux_mean <- function(trials, prob){
  return(trials*prob)
}

#Private Function that finds the variance
aux_variance <- function(trials, prob){
  return(trials*prob*(1-prob))
}

#Private Function that finds the mode
aux_mode <- function(trials, prob){
  m <- trials*prob + prob
  
  if(m == round(m)){
    return(c(m, m-1))
  }
  
  else{
    m <- floor(m)
    return (m)
  }
}

#Private Function that finds the skewness
aux_skewness <- function(trials, prob){
  return((1-2*prob)/(sqrt(aux_variance(trials, prob))))
}

#Private Function that finds Kurtosis
aux_kurtosis <- function(trials, prob){
  return((1 - 6*prob*(1-prob))/aux_variance(trials,prob))
}

#' @title bin_choose
#' @description calculates n choose k
#' @param n number of trials
#' @param k number of successes
#' @return the number of combinations in which k successes can occur in n trials
#' @export
#' @examples
#' # Calculates the number n choose k for n = 7 and k =0,1,2,3
#' bin_choose(n = 7, k = 0:3)
#'

bin_choose <- function(n, k){
  if (any(k > n)){
    stop("k cannot be greater than n")
  }
  else{
    return((factorial(n)/(factorial(k)*factorial(n-k))))
  }
}

#' @title bin_probability
#' @description calculates the probability of k successes in n trials with probability prob of success
#' @param n number of trials
#' @param k number of successes
#' @param prob probility of success in each trial
#' @return the probability of k successes in n trials with probability prob of success in each trial
#' @export
#' @examples
#' # Calculates probability of 4 successes in 5 trials with probability of success .8
#' bin_probability(success = 4, trials = 5, prob = 0.8)


bin_probability <- function(success, trials, prob){
  check_trials(trials)
  check_prob(prob)
  check_success(success, trials)
  
  return(bin_choose(trials,success)*(prob^success)*(1-prob)^(trials - success))
}

#' @title bin_distribution
#' @description calculates the binomial PMF with n trials and probability prob
#' @param n number of trials
#' @param prob probablity of success
#' @return a dataframe with class "bindis"
#' @export
#' @examples
#' # Calculates the PMF of a binomial distribution of 6 trials and a success probability of .5
#' bin_distribution(trials = 6, prob = 0.5)
#'

bin_distribution <- function(trials, prob){
   distribution <- data.frame(success = 0:trials, probability = bin_probability(0:trials, trials, prob))
   class(distribution) <- c("bindis")
   return (distribution)
}

#' @export
plot.bindis <- function(bindis){
  barplot(bindis$probability, xlab = "Successes", ylab = "Probability", names.arg = c(0:(length(bindis$success)-1)))
}

#' @title bin_cumulative
#' @description calculates the binomial CDF with n trials and probability prob
#' @param n number of trials
#' @param prob probablity of success 
#' @return a dataframe with class "bincum"
#' @export
#' @examples
#' Calculates the PMF of a binomial distribution of 6 trials and a success probability of .5
#' bin_cummulative(trials = 6, prob = 0.5)

bin_cumulative <- function(trials, prob){
  distribution <- data.frame(success = 0:trials, probability = bin_probability(0:trials, trials, prob), cumulative = cumsum(bin_probability(0:trials, trials, prob)))
  class(distribution) <- c("bincum")
  return (distribution)
}

#'@export
plot.bincum <- function(bincum){
  plot(bincum$success,bincum$cumulative, xlab = "Successes", ylab = "Cumulative Probability")
  lines(bincum$success,bincum$cumulative)
}

#' @title bin_variable
#' @description creates a binomial random variable object with attributes "trials" and "prob"
#' @param n number of trials
#' @param prob probablity of success 
#' @return an object of class "binvar"
#' @export
#' @examples
#' # Creates a binomial random variable object with 12 trials and a probability of success for each trial of .1
#' bin1 <- bin_variable(trials = 12, p = 0.1)
bin_variable <- function(trials, prob){
  
  check_trials(trials)
  check_prob(prob)
  
  x <- list(trials = trials,prob = prob)
  class(x) <- "binvar"
  return(x)
}

#'@export
print.binvar <- function(binvar){
  cat("  'Binomial variable'","\n\n" ," Parameters","\n","- number of trials:",binvar$trials,"\n","- prob of success:",binvar$prob)
}


#' @export
summary.binvar <- function(binvar){
  t <- binvar$trials
  p <- binvar$prob
  summ <- list(trials = t, prob = p, mean = aux_mean(t, p), variance = aux_variance(t, p), mode = aux_mode(t, p), skewness = aux_skewness(t, p), kurtosis = aux_kurtosis(t,p))
  class(summ) <- "summary.binvar"
  return (summ)
}

#'@export
print.summary.binvar <- function(summary.binvar){
  cat("  'Binomial variable'","\n\n" ," Parameters","\n","- number of trials:",summary.binvar$trials,"\n","- prob of success:",summary.binvar$prob, "\n\n")
  cat("Measures","\n", "- mean:", summary.binvar$mean, "\n - variance:", summary.binvar$variance, "\n - mode:"
      ,summary.binvar$mode, "\n - skewness:", summary.binvar$skewness, "\n - kurtosis:", summary.binvar$kurtosis)
}

#' @title bin_mean
#' @description finds the mean of a binomial distribution with "trials" and probability "prob"
#' @param n number of trials
#' @prob probablity of success 
#' @return the mean of the given binomial distribution with "trials "trials and probability "prob" of success
#' @export
#'
bin_mean <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  return(aux_mean(trials, prob))
}

#' @title bin_variance
#' @description finds the variance of a binomial distribution with "trials" and probability "prob"
#' @param n number of trials
#' @prob probablity of success 
#' @return the variance of the given binomial distribution with "trials "trials and probability "prob" of success
#' @export
#'
bin_variance <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  return(aux_variance(trials, prob))
}

#' @title bin_mode
#' @description finds the mode of a binomial distribution with "trials" and probability "prob"
#' @param n number of trials
#' @prob probablity of success 
#' @return the mode of the given binomial distribution with "trials "trials and probability "prob" of success
#' @export
#'
bin_mode <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  return(aux_mode(trials, prob))
}

#' @title bin_skewness
#' @description finds the skewness of a binomial distribution with "trials" and probability "prob"
#' @param n number of trials
#' @prob probablity of success 
#' @return the skewness of the given binomial distribution with "trials "trials and probability "prob" of success
#' @export
#'
bin_skewness <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  return(aux_skewness(trials, prob))
}

#' @title bin_kurtosis
#' @description finds the kurtosis of a binomial distribution with "trials" and probability "prob"
#' @param n number of trials
#' @prob probablity of success 
#' @return the kurtosis of the given binomial distribution with "trials "trials and probability "prob" of success
#' @export
#'
bin_kurtosis <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  return(aux_kurtosis(trials, prob))
}







