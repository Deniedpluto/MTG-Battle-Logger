# This is an implementation of the multielo package from https://github.com/djcunningham0/multielo
# All credit for building the initial implementation goes to djcunningham0

linear_score_function <- function(n) {
# With the linear score function the points awarded scale linearly from first place through last place.
# For example, improving from 2nd to 1st place has the same sized benefit as improving from 5th to 4th place.
# 
# n: number of players
# 
# returns an array of the points to assign to each place (summing to 1)
  # initialize a vector
  v <- vector()
  # for each place (p) give it a weight for winning.
  for(p in 1:(n)) {
    v[[p]] <- (n - p) / (n * (n - 1) / 2)   
  }
  return(v)
} 

exponential_score_function <- function(n, score_base) {
# With an exponential score function with base > 1, more points are awarded to the top finishers and the point distribution is flatter at the bottom. 
# For example, improving from 2nd to 1st place is more valuable than improving from 5th place to 4th place. 
# A larger base value means the ratings will be more weighted towards the top finishers.
# 
# base: base for the exponential score function (> 1)
# returns a that takes parameter n for number of players and returns an array of the points to assign to each place (summing to 1)
  
  if(score_base < 1) {
    stop("base must be >= 1")
  } else if(score_base == 1) {
    # it converges to this as base -> 1
    return(linear_score_function(n))
  } else {
    out <- vector()
    for(p in 1:n) {
      out[[p]] <- score_base ** (n - p) - 1
    }
  return(out / sum(out))  
  }
}

get_new_ratings <- function(initial_ratings, result_order = c(1:length(initial_ratings)), teams = c(0), k_value = 32, d_value = 400, score_base = 1, log_base = 10) {
# Update ratings based on results. Takes an array of ratings before the matchup and returns an array with the updated ratings. 
# Provided array should be ordered by the actual results (first place finisher's initial rating first, second place next, and so on).
# 
# initial_ratings: array of ratings (float values) in order of actual results
# result_order: list where each value indicates the place the player in the same index of initial_ratings finished in. Lower is better. Identify ties by entering the same value for players that tied. For example, [1, 2, 3] indicates that the first listed player won, the second listed player finished 2nd, and the third listed player finished 3rd. [1, 2, 2] would indicate that the second and third players tied for 2nd place. (default = 1:length(initial_ratings))
# teams <- c(0) # Teams parameter determines if expected ratings should be averaged across a team. If playing in teams then pass a list i.e. c(1,1,2,2,3,3) indicates player 1 & 2 are on a team, player 3 & 4 are on a team, and players 5 & 6 will be on a team.
# k_value <- 32 # K parameter in Elo algorithm that determines how much ratings increase or decrease after each match
# d_value <- 400 # D parameter in Elo algorithm that determines how much Elo difference affects win probability
# score_base <- 1 # score_base: base value to use for scoring function; scores are approximately multiplied by this value as you improve from one place to the next (minimum allowed value is 1, which results in a linear scoring function)
# log_base <- 10 # base to use for logarithms throughout the Elo algorithm. Traditionally Elo uses base-10 logs
#
# Example usage:
# >>> elo = MultiElo()
# >>> elo.get_new_ratings([1200, 1000])
# array([1207.68809835,  992.31190165])
# >>> elo.get_new_ratings([1200, 1000, 1100, 900])
# array([1212.01868209, 1012.15595083, 1087.84404917,  887.98131791])
# 
# returns an array of updated ratings (float values) in same order as input
  # determine number of players
  n <- length(initial_ratings) 
  # calculate the actual ratings 
  actual_scores <- get_actual_scores(n, result_order, score_base)
  # calculate the expected scores
  expected_scores <- get_expected_scores(initial_ratings, result_order, log_base, d_value)
  # determine scale factor
  scale_factor <- k_value * (n-1)
  # apply scale factor to actual and expected scores to 
  return(initial_ratings + scale_factor *(actual_scores - expected_scores))
}

get_actual_scores <- function(n, result_order, score_base) {
  # Return the scores to be awarded to the players based on the results.
  # 
  # n: number of players in the matchup
  # result_order: list indicating order of finish (see docstring for MultiElo.get_new_ratings for more details)
  # 
  # returns an array of length n of scores to be assigned to first place, second place, and so on
  
  # returns a weighted score for each position
  scores <- exponential_score_function(n, score_base)
  # reorders the scores based on the order. Ties are random as they will be averaged together.
  scores <- scores[rank(result_order, ties = 'random')]
  # find the distinct results to know if there were ties.
  distinct_results <- unique(result_order)
  # if there are ties, then average the weight between the tied groups.
  if(length(distinct_results) != n) {
    score_update <- cbind(data.table(scores), data.table(result_order))
    scores <- score_update[, new_scores := mean(scores), by = result_order]$new_scores
  }
  return(scores)
}

get_expected_scores <- function(initial_ratings, teams, log_base, d_value) {
  # Get the expected scores for all players given their ratings before the matchup.
  # 
  # ratings: array of ratings for each player in a matchup
  # returns an array of expected scores for all players
  n = length(initial_ratings)
  base_matrix <- matrix(rep(initial_ratings, n), ncol = n)
  # get all pairwise differences
  diff_matrix <- base_matrix - t(base_matrix) 
  # get individual contributions to expected score using logistic function
  logistic_matrix <- 1/(1 + log_base ** (diff_matrix / d_value))
  diag(logistic_matrix) <- 0
  # get each expected score (sum individual contributions, then scale)
  expected_scores = colSums(logistic_matrix)
  denom = n * (n - 1) / 2  # number of individual head-to-head matchups between n players
  expected_scores = expected_scores / denom
  
  # find the distinct results to know if there were ties.
  distinct_teams <- unique(teams)
  # if there are ties, then average the weight between the tied groups.
  if(length(distinct_teams) != n & length(teams) > 1) {
    score_update <- cbind(data.table(expected_scores), data.table(teams), data.table(initial_ratings))
    score_update[, `:=`(new_scores = mean(expected_scores), team_rating = mean(initial_ratings)), by = teams]
    score_update[, weight := initial_ratings/team_rating][, weighted_score := new_scores * weight]
    expected_scores <- score_update$weighted_score
  }

  # this should be guaranteed, but check to make sure. It is rounded to 15 decimal places to account for rounding errors in R. R generally stores 22 decimals.
  if(round(sum(expected_scores),10) != 1) {
    stop("expected scores do not sum to 1")
  }
  return(expected_scores)
}
