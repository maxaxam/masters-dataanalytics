# data
chests_data=list(N=50, x = c(40, 37, 36.5, 43.3, 38.4, 41.4, 43.2, 40.6, 37.9, 43.9, 41.8, 36.2, 41.1, 38.7, 39.4, 41.3, 42, 37.9, 37.4, 36.3, 39.3, 42.1, 41.6, 40.1, 39.2, 36, 39.9, 42, 39, 37.9, 40.7, 39.2, 41.3, 37.1, 42.1, 37.7, 37.1, 39, 37.2, 40.6, 42.3, 40.5, 42.2, 38.3, 40.1, 40.4, 38.9, 39.2, 41, 41.2))

# hyperparameters (parameters for the priors)
chests_hypers=list(phi=4, theta0=38, phi0=9)

# save in a single list
chests=append(chests_data, chests_hypers)
