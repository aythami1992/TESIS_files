library(ergm)
set.seed(0)
??'ergm-terms'
data(package='ergm')
data(florentine)

par(mfrow=c(1,2)) # Setup a 2 panel plot
plot(flomarriage,
     main="Florentine Marriage",
     cex.main=0.8,
     label = network.vertex.names(flomarriage)) # Plot the network
wealth <- flomarriage %v% 'wealth' # %v% references vertex attributes
wealth

plot(flomarriage,
     vertex.cex=wealth/25,
     main="Florentine marriage by wealth", cex.main=0.8) # Plot the network with vertex size proportion


# A simple Bernoulli("Erdos/Renyi") model ---------------------------------

summary(flomarriage ~ edges)
flomodel.01 <- ergm(flomarriage ~ edges)
summary(flomodel.01)
# Para interpretar el coeficiente debemos usar el log-odds: theta_1= theta * Chance nº ties (=1)
# done la probabilidad (tie-level) será : inverse logit == exp(theta_1)/(1-exp(theta_1))


# Triad formation ---------------------------------------------------------

summary((flomarriage ~ edges + triangles))
flomodel.02 <- ergm(flomarriage ~ edges + triangles)
summary(flomodel.02)

# Para interpretar el coeficiente debemos usar el log-odds: theta_1= theta * Chance nº ties (=1) + theta_2 + Chance nº triangles
# Tenemos 3 posibles situaciones:
# Un nuevo tie no crea triangles: entonces el log-odds: theta_1= theta * Chance nº ties (=1)
# Un nuevo tie crea 1 triangles: entonces el log-odds: theta_1= theta * Chance nº ties (=1) + theta_2 *Chance nº triangles (=1 en este caso)
# Un nuevo tie crea 2 triangles: entonces el log-odds: theta_1= theta * Chance nº ties (=1) + theta_2 *Chance nº triangles (=2 en este caso)
#donde la probabilidad (tie-level) será : inverse logit == exp(theta_1)/(1-exp(theta_1)) --> [0.15, 0.13, 0.11] respectivamente


# Node covariates: effects on mean degree ---------------------------------

summary(wealth)
summary(flomarriage~ edges+nodecov('wealth'))
flomodel.03 <- ergm(flomarriage~ edges+nodecov('wealth'))
summary(flomodel.03)

