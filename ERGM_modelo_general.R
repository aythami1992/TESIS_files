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
# donde la probabilidad (tie-level) será : inverse logit == exp(theta_1)/(1-exp(theta_1)) --> [0.15, 0.13, 0.11] respectivamente


# Node covariates: effects on mean degree ---------------------------------

summary(wealth)
summary(flomarriage~ edges+nodecov('wealth'))
flomodel.03 <- ergm(flomarriage~ edges+nodecov('wealth'))
summary(flomodel.03)

# Para interpretar el coeficiente debemos usar el log-odds: theta_1= theta * Chance nº ties (=1) + theta_2 * sum wealth both nodes
# Tenemos 3 posibles situaciones:
# Un nuevo tie entre 2 nodos con minima wealth: entonces el log-odds: theta_1= theta * Chance nº ties (=1) + theta_2 *(146 + 146)
# Un nuevo tie entre 2 nodos con maxima wealth: entonces el log-odds: theta_1= theta * Chance nº ties (=1) + theta_2 *(146 + 146)
# Un nuevo tie entre 2 nodos con uno minima y otro maxima wealth: entonces el log-odds: theta_1= theta * Chance nº ties (=1) + theta_2 *(146 + 3)
# donde la probabilidad (tie-level) será : inverse logit == exp(theta_1)/(1-exp(theta_1)) --> [0.07, 0.58, 0.25] respectivamente

# To specify homophily on wealth, you could use the ergm-term absdiff (next section) .


# Nodal covariates: Homophily ---------------------------------------------
data(package='ergm')
data(faux.mesa.high)
mesa <- faux.mesa.high
mesa

par(mfrow=c(1,1)) # Back to 1-panel plots
plot(mesa,vertex.col='Grade')
legend('bottomleft', fill=7:12,
       legend=paste('Grade', 7:12), cex = 0.75)

fauxmodel.01 <- ergm(mesa ~ edges)
summary(fauxmodel.01)

Prob= exp(-2.5649)/(1+exp(-2.5649))

fauxmodel.01b <- ergm(mesa ~ edges+ nodefactor('Grade') +nodematch('Grade',diff=T))
summary(fauxmodel.01b)

fauxmodel.01 <- ergm(mesa ~ edges
                     + nodefactor('Grade') + nodematch('Grade',diff=T)
                     + nodefactor('Race') + nodematch('Race',diff=T))
summary(fauxmodel.01)
# Note that two of the coefficients are estimated as -Inf (the nodematch coefficients for race Black and Other).
# Why is this?
table(mesa %v% 'Race')
mixingmatrix(mesa, 'Race') # pocas obs en Black and Other y no forman ningún tie

fauxmodel.01mix <- ergm(mesa ~ edges+ nodefactor('Grade') +nodemix('Grade')) #  nodemix es Otra característica del nodo
summary(fauxmodel.01mix)
mixingmatrix(mesa, 'Grade')
# It’s important to check the descriptive statistics of a model in the observed network before fitting
# the model. ( Hacer un summary del modelo que vamos a ajustar antes de hacerlo)


# Directed ties --------------------------------------------------
data(samplk)
ls()
plot(samplk3)
summary(samplk3 ~ edges+mutual)
sampmodel.01 <- ergm(samplk3 ~ edges+mutual)
