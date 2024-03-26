---
title: "Sustainable Fishery Modelling"
output: html_document
date: "2024-03-26"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Introduction

Humans have fished the oceans for a very long time, and up until 100 years ago most people belived this would always be the case. Nowadays countries put more effort into managing and protecting fish stocks, but fishery collapse still occurs. Declining stocks are still fished and ultimately a population may not recover once fishing pressure is ceased. The aim of this report is to formulate a process-based model for a sustainable fishery that will respond to different circumstances, and allow us to assess parameters.

# Model formulation

Density dependent population growth relates to the fact that a population cannot grow geometrically forever, as it will start to run out of resources and space. This leads to fluctuations in population size over time. 
```{r}
R0 = 0.2
K = 100
StartPopulation = 100
v = 0.15
Times = 1000

Time = 0:Times
N = numeric(Times+1)
N[N==0]=NA
N[1] = StartPopulation
G.N = numeric(Times+1)
G.N[G.N==0]=NA
delta.N = numeric(Times+1)
delta.N[delta.N==0]=NA
R.deviation = runif(Times+1, -v, v)

for (time in 1:Times)
{
  R = R0 * (1 - N[time] / K) + R.deviation[time]
  G.N[time]= R * N[time]
  delta.N[time] = G.N[time]
  N[time+1] = N[time] + delta.N[time]
  N[time+1] = ifelse(N[time+1] < 0, 0, N[time+1])
}

LogisticGrowth = data.frame(Time, N, G.N, delta.N, R.deviation)
round(head(LogisticGrowth,10),2)
with(LogisticGrowth, 
     plot(N ~ Time, type="l",  lwd=2, col="blue", 
          xlab="Time", ylab="Population Size (N)", main="Population over time"))

```

For a population that grows logistically, the largest possible population growth G(N) occurs at a population size (N) equal to the carrying capacity (K) divided by 2. The carrying capacity is the population size when per capita recruitment (b) is equal to per capita mortality (d). 

$$
G(N_t) = R_0N_t-(R_0N_t^2/K)
$$
By substituting K/2 to the logistic growth equation above, we find the maximum sustainable yield of the fishery (MSY).
$$
MSY=(R_0K)/4
$$

```{r}
R0 = 0.2
K=100
StartPopulation = 2
v=0.15
Times = 100
h=5

Time = 0:Times
N = numeric(Times+1)
N[N==0]=NA
N[1] = StartPopulation
G.N = numeric(Times+1)
G.N[G.N==0]=NA
delta.N = numeric(Times+1)
delta.N[delta.N==0]=NA
R.deviation = runif(Times+1, -v, v)

for (time in 1:Times)
{
  R = R0 * (1 - N[time] / K) + R.deviation[time]
  G.N[time]= R * N[time]
  delta.N[time] = G.N[time]-h
  N[time+1] = N[time] + delta.N[time]
  N[time+1] = ifelse(N[time+1] < 0, 0, N[time+1])
}

LogisticGrowth = data.frame(Time, N, G.N, delta.N, R.deviation)
#round(head(LogisticGrowth,10),2)

#with(LogisticGrowth, 
#     plot(N ~ Time, type="l",  lwd=2, col="blue", 
#          xlab="Time", ylab="Population Size (N)"))

LogisticModel <- function(R0, h, K, Times, StartPopulation, v){
  Time = 0:Times
  N = numeric(Times+1)
  N[N==0]=NA
  N[1] = StartPopulation
  G.N = numeric(Times+1)
  G.N[G.N==0]=NA
  delta.N = numeric(Times+1)
  delta.N[delta.N==0]=NA
  R.deviation = runif(Times+1, -v, v)
  for (time in 1:Times) {
    R = R0 * (1 - N[time] / K) + R.deviation[time]
    G.N[time]= R * N[time]
    delta.N[time] = G.N[time] - h
    N[time+1] = N[time] + delta.N[time]
    N[time+1] = ifelse(N[time+1] < 0, 0, N[time+1])
  }
  LogisticGrowth = data.frame(Time, N, G.N, delta.N, R.deviation)
  return(LogisticGrowth)
}
LogisticGrowth<- LogisticModel(R0=0.2, h=5, K=100, Times=100, StartPopulation = 50, v=0.15)

num.sims = 4
stored.sims = matrix(nrow=Times+1,ncol=num.sims)

for (sim in 1:num.sims) {
  LogisticGrowth <- LogisticModel(R0=0.2, h=5, K=100, Times=100, StartPopulation = 50, v=0.15)
  stored.sims[,sim]= LogisticGrowth$N
}
matplot(stored.sims,type="l",xlab = "Time", ylab = "Population Size (N)")

```

To fulfill the first criteria for sustainable fisheries, the harvest rate has to equal the population growth. That means, fish are being harvested at the same rate the population is growing. I test with four simulations how a population responds to this harvest rate. In the plot above showing population size over time, it is evident that populations collapse if this harvest rate continues. In 20 simulations shown below, 19 out of 20 populations went extinct. 

```{r}
R0 = 0.2
K=100
StartPopulation = 50
v=0.15
Times = 100
h=5

Time = 0:Times
N = numeric(Times+1)
N[N==0]=NA
N[1] = StartPopulation
G.N = numeric(Times+1)
G.N[G.N==0]=NA
delta.N = numeric(Times+1)
delta.N[delta.N==0]=NA
R.deviation = runif(Times+1, -v, v)

for (time in 1:Times)
{
  R = R0 * (1 - N[time] / K) + R.deviation[time]
  G.N[time]= R * N[time]
  delta.N[time] = G.N[time]-h
  N[time+1] = N[time] + delta.N[time]
  N[time+1] = ifelse(N[time+1] < 0, 0, N[time+1])
}

LogisticGrowth = data.frame(Time, N, G.N, delta.N, R.deviation)
#round(head(LogisticGrowth,10),2)

#with(LogisticGrowth, 
#     plot(N ~ Time, type="l",  lwd=2, col="blue", 
#          xlab="Time", ylab="Population Size (N)", main="Population Growth"))

LogisticModel <- function(R0, h, K, Times, StartPopulation, v){
  Time = 0:Times
  N = numeric(Times+1)
  N[N==0]=NA
  N[1] = StartPopulation
  G.N = numeric(Times+1)
  G.N[G.N==0]=NA
  delta.N = numeric(Times+1)
  delta.N[delta.N==0]=NA
  R.deviation = runif(Times+1, -v, v)
  for (time in 1:Times) {
    R = R0 * (1 - N[time] / K) + R.deviation[time]
    G.N[time]= R * N[time]
    delta.N[time] = G.N[time] - h
    N[time+1] = N[time] + delta.N[time]
    N[time+1] = ifelse(N[time+1] < 0, 0, N[time+1])
  }
  LogisticGrowth = data.frame(Time, N, G.N, delta.N, R.deviation)
  return(LogisticGrowth)
}
LogisticGrowth<- LogisticModel(R0=0.2, h=5, K=100, Times=100, StartPopulation = 50, v=0.15)

num.sims = 20
stored.sims = matrix(nrow=Times+1,ncol=num.sims)

for (sim in 1:num.sims) {
  LogisticGrowth <- LogisticModel(R0=0.2, h=5, K=100, Times=100, StartPopulation = 50, v=0.15)
  stored.sims[,sim]= LogisticGrowth$N
}
matplot(stored.sims,type="l",xlab = "Time", ylab = "Population Size (N)", main="Population size over time")
```

```{r}
R0 = 0.2
K=100
StartPopulation = 75
v=0.15
Times = 100
h=3.75

Time = 0:Times
N = numeric(Times+1)
N[N==0]=NA
N[1] = StartPopulation
G.N = numeric(Times+1)
G.N[G.N==0]=NA
delta.N = numeric(Times+1)
delta.N[delta.N==0]=NA
R.deviation = runif(Times+1, -v, v)

for (time in 1:Times)
{
  R = R0 * (1 - N[time] / K) + R.deviation[time]
  G.N[time]= R * N[time]
  delta.N[time] = G.N[time]-h
  N[time+1] = N[time] + delta.N[time]
  N[time+1] = ifelse(N[time+1] < 0, 0, N[time+1])
}

LogisticGrowth = data.frame(Time, N, G.N, delta.N, R.deviation)
#round(head(LogisticGrowth,10),2)

#with(LogisticGrowth, 
#     plot(N ~ Time, type="l",  lwd=2, col="blue", 
#          xlab="Time", ylab="Population Size (N)", main="Population Growth"))

LogisticModel <- function(R0, h, K, Times, StartPopulation, v){
  Time = 0:Times
  N = numeric(Times+1)
  N[N==0]=NA
  N[1] = StartPopulation
  G.N = numeric(Times+1)
  G.N[G.N==0]=NA
  delta.N = numeric(Times+1)
  delta.N[delta.N==0]=NA
  R.deviation = runif(Times+1, -v, v)
  for (time in 1:Times) {
    R = R0 * (1 - N[time] / K) + R.deviation[time]
    G.N[time]= R * N[time]
    delta.N[time] = G.N[time] - h
    N[time+1] = N[time] + delta.N[time]
    N[time+1] = ifelse(N[time+1] < 0, 0, N[time+1])
  }
  LogisticGrowth = data.frame(Time, N, G.N, delta.N, R.deviation)
  return(LogisticGrowth)
}
LogisticGrowth<- LogisticModel(R0=0.2, h=5, K=100, Times=100, StartPopulation = 50, v=0.15)

num.sims = 20
stored.sims = matrix(nrow=Times+1,ncol=num.sims)

for (sim in 1:num.sims) {
  LogisticGrowth <- LogisticModel(R0=0.2, h=3.75, K=100, Times=100, StartPopulation = 75, v=0.15)
  stored.sims[,sim]= LogisticGrowth$N
}
matplot(stored.sims,type="l",xlab = "Time", ylab = "Population Size (N)")

```
When harvest rate is set below the maximum sustainable yield, the population can be managed at a stable and unstable equilibrium. A population is unstable when it cannot bounce back after a good or bad year. A stable population size is to the right of the peak in the production function, at $$N=3K/4$$

The graph above shows that with harvesting a larger population size at a lower harvest rate reduces extinction risk.

```{r}
R0 = 0.2
K=100
StartPopulation = 25
v=0.15
Times = 100
h=3.75

Time = 0:Times
N = numeric(Times+1)
N[N==0]=NA
N[1] = StartPopulation
G.N = numeric(Times+1)
G.N[G.N==0]=NA
delta.N = numeric(Times+1)
delta.N[delta.N==0]=NA
R.deviation = runif(Times+1, -v, v)

for (time in 1:Times)
{
  R = R0 * (1 - N[time] / K) + R.deviation[time]
  G.N[time]= R * N[time]
  delta.N[time] = G.N[time]-h
  N[time+1] = N[time] + delta.N[time]
  N[time+1] = ifelse(N[time+1] < 0, 0, N[time+1])
}

LogisticGrowth = data.frame(Time, N, G.N, delta.N, R.deviation)
#round(head(LogisticGrowth,10),2)

#with(LogisticGrowth, 
#     plot(N ~ Time, type="l",  lwd=2, col="blue", 
#          xlab="Time", ylab="Population Size (N)", main="Population Growth"))

LogisticModel <- function(R0, h, K, Times, StartPopulation, v){
  Time = 0:Times
  N = numeric(Times+1)
  N[N==0]=NA
  N[1] = StartPopulation
  G.N = numeric(Times+1)
  G.N[G.N==0]=NA
  delta.N = numeric(Times+1)
  delta.N[delta.N==0]=NA
  R.deviation = runif(Times+1, -v, v)
  for (time in 1:Times) {
    R = R0 * (1 - N[time] / K) + R.deviation[time]
    G.N[time]= R * N[time]
    delta.N[time] = G.N[time] - h
    N[time+1] = N[time] + delta.N[time]
    N[time+1] = ifelse(N[time+1] < 0, 0, N[time+1])
  }
  LogisticGrowth = data.frame(Time, N, G.N, delta.N, R.deviation)
  return(LogisticGrowth)
}
LogisticGrowth<- LogisticModel(R0=0.2, h=5, K=100, Times=100, StartPopulation = 50, v=0.15)

num.sims = 20
stored.sims = matrix(nrow=Times+1,ncol=num.sims)

for (sim in 1:num.sims) {
  LogisticGrowth <- LogisticModel(R0=0.2, h=3.75, K=100, Times=100, StartPopulation = 25, v=0.15)
  stored.sims[,sim]= LogisticGrowth$N
}
matplot(stored.sims,type="l",xlab = "Time", ylab = "Population Size (N)")

```

When applying the same harvest rate to a smaller population size, probability of fishery collapse increases.


## Results and Discussion
Our model of sustainable harvesting consists of two main rules, which specify that the fish population should be in equilibrium and ecologically stable. If we modify our assumptions to make the model more realistic and our original conclusions hold up, then it can be said that the model is robust. The first criteria for sustainability is not sufficient for a robust model based on the simulation results above. Consequently, some modifications have to be made. It is expected that by harvesting on the right side of the peak of the production function (POPF), populations will not only be in equilibrium, but will also be able to tend back towards equilibrium in a good or bad year. That is, both sustainability criteria would be met. In order to perfect our model, adjustments have to be made to account for environmental fluctuations. These would potentially cause the population to drop so low that it reaches the other side of the peak of the production function and put the population at risk of extinction. A larger target stock population and a lower harvest, that is far enough to the right of the peak of the production function, would potentially increase the recovery period of the population and decrease the extinction probability. A larger target stock size 4K/5 and lower harvest rate of 3.2 produced the lowest extinction rate. 

In conclusion, our “rules of thumb” for sustainability have shown to be generally robust. However, for the industry to be prepared for environmental fluctuations, it is recommended that fisheries are harvested at a large stock population, with lower harvest rate. This strategy decreases the risk of extinction although implementation will be challenging. 

