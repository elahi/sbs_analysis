# Metabolic theory

######################################

### Constants ###
k <- 0.0000862 # boltzmann constant (metabolic theory)
E <- 0.65 #activation energy for development rate = 0.65. E for PLD (1/dev) = -0.65

# rate = a * e ^ (-E / k*T) # after Forster 2011
# a = constant (species specific; need to solve for this for each parameter)
# E = activation energy
# k = boltzmann constant
# T = temp in Kelvin

# Higher Ea is analogous to higher Q10
# From Dell 2011 PNAS
# Ea = 0.2eV is small (Q10 = 1.27 - 1.31)
# Ea = 0.55-0.65 is median and mean, respectively (Q10 = 1.95 - 2.65)
# Ea = 1.2 eV is large (Q10 = 4.33 - 6.05)

# solve for a
# a = R/ (exp(-Eo/kT))


########################################################


# general Arrhenius function for a rate
# if the rate is a positive function of temperature, choose dir = "pos", if negative, choose dir = "neg"
ArrF <- function(coef_a, E, k, temp, dir) {
  if (dir == "neg") {
    newRate <- coef_a * exp(E/(k* temp))}
  if (dir == "pos") {
    newRate <- coef_a * exp(-E/(k* temp))}
  if (dir != "neg" & dir != "pos") {
    print("Direction of response (dir) must be positive (pos) or negative (neg)") } else {
      return(newRate)
    }
}

# general function to calculate the coefficient a for a given set of parameters
# if the rate is a positive function of temperature, choose dir = "pos", if negative, choose dir = "neg"

aCoef <- function(rate, E, k, temp, dir) {
  if (dir == "neg") {
    a_rate <- rate / (exp(E/(k * temp)))} 
  if (dir == "pos") {
    a_rate <- rate / (exp(-E/(k * temp)))} 
  if (dir != "neg" & dir != "pos") {
    print("Direction of response (dir) must be positive (pos) or negative (neg)") } else {
      return(a_rate)
    }
}

################################################



