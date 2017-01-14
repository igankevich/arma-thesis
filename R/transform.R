arma.skew_normal_1 <- function(z, skewness, kurtosis) {
  (24 + 4*z*(-3 + z**2)* skewness + (3 - 6*z**2 + z**4)* kurtosis)/(24*(exp((z**2)/2)*sqrt(2*pi)))
}

arma.bits.erf <- function(x) {
  2 * pnorm(x * sqrt(2)) - 1
}

arma.bits.erfc <- function(x) {
  2 * pnorm(x * sqrt(2), lower = FALSE)
}

arma.bits.skewness_2 <- function(alpha) {
  (sqrt(2) * (4 - pi) * (alpha**3)) / (sqrt(pi + (pi-2) * (alpha**2))**3)
}

arma.bits.kurtosis_2 <- function(alpha) {
  (8*(-3 + pi)*(alpha**4))/ ((pi + (-2 + pi)*(alpha**2))**2)
}

arma.skew_normal_2 <- function(z, alpha) {
  arma.bits.erfc( -((z*alpha)/sqrt(2)) ) / (exp((z**2)/2)*sqrt(2*pi))
}
