pacman::p_load(tidyverse)

#### EXERCISE 1

# g: non-linear function relating the average light intenstiy with the size
g <- function(v) {
  return(v^2)
} 
# size/diameter of food item
v = seq(0.01, 5, by = 0.01)
# u: estimate of light intensity7
u = 2
var_u = 1

## prior
v_p = 3
var_p = 1

# p(v)
p_v <- dnorm(v, v_p, var_p^2)

data.frame(x = v, y = p_v) %>% 
  ggplot(aes(x, y)) +
  geom_line() +
  theme_bw() +
  labs(x = 'v', y = 'p(v)')

# p(u|v)
p_u_v <- dnorm(u, g(v), var_u^2)

numerator <- (p_v * p_u_v)
denominator <- sum(numerator * 0.01)

p_v_u <- numerator / denominator

plot <- data.frame(x = v, y = p_v_u) %>% 
  ggplot(aes(x, y)) +
  geom_line() +
  theme_bw() +
  labs(x = 'v', y = 'p(v|u)')

plot


#### EXERCISE 2
f_der <- function(phi) {
  epsilon_p <- (v_p - phi) / var_p
  epsilon_u <- (u-g(phi)) / var_u ## what is the g(phi)?
  #g: non-linear function relating the average light intenstiy with the size
  g_der <- 2*phi
  
  f_d <- epsilon_p + epsilon_u * g_der
  return(f_d)
}

time_steps = 500

# most likely size of food
phi <- array(0, c(time_steps))
phi[1] <- v_p

delta_t <- 0.01

for (i in 2:time_steps) {
  phi[i] <- phi[i-1] + delta_t * f_der(phi[i-1])
}

data.frame(x = (seq(1, time_steps, by = 1)/100), y = phi) %>% 
  ggplot(aes(x = x, y= y)) + 
  geom_point() +
  geom_line() +
  labs(x = "Time", y = "phi")

plot(phi)

#### EXERCISE 3

#simulate the model
phi <- array(0, c(time_steps))
epsilon_p <- array(0,c(time_steps))
epsilon_u <- array(0,c(time_steps))
phi[1] = v_p
epsilon_p[1] = 0
epsilon_u[1] = 0

for (i in 2:time_steps) {
  phi[i] <- phi[i-1] + delta_t * (-epsilon_p[i-1] + epsilon_u[i-1] * (2*phi[i-1]))
  epsilon_p[i] <- epsilon_p[i-1] + delta_t * (phi[i-1] - v_p - var_p*epsilon_p[i-1])
  epsilon_u[i] <- epsilon_u[i-1] + delta_t * (u - g(phi[i-1]) - var_u*epsilon_u[i-1])
}

plot(phi)
plot(epsilon_p)
plot(epsilon_u)
plot(phi,epsilon_p)
plot(epsilon_p,epsilon_u)
plot(epsilon_u,phi)



data_ex3 <- tibble("Time" = (1:time_steps/100), "Phi" = phi, "Epsilon_p" = epsilon_p, "Epsilon_u" = epsilon_u)

data_ex3_long <- data_ex3 %>% 
  pivot_longer(cols = c("Phi", "Epsilon_p", "Epsilon_u"))

ggplot(data_ex3_long, aes(Time, value, color=name))+
  geom_line(aes(linetype=name))+
  theme_bw()+
  ylim(-2, 3)

#### EXERCISE 4




#### EXERCISE 5