# Solve deterministic life-cycle model for policy function
n_grid <- 100

a_max <- 1000
a_min <- 0
agrid <- seq(a_max, a_min, length.out = n_grid)

c_max <- 1000
c_min <- 0
cgrid <- seq(c_max, c_min, length.out = n_grid)

u <- function(c, gamma = 2, b = 10) {
  return(b + c^(1-gamma)/(1-gamma))
}

ugrid <- u(cgrid)

# populate vgrid initial by 0s - two dimensions, a and age = o
vgrid <- matrix(rep.int(0, n_grid*n_grid), nrow = n_grid)

# the policy function to pick c[t+1](a_t)
apolicy <- matrix(rep.int(0, n_grid*n_grid), nrow = n_grid)

# solve backwards - first find vgrid[a_i, age=100]
r <- 0.03

for i=1:n_grid {
  for j=1:n_grid {
    apolicy[j, 100-i+1] <- which.max(ugrid[,])
    vgrid[j, 100-i+1] <- u(a[j]*(1+r) - apolicy[j, 100-i+1]) + vgrid[...] # problem is interpolation
  }
}
