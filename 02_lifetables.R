mx_to_ax <- function(mx, age, sex = "f"){
  # new:
  sex <- sex |> 
    substr(1,1) |> 
    tolower()
  # -----
  sex <- rep(sex[1], 111)
  ax = case_when(
    age == 0 & sex == "m" & mx < 0.023 ~ 0.14929 - 1.99545 * mx,
    age == 0 & sex == "m" & mx >= 0.023 & mx < 0.08307 ~ 0.02832 + 3.26021 * mx,
    age == 0 & sex == "m" & mx >= 0.08307 ~ 0.29915,
    age == 0 & sex == "f" & mx < 0.01724 ~ 0.14903 - 2.05527 * mx,
    age == 0 & sex == "f" & mx >= 0.01724 & mx < 0.06891 ~ 0.04667 + 3.88089 * mx,
    age == 0 & sex == "f" & mx >= 0.06891 ~ 0.31411,
    age == max(age) ~ 1/mx,
    TRUE ~ 0.5
  )
  return(ax)
}

mx_to_qx <- function(mx, ax){
  qx <- mx / (1 + (1 - ax) * mx)
  qx <- if_else(qx > 1, 1, qx)
  qx[length(qx)] <- 1 # new, forced closeout
  return(qx)
}

qx_to_lx <- function(qx, radix = 1){
  lx <- cumprod(1 - qx)
  # lx <- c(1, lx[-length(lx)])
  lx <- dplyr::lag(lx, default = 1)
  lx <- radix * lx
  return(lx)
}

lxdx_to_Lx <- function(lx, dx, ax){
  Lx <- lx - (1 - ax) * dx
  return(Lx)
}

Lx_to_Tx <- function(Lx){
  Tx <- Lx |> 
    rev() |> 
    cumsum() |> 
    rev()
  return(Tx)
}
# a simple lifetable function 

lt_full <- function(.x, .y, radix = 1){
   out <-
    .x |> 
    mutate(
      ax = mx_to_ax(mx = mx,
                    age = age, 
                    sex = .y$sex),
      qx = mx_to_qx(mx = mx, 
                    ax = ax),
      lx = qx_to_lx(qx = qx, radix = radix),
      dx = lx * qx,
      Lx = lxdx_to_Lx(lx = lx,
                      ax = ax,
                      dx = dx),
      Tx = Lx_to_Tx(Lx),
      ex = Tx / lx) 
  return(out)
}


lt_full2 <- function(age, mx, radix = 1, sex){
      ax = mx_to_ax(mx = mx,
                    age = age, 
                    sex = sex)
      qx = mx_to_qx(mx = mx, 
                    ax = ax)
      lx = qx_to_lx(qx = qx, radix = radix)
      dx = lx * qx
      Lx = lxdx_to_Lx(lx = lx,
                      ax = ax,
                      dx = dx)
      Tx = Lx_to_Tx(Lx)
      ex = Tx / lx
 
    out <- tibble(age = age,
             nx = nx,
             mx = mx,
             ax = ax,
             qx = qx,
             lx = lx,
             dx = dx,
             Lx = Lx,
             Tx = Tx,
             ex = ex)
    
    return(out)
      
}

