# Center
matrix(
  data = 
    c(
      0.05, 0.15, 0.05
      ,0.15, 0.20, 0.15
      ,0.05, 0.15, 0.05
    )
  ,byrow = TRUE
  ,nrow = 3
  ,ncol = 3
  ,dimnames = list(Precipitation = c("N", "L", "M"), Wind = c("L", "M", "H"))
) #%>% t()

# Corner
matrix(
  data = 
    c(
      0.25, 0.15, 0.10
      ,0.15, 0.15, 0.05
      ,0.10, 0.05, 0.00
    )
  ,byrow = TRUE
  ,nrow = 3
  ,ncol = 3
  ,dimnames = list(Precipitation = c("N", "L", "M"), Wind = c("N", "L", "M"))
)

# Edge
matrix(
  data = 
    c(
      0.15, 0.15, 0.00
      ,0.20, 0.15, 0.05
      ,0.15, 0.15, 0.00
    )
  ,byrow = TRUE
  ,nrow = 3
  ,ncol = 3
  ,dimnames = list(c("N", "L", "M"), rev(c("L", "M", "H")))
) #%>% t()

# Tropical
matrix(
  data = 
    c(
      0.00, 0.00, 0.10
      ,0.00, 0.20, 0.15
      ,0.10, 0.15, 0.30
    )
  ,byrow = TRUE
  ,nrow = 3
  ,ncol = 3
  ,dimnames = list(Precipitation = c("M", "H", "T"), Wind = c("M", "H", "T"))
)
