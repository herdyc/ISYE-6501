library(FrF2)

houses <- FrF2(
  nruns=16, nfactors=10, 
  factor.names = c(
    'Pool', 'Backyard', 'Barrel Tile', 'Garage', 'Multi Car Garage',
    'Car Port', 'Fancy Kitchen', 'Fences', 'Laundry Rooms', 'Smart Houses'
  ),
  default.levels = c('Yes', 'No'))

houses