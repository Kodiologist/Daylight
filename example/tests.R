..phooey = 3

if (exists("..phooey", .GlobalEnv, inherits = F))
   stop("Something's wrong: we're evaluating this in .GlobalEnv")

if ("package:boot" %in% search())
   stop("Can't run the library tests: 'boot' already loaded")

evalq(library(boot), .GlobalEnv)

if (!exists("boot", .GlobalEnv, inherits = T))
   stop("Normally loaded library not visible from .GlobalEnv")
if (exists("boot", inherits = T))
   stop("Normally loaded library visible from our environment")

library(boot)

if (!exists("boot", inherits = T))
   stop("Fake-loaded library not visible from our environment")
