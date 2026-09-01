a <- data.frame(PIB=c(75000, 75000, 50000, 40000, 40000), 
                EV = c(80,75, 60, 70, 65))
rownames(a)<- c("P1","P2","P3","P4","P5")
Z <- get_poset(a, rownames(a))

plot(Z)
