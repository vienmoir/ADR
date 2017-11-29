data = read.csv("антибиотики2.csv", stringsAsFactors = FALSE)
data$adr = ""
for (i in 1:nrow(data)) {
  print(data$review[i])
  print("¬ведите 1, если в тексте упоминаютс€ побочные эффекты,")
  print("¬ведите 0 в противном случае,")
  print("¬ведите -1, если не уверены,")
  print("¬ведите s, если устали и хотите прерватьс€:")
  n <- readline(prompt="»так:  ")
  if (n == "s") break
  data$adr[i] = n
}


