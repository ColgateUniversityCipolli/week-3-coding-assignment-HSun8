# Henry Sun 
# Homework 3 R File

# prime num is num with no factors 
# besides 1 and itself
# thus, prime * prime = composite
# if we add all numbers in 
all.nums = 1:100
all.primes = c()
sample.nums = c(6, 10, 14, 15, 21, 22, 26, 33, 34, 
                35, 38, 39, 46, 51, 55, 57, 58, 
                62, 65, 69, 75, 77, 82, 85, 86, 
                87, 91, 93, 94, 95)
# iterate through all the numbers
# check if prime by using modular division 
# only keep num when factor = num and factor = 1
# prevent duplicates by doing not in
for (i in 1:length(all.nums)){
  all.factors = c()
  for (j in 1:i){
    if (i %% j == 0){
      all.factors = c(all.factors, j) 
    }
  }
  if (length(all.factors) <= 2 & !i %in% all.primes){
    all.primes = c(all.primes, i)
  }
}

# now that we have all prime nums
# know property of all nums in table
# is product of unique prime products 
prime.products = c()
for (k in 1:length(all.primes)){
  for(l in 1:length(all.primes)){
    prime.product = all.primes[k] * all.primes[l]
    if (all.primes[k] != all.primes[l] 
        & prime.product < 100){
      prime.products = setdiff(c(prime.products, prime.product), all.primes)
    }
  }
}

# solutions
setdiff(prime.products, sample.nums)
setdiff(sample.nums, prime.products)