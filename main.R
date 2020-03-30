pow_digit <- function(base, exponent){
  num_space <- ceiling(log10(base^exponent))
  x <- rep(0, num_space)
  x[num_space] <- 1
  dum <- 0
  for (i_ex in seq(exponent)) {
    x <- x * base
    for (i_sp in num_space:1) {
      tmp <- x[i_sp]
      x[i_sp] <- (tmp + dum) %% 10
      dum <- (tmp + dum) %/% 10
    }
  }
  return(paste(x, collapse = ""))
}
factorial_digit <- function(base){
  num_space <- ceiling(log10(factorial(base)))
  x <- rep(0, num_space)
  x[num_space] <- 1
  dum <- 0
  for (i_ex in seq(base)) {
    x <- x * i_ex
    for (i_sp in num_space:1) {
      tmp <- x[i_sp]
      x[i_sp] <- (tmp + dum) %% 10
      dum <- (tmp + dum) %/% 10
    }
  }
  return(paste(x, collapse = ""))
}
large_digit <- function(x, y){
  x_digit <- strsplit(x, "")[[1]]
  y_digit <- strsplit(y, "")[[1]]
  
  len_digit <- max(length(x_digit), length(y_digit))
  
  x_digit <- c(rep("0", len_digit - length(x_digit)), x_digit)
  y_digit <- c(rep("0", len_digit - length(y_digit)), y_digit)
  
  sum_digit <- rep(0, len_digit)
  dum <- 0
  for (i in len_digit:1) {
    sum_digit[i] <- (as.numeric(x_digit[i]) + as.numeric(y_digit[i]) + dum) %% 10
    dum <- (as.numeric(x_digit[i]) + as.numeric(y_digit[i]) + dum) %/% 10
  }
  
  result <- paste(c(ifelse(dum == 0, "", dum), sum_digit), collapse = "")
  return(result)
}
divisors <- function(base){
  tmp <- NULL
  for (i in seq(sqrt(base))) {
    if (base %% i == 0) {
      tmp <- c(tmp, i , base / i)
    }
  }
  tmp <- union(tmp, tmp)
  tmp <- tmp[order(tmp)]
  return(tmp)
}
repeating_decimal <- function(num_limit){
  
  x <- 0
  val <- 0
  
  for (i in 1:num_limit){
    d <- NULL
    tt <- NULL
    last_d <- NULL
    dum <- 1
    n <- 1
    
    while(!((dum == 0) | (dum %in% last_d))){
      last_d <- d
      n <- (10 * dum) %/% i
      tt <- c(tt, (10 * dum) %/% i)
      d <- c(d, (10 * dum) %% i)
      dum <- (10 * dum) %% i
    }
    if (dum %in% last_d) {
      tmp <- length(last_d) - which(last_d == dum) + 1
      if (tmp > val) {
        val <- tmp
        x <- i
      }
      
    }
    # if (i == 1) {
    #   cat("\n", "SEQ", "\t", "best", "\t", "val", "\t", "now")
    # }
    # cat("\n", i, "\t", x, "\t", val, "\t", tmp)
    tmp <- 0
  }
  
  return(c(x, val))
}
eratosthenes_sieve <- function(num_limit){
  raw_set <- seq(num_limit)
  raw_set <- raw_set[-1]
  i <- raw_set[1]
  iter <- 1
  while (i < (num_limit / 2)) {
    raw_set <- raw_set[(raw_set == i) | !(raw_set %% i == 0)]
    i <- raw_set[1 + iter]
    iter <- iter + 1
    if (iter %% 1000 == 0) {
      cat("\n", iter, "times complete.")
    }
  }
  cat("\n", "total ", iter, "times.")
  return(raw_set)
}
euclidean_algorithm <- function(num_a, num_b){
  a <- max(num_a, num_b)
  b <- min(num_a, num_b)
  
  if (b == 0) {
    return(0)
  }
  while(TRUE){
    
    r <- a %% b
    if (r == 0) {
      return(b)
    }
    a <- b
    b <- r
  }
}
move_one <- function(coord_x, coord_y, direction){
  if (direction == "u") {
    coord_y <- coord_y - 1
  } else if (direction == "d") {
    coord_y <- coord_y + 1
  } else if (direction == "r") {
    coord_x <- coord_x + 1
  } else if (direction == "l") {
    coord_x <- coord_x - 1
  }
  return(coord_x, coord_y)
}
digit_to_binary <- function(x){
  if (x == 0){
    return(0)
  }
  bin_size <- ifelse(log2(x) == ceiling(log2(x)), log2(x) + 1, ceiling(log2(x)))
  bin_num <- rep(NA, bin_size)
  tmp <- x
  for (i in 1:bin_size) {
    bin_num[bin_size - i + 1] <- tmp %% 2
    tmp <- tmp %/% 2
  }
  return(bin_num)
}
binary_to_digit <- function(x){
  tmp <- as.numeric(strsplit(x, "")[[1]])
  bin_len <- length(tmp)
  
  return(sum((2^(seq(bin_len:1) - 1)) * tmp))
}
num_split <- function(x){
  return(as.numeric(strsplit(paste0(x), "")[[1]]))
}
pandigital_test <- function(x){
  len <- 0
  pan_set <- NULL
  i_dum <- 1
  while(len < 9){
    tmp <- as.numeric(paste(x, collapse = "")) * i_dum
    pan_set <- c(pan_set, tmp)
    len <- len + nchar(tmp)
    i_dum <- i_dum + 1
  }
  pan_num <- paste(pan_set, sep = "", collapse = "")
  
  check <- num_split(pan_num)
  if (length(check) != 9) {
    return(FALSE)
  } else if (all(check[order(check)] == c(1:9))) {
    cat("\n", pan_num, "\t", length(x))
    return(TRUE)
  } else {
    return(FALSE)
  }
}
