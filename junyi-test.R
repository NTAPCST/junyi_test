#1
#(A)請寫一個程式把裡面的字串反過來。
library(dplyr) #僅使用於 %>% (pip)的部分，無關字串切割

f <- function(string){
  strsplit(string, "")[[1]] %>% rev() %>% paste(collapse = "")
}

f("junyiacademy")

#(B)請寫一個程式把裡面的字串，每個單字本身做反轉，但是單字的順序不變。
reverse <- function(string){
  s <- strsplit(string, " ")[[1]]
  s1 <- c()
  result <- c()
  for (i in 1:length(s)) {
    s1 <- strsplit(s[i], "")[[1]] %>% rev() %>% paste(collapse = "")
    result[i] <- s1
  }
  result
}

reverse("flipped class room is important")

#2
#請寫一個程式，Input 是一個數字，Output 是從 1 到這個數字，
#扣除掉所有 3 的倍數以及 5 的倍數，但是需要保留同時是 3 和 5 的倍數的總數字數。

LCM <- function(x,y){
  if ( max(x,y) %% min(x,y) == 0) least_common_multiple = max(x,y)
  else least_common_multiple = x*y
  least_common_multiple
}

count <- function(number){
  count_3 <- number%/%3
  count_5 <- number%/%5
  count_lcm <- number%/%LCM(3,5)
  result <- number - count_3 - count_5 + 2*count_lcm #2*次是因為一次是重複扣，另一次是希望留15
  result
}


