#1
#(A)�мg�@�ӵ{����̭����r��ϹL�ӡC
library(dplyr) #�ȨϥΩ� %>% (pip)�������A�L���r�����

f <- function(string){
  strsplit(string, "")[[1]] %>% rev() %>% paste(collapse = "")
}

f("junyiacademy")

#(B)�мg�@�ӵ{����̭����r��A�C�ӳ�r����������A���O��r�����Ǥ��ܡC
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
#�мg�@�ӵ{���AInput �O�@�ӼƦr�AOutput �O�q 1 ��o�ӼƦr�A
#�������Ҧ� 3 �����ƥH�� 5 �����ơA���O�ݭn�O�d�P�ɬO 3 �M 5 �����ƪ��`�Ʀr�ơC

LCM <- function(x,y){
  if ( max(x,y) %% min(x,y) == 0) least_common_multiple = max(x,y)
  else least_common_multiple = x*y
  least_common_multiple
}

count <- function(number){
  count_3 <- number%/%3
  count_5 <- number%/%5
  count_lcm <- number%/%LCM(3,5)
  result <- number - count_3 - count_5 + 2*count_lcm #2*���O�]���@���O���Ʀ��A�t�@���O�Ʊ�d15
  result
}

