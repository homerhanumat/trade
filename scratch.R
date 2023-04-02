library(tidyverse)

set.seed(3030)
b <- 30
s <- 40
bmean <- 55
smean <- 52
buyers <- rnorm(b, mean = bmean, sd = 10) %>% 
  sort(decreasing = TRUE)
sellers <- rnorm(s, mean = smean, sd = 10) %>% sort()

df_b <- data.frame(
  x = 0:b,
  y = c(max(buyers), buyers),
  type = "buyer"
)

df_s <- data.frame(
  x = 0:s,
  y = c(min(sellers), sellers),
  type = "seller"
)
df <- df_b %>% bind_rows(df_s) %>% 
  mutate(
    text = ifelse(
      type == "buyer",
      glue::glue("A buyer is willing to pay at most {round(y, 2)}."),
      glue::glue("A seller will sell for at least {round(y, 2)}.")
    )
  )

equilibrium <- function(b, s) {
  if (min(b) >= max(s)) return(c(NA, NA))
  m <- min(length(b), length(s))
  a <- which(b[1:m] < s[1:m])[1]
  z <- s[a] - s[a - 1] - b[a] + b[a - 1]
  x <- (a * z + b[a] - s[a]) / z
  y <- (b[a] - b[a - 1]) * (x - a) + b[a]
  c(x, y)
}

e <- equilibrium(b = buyers, s = sellers)


plot <- df %>% 
  ggplot(aes(x = x, y = y)) +
  geom_line(aes(color = type)) +
  geom_point(size = 0.3, aes(color = type)) +
  geom_segment(aes(
    x = 0,
    xend = max(x),
    y = e[2],
    yend = e[2]
  )) +
  geom_point(aes(x = e[1], y = e[2]), size = 2)

brect <- function(x, e, y) {
  geom_rect(
    data = data.frame(
      x = x, e = e, y = y
    ),
    aes(
      xmin = x - 1,
      xmax = x,
      ymin = e, ymax = y
    ),
  color = "red")
}

plot + brect(10, e[2], buyers[10])


