library(rvest)
library(dplyr)

h <- read_html("https://www.nice.org.uk/guidance/conditions-and-diseases")

l1 <- h |> html_elements("#section .menu , h3") |> html_text2() |> unique()

#############################
l2 <- list()
for (ii in 1:length(l1)) {

  if (l1[ii]=="ME/CFS") l1[ii] <- "mecfs"
  h2 <- read_html(gsub(",","",paste0("https://www.nice.org.uk/guidance/conditions-and-diseases","/",gsub(" ","-",l1[ii]))))

  l2[[ii]] <- h2 |> html_elements("#subsection") |> html_text2() |> strsplit("\n")
  l2[[ii]] <- list(unlist(l2[[ii]])[-1]) #drop first elemnent
}

df_i <- list()
count <- 1
for (ii in 1:length(l1)) {
      df_i[[count]] <- expand.grid(l1[ii], l2[[ii]][[1]])
      count <- count+1
}
df <- do.call(rbind,df_i)

#############################

df_i <- list()
for (ii in 1:nrow(df)) {
    print(nrow(df)-ii)
    h3 <- read_html(
      gsub("'s","",
      gsub(",","",
           paste0(tolower(paste0("https://www.nice.org.uk/guidance/conditions-and-diseases","/",gsub(" ","-",df$Var1[ii]),"/",gsub(" ","-",df$Var2[ii]))),"/products?ProductType=Guidance&Status=Published&Status=Terminated")
           ))
      )
    l3 <- h3 |> html_elements(".gtm-topic-list-product") |> html_text2()
    df_i[[ii]] <- cbind(df[ii,],data.frame(Var3=l3))
}
nicedf <- do.call(rbind, df_i)

nicedf <- nicedf |>
  rename(L1=Var1, L2=Var2, L3=Var3)

save(nicedf, file="./data-raw/nice_hierarchy.rdata")
