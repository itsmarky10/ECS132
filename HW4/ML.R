mergeall <- function()
{
  data <- read.table("u.data")
  dataU <- read.table("u.user", sep = "|")
  dataI <- read.table("u.item", sep = "|", quote = "\"")
  dataI <- dataI[,-4]
  names(data) <- c("UserID", "MovieID", "Rating", "Timestamp")
  names(dataU) <- c("UserID", "Age", "Gender", "Occupation", "ZIPCode")
  names(dataI) <- c("MovieID", "MovieName", "ReleaseDate", "WebPage", "unknown", "Action" , "Adventure", "Animation", "Children's", "Comedy", "Crime","Documentary", "Drama", "Fantasy", "Film-Noir", "Horror", "Musical", "Mystery", "Romance", "Sci-Fi", "Thriller", "War", "Western")
  d <- merge(dataU, dataI)
  d <- merge(data, d)
  write.table(d, file = "u.all", quote = F, sep = "|",  row.names = F,      
             col.names = F)
}

propf <- function()
{
  dataU <- read.table("u.user", sep = "|")
  return (mean(dataU[,3]=="F"))
}

oldest <- function()
{
  dataU <- read.table("u.user", sep = "|")
  return (max(dataU[,2]))
}

mostactive <- function()
{
	data <- read.table("u.data", sort = T)
	names(data) <- c("UserID", "MovieID", "Rating", "Timestamp")
	maxx <- 0
	temp <- 0
	for (i in 1:range(data[,1])[2])
	{
		temp <- length(data[which(data[,1] == i),1])
		if (temp > maxx)
			maxx = temp
	}
	return (maxx)
}