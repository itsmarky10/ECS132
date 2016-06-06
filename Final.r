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
}

parta <- function(){
	
	mergeall() 
	male = split(d, gender)$M
	SampleMean = mean(male$Rating)
	se = sd(male$Rating)/sqrt(length(male$Rating))
	 
	print(SampleMean + qnorm(0.025)*se)
	print(SampleMean + qnorm(0.975)*se)
}

partb <- function(){
	mergeall()
	 
	female = split(d, gender)$F
	SampleMean = mean(female$Rating)
	se = sd(female$Rating)/sqrt(length(female$Rating))
	 
	print(SampleMean + qnorm(0.025)*se)
	print(SampleMean + qnorm(0.975)*se)
}

partc <- function()
{
	mergeall()
	
	male = split(d, gender)$M
	female = split(d, gender)$F
	
	MeanM = mean(male$Rating)
	MeanF = mean(female$Rating)
	seM = sd(male$Rating)^2/(length(male$Rating))
	seF = sd(female$Rating)^2/(length(female$Rating))
	M = MeanM - MeanF 
	print(M + qnorm(0.025)*sqrt(seF + seM))
	print(M + qnorm(0.975)*sqrt(seF + seM))
}

partd <- function()
{
	mergeall()
	
	male = split(d, gender)$M
	female = split(d, gender)$F
	t.test(male$Rating, female$Rating)
}

parte <- function()
{
	mergeall() 
	
	male = split(d, gender)$M
	female = split(d, gender)$F
	
	x = male$Rating
	y = female$Rating
	
	hist(x, main = "Male and Female ratings", xlab = "Rating", ylab = "Number of people", col="red")
	hist(y, add =T, col="blue")
}

partf <- function()
{
	mergeall() 
	
	M = mean(d$Rating) 
	S = sd(d$Rating) 
	n = sqrt(length(d$Rating))
	
	print(M + qnorm(0.025)*S/n)
	print(M + qnorm(0.975)*S/n)
}

partg <- function()
{
	mergeall() 
	male = split(d, gender)$M
	n = length(d[,4])
	pm = length(male[,4])/n
	seM = pm*(1-pm)/n 
	
	print(pm + qnorm(0.025)*sqrt(seM))
	print(pm + qnorm(0.975)*sqrt(seM))
}

parth <- function()
{
	mergeall() 
	lmout = lm(d$Age~d$Gender)
	summary(lmout) 
}
