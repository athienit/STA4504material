# Read data from Table 9.8.
# Thanks to Gary LosHuertos for the code.

# Function I wrote to make expansions of categorical data easier
expand.table <- function(...) {
	n <- 1
	for(param in names(list(...))) {
		n <- n * length(list(...)[[param]])
	}
	i <- 1
	data <- NULL
	for(param in names(list(...))) {
		n.i <- length(list(...)[[param]])
		n.rem <- 1
		for(j in i:length(names(list(...)))) {
			n.rem <- n.rem * length(list(...)[[names(list(...))[j]]])
		}
		rep.each <- n.rem/n.i
		rep.times <- n/n.rem
		
		col <- rep(list(...)[[param]], times=rep.times, each=rep.each)
		if(is.null(data)) {
			data <- col
		} else {
			data <- data.frame(data, col)
		}
		
		i <- i + 1
	}
	colnames(data) <- names(list(...))
	
	data
}

# Enter the data - expand.table generates the columns so that the data are ordered as they are in the textbook
data.grouped <- expand.table(age7=c("No", "Yes"), age8=c("No", "Yes"), age9=c("No", "Yes"), maternal.smoking=c("No", "Yes"), age10=c("No", "Yes"))
data.grouped <- data.frame(data.grouped, children=c(
	237, 10, 118, 6,
	15, 4, 8, 2,
	16, 2, 11, 1,
	7, 3, 6, 4,
	24, 3, 7, 3,
	3, 2, 3, 1,
	6, 2, 4, 2,
	5, 11, 4, 7
))
data.grouped <- data.frame(data.grouped)

# Expand into subjects - repeat each row the number of times specified in the "children" column
data.subjects <- data.grouped[rep(1:nrow(data.grouped), times=data.grouped$children),]

# We no longer need the "children" column
data.subjects <- subset(data.subjects, select=-children)

# Assign simple subject numbers
data.subjects <- data.frame(subject=factor(1:nrow(data.subjects)), data.subjects)
n <- nrow(data.subjects)

# Un-pivot the data based on age
data.unpivoted <- data.frame(
	data.subjects$subject,
	rep(8:10, each=n),
	matrix(data.subjects[,c('age7', 'age8', 'age9', 'age10')][as.matrix(cbind(1:n, rep(c(2:4, 1:3), each=n)))], ncol=2),
	data.subjects$maternal.smoking
)
# The column names get an ugly prefix from data.frame - replace them with nice names
colnames(data.unpivoted) <- c("subject", "time", "illness", "illness.before", "maternal.smoking")

# Order by subject number and time
data.unpivoted <- data.unpivoted[order(data.unpivoted$subject, data.unpivoted$time),]
rownames(data.unpivoted) <- NULL

# Fit the GLM to make sure it matches the output on page 289 of the Agresti book
# (glm(illness ~ illness.before + maternal.smoking + time, family=binomial, data=data.unpivoted))

# Write the data to a tab delimited text file
#getwd() # directory where file will be saved
#write.table(data.unpivoted, file="Table-9.8.txt", sep="\t", col.names=TRUE)

# How to read data from text file
#read.data <- read.table(paste(getwd(),c("/Table-9.8.txt"),sep=""),header=TRUE)
# Read data from directory "C:/.../Table-9.8.txt"

library(gee)
gee1=gee((illness == "Yes") ~ illness.before+time+maternal.smoking,
	  id=subject, data=data.unpivoted, family=binomial, corstr="exchangeable")
summary(gee1)

library(lme4)
lme=glmer((illness == "Yes") ~ illness.before+time+maternal.smoking+(1|subject),
		data=data.unpivoted, family=binomial)
summary(lme)
