bscheduler.load_node_discovery_data <- function () {
	dir <- file.path('build', 'bscheduler-benchmarks', 'output', 'm12')
	all_files <- list.files(
		dir,
		pattern='bsc.10.1.0.1.log',
		recursive=TRUE
	)
	all_data <- data.frame(
		daemons=rep(NA,0),
		nodes=rep(NA,0),
		attempt=rep(NA,0),
		timeout=rep(NA,0),
		t=rep(NA,0)
	)
	row <- 1
	for (file in all_files) {
		daemons <- as.numeric(gsub('^d([0-9]+)/.*$', '\\1', file, perl=TRUE))
		nodes <- as.numeric(gsub('^d[0-9]+/n([0-9]+)/.*$', '\\1', file, perl=TRUE))
		attempt <- as.numeric(gsub('^d[0-9]+/n[0-9]+/a([0-9]+)/.*$', '\\1', file, perl=TRUE))
		f <- file.path(dir, file)
		data <- readLines(f)
		nsubordinates <- length(data[grepl('add subordinate', data)])
		data <- data[grepl('time since epoch', data)]
		data <- gsub('^.*time since epoch ([0-9]+)ms.*$', '\\1', data, perl=TRUE);
		data <- as.numeric(data)
		data <- data - min(data)
		data <- data.frame(data)
		# calculate adjacent difference
		diff <- data[-1,] - data[-nrow(data),]
		# find termination time point
		idx <- which(diff > 4000)
		if (length(idx) > 0) {
			idx <- idx[[1]]
		}
		if (length(idx) == 0 || daemons == 1) {
			idx <- nrow(data)
		}
		# remove all events after termination
		data <- data[c(1:idx),]
		t <- max(data)
		all_data[row, 'attempt'] <- attempt
		all_data[row, 'nodes'] <- nodes
		all_data[row, 'daemons'] <- daemons
		all_data[row, 't'] <- t
		if (daemons == 1) {
			all_data[row, 'timeout'] <- 190
		} else {
			all_data[row, 'timeout'] <- 100
		}
		row <- row + 1
		if (nsubordinates != nodes*daemons-1) {
			write(paste('# Bad no. of subordinates:', f, nsubordinates), stderr())
			write(paste('rm -rf', dirname(f)), stderr())
		}
	}
	# subtract artificial timeout
	all_data$t <- all_data$t - (all_data$nodes*all_data$daemons)*all_data$timeout
	all_data$timeout <- NULL
	write('All data:', stdout())
	print(all_data[order(all_data$daemons, all_data$nodes, all_data$attempt), ])
	result <- aggregate(
		all_data$t,
		by=list(nodes=all_data$nodes, daemons=all_data$daemons),
		FUN=mean
	)
	result$t_avg <- result$x
	result$x <- NULL
	result$t_min <- aggregate(
		all_data$t,
		by=list(nodes=all_data$nodes, daemons=all_data$daemons),
		FUN=min
	)$x
	result$t_max <- aggregate(
		all_data$t,
		by=list(nodes=all_data$nodes, daemons=all_data$daemons),
		FUN=max
	)$x
	# convert milliseconds to seconds
	result$t_avg <- result$t_avg / 1000
	result$t_min <- result$t_min / 1000
	result$t_max <- result$t_max / 1000
	result
}

bscheduler.plot_discovery <- function (xlabel='No. of physical nodes',
                                       ylabel='Time, s',
                                       toplabel='Processes per node') {
	result <- bscheduler.load_node_discovery_data();
	# consider only large number of nodes
	result <- result[result$nodes>=5,]
	params <- list(list(n=1, col='black'),
				   list(n=8, col='#707070'),
				   list(n=32, col='blue'),
				   list(n=64, col='#c04040'))
	
	ltext <- sapply(params, function (p) {
		paste(toplabel, p$n, sep=': ')
	})
	lcolors <- sapply(params, function (p) { p$col })
	
	#par(mfrow=c(3,2))
	plot.new()
	plot.window(
		xlim=range(result$nodes),
		ylim=range(result$t_min, result$t_max, 0.5, 2.0)
	)
	for (p in params) {
		n <- p$n
		res <- result[result$daemons==n,]
		x <- res$nodes
		lines(x, res$t_avg, col=p$col, lwd=2)
		points(x, res$t_avg, col=p$col)
		lines(x, res$t_min, lty='dashed', col=p$col)
		lines(x, res$t_max, lty='dashed', col=p$col)
	}
	axis(1, at=c(1:max(result$nodes)))
	axis(2, at=seq(0.5,2.0,0.5))
	title(xlab=xlabel, ylab=ylabel)
	legend(
		'topright',
		legend=ltext,
		col=lcolors,
		lty='solid',
		lwd=2
	)
	box()
}
