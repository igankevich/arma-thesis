source(file.path("build", "arma-benchmarks", "R", "arma.load.R"))
source(file.path("build", "arma-benchmarks", "R", "arma.load_events.R"))

arma.load_benchmark_data <- function(attempt, framework, models, tags) {
	data <- data.frame()
	for (m in models) {
		if (!(m %in% colnames(data))) {
			data[,m] <- rep(NA, nrow(data))
		}
		idx <- 1
		for (t in tags) {
			values <- arma.load(
				file.path(
          "build", "arma-benchmarks", "output",
          "gpulab1", attempt, 10000, framework, m),
				t,
				".*\\s+([0-9]+)us.*"
			)
			name <- names(tags)[idx]
			data[if (nchar(name) == 0) t else name,m] <- mean(values/1000/1000)
			idx <- idx + 1
		}
	}
  data
}

arma.print_openmp_vs_opencl <- function(model_names, row_names) {
  library(ascii)
  options(asciiType="org")
  models <- c("ar", "ma", "lh");
  frameworks <- c("openmp", "opencl")
  tags <- list(
    determine_coefficients=c("deteremine_coefficients", "determine_coefficients"),
    "validate",
    "generate_surface",
    nit=c("nit_acf", "nit_realisation"),
    "copy_to_host",
    velocity=c("window_function", "second_function", "fft", "dev_to_host_copy"),
    "write_all"
  )
  all_data <- list()
  for (framework in frameworks) {
    all_data[[framework]] <- arma.load_benchmark_data("a4", framework, models, tags)
  }
  # translate and pretty print in org-mode format
  saved_row_names <- rownames(all_data$openmp)
  for (framework in names(all_data)) {
    rownames(all_data[[framework]]) <- 1:nrow(all_data[[framework]])
  }
  big_table <- merge(all_data$openmp, all_data$opencl, by="row.names")
  rownames(big_table) <- saved_row_names
  big_table[,"Row.names"] <- saved_row_names
  big_table <- big_table[,!(names(big_table) == "ma.y")]
  big_table[,"Row.names"] <- sapply(big_table[,"Row.names"], function (c) get(c, row_names))
  colnames(big_table) <- sapply(colnames(big_table), function (c) get(c, model_names))
#  rownames(big_table) <- sapply(rownames(big_table), function (c) paste("~", c, "~", sep=""))
  print(ascii(c("", "", "", "OpenMP", "", "OpenCL")))
  print(ascii(big_table, include.rownames=FALSE))
}

arma.load_io_benchmark_data <- function(attempt, filesystems, suffix, tags) {
	data <- data.frame()
	for (fs in filesystems) {
		if (!(fs %in% colnames(data))) {
			data[,fs] <- rep(NA, nrow(data))
		}
		idx <- 1
		for (t in tags) {
			values <- arma.load(
				file.path(
          "build", "arma-benchmarks", "output", "gpulab1",
          paste(attempt, fs, suffix, sep="-"), 10000, "openmp", "ar"),
				t,
				".*\\s+([0-9]+)us.*"
			)
			name <- names(tags)[idx]
			data[if (length(name) == 0 || nchar(name) == 0) t else name,fs] <- mean(values/1000/1000)
			idx <- idx + 1
		}
	}
  data
}

arma.print_sync_vs_async_io <- function(suffix_names, row_names, top_names) {
  library(ascii)
  options(asciiType="org")
  tags <- list("generate_surface", "write_all")
  filesystems <- c("xfs", "nfs", "gfs")
  all_data <- list()
  for (suffix in c("seq", "par")) {
    all_data[[suffix]] <- arma.load_io_benchmark_data("a5", filesystems, suffix, tags)
  }
  # translate and pretty print in org-mode format
  saved_row_names <- rownames(all_data$seq)
  for (suffix in names(all_data)) {
    rownames(all_data[[suffix]]) <- 1:nrow(all_data[[suffix]])
  }
  big_table <- merge(all_data$seq, all_data$par, by="row.names")
  rownames(big_table) <- saved_row_names
  big_table[,"Row.names"] <- saved_row_names
  big_table[,"Row.names"] <- sapply(big_table[,"Row.names"], function (c) get(c, row_names))
  colnames(big_table) <- sapply(colnames(big_table), function (c) get(c, suffix_names))
  print(ascii(c("", "", "", top_names[[1]], "", "",  top_names[[2]])))
  print(ascii(big_table, include.rownames=FALSE))
}

arma.plot_io_events <- function (fsnames) {
  filesystems <- c("xfs", "nfs", "gfs")
  conf <- list(
    a=list(
      color='#000000',
      lty="solid",
      lwd=3,
      name="generate_surface"
    ),
    b=list(
      color='#9F2D20',
      lty="solid",
      lwd=3,
      name="write_surface"
    )
  )
  for (fs in filesystems) {
    attempt <- paste("a5", fs, "events", sep="-")
    data <- arma.load_events(
      file.path("build", "arma-benchmarks",
                "output", "gpulab1", attempt, 10000, "openmp", "ar"),
      c("write_surface", "generate_surface", "programme")
    )
    ev_prog <- data[data$event == "programme",]
    ev_gen <- data[data$event == "generate_surface",]
    ev_write <- data[data$event == "write_surface",]
    ev_write$thread_no = max(ev_gen$thread_no) + 1
    threads <- 0:max(ev_write$thread_no)
    max_x <- max(ev_write$t1, ev_gen$t1)/1000/1000
    plot.new()
    plot.window(xlim=c(0,max_x), ylim=range(threads))
    conf$a$table=ev_gen
    conf$b$table=ev_write
    for (c in conf) {
      table <- c$table
      for (row in seq(1,nrow(table),1)) {
        ev <- table[row,]
        ys <- rep(ev$thread_no, 2)
        xs <- c(ev$t0, ev$t1)/1000/1000
    #		points(xs[[1]], ys[[1]], pch=19, cex=0.4)
    #		arrows(xs[[1]], ys[[1]], xs[[2]], ys[[2]], angle=10, length=0.05)
        lines(xs, ys, lwd=3, col=c$color)
      }
    }
    axis(1, at=pretty(c(0,max_x)))
    axis(
      2,
      at=threads,
      labels=c(sapply(
        threads[1:(length(threads)-1)],
        function (t) paste("omp", t, sep="-")
      ), "io-0"),
      las=2
    )
    mtext("Time, s", side=1, line=3)
    mtext("Thread", side=2, line=4)
    title(fsnames[[fs]])
  }
  legend(
    "bottom",
    inset=c(0,0.2),
    legend=sapply(conf, function (c) c$name),
    col=sapply(conf, function (c) c$color),
    lty=sapply(conf, function (c) c$lty),
    lwd=sapply(conf, function (c) c$lwd),
    xpd=TRUE
  )
}
