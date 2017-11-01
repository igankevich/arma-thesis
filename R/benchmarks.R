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

arma.plot_io_events <- function (names) {
  fsnames <- list(
    xfs="XFS",
    nfs="NFS",
    gfs="GlusterFS"
  )
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
    mtext(names$xlab, side=1, line=3)
    mtext(names$ylab, side=2, line=4)
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

arma.load_realtime_data <- function () {
  tags <- c(
    "harts_g1",
    "harts_g2",
    "harts_fft",
    "harts_copy_to_host"
  )
  sizes <- 2^c(7:14)
  frameworks <- c("openmp", "opencl")
  attempt <- "a6"
  data <- data.frame()
  row <- 1
  for (framework in frameworks) {
    for (m in sizes) {
      for (t in tags) {
        all_data <- arma.load(
          file.path("build", "arma-benchmarks", "output", "storm", attempt, m, framework),
          t,
          ".*\\s+([0-9]+)us.*"
        )
        data[row,"framework"] <- framework
        data[row,"size"] <- as.integer(m)
        data[row,"t"] <- mean(all_data/1000/1000)
        data[row,"routine"] <- t
        row <- row + 1
      }
    }
  }
  data
}

arma.aggregate_by_size <- function (data, framework) {
  fwdata <- data[data["framework"] == framework,]
  fwdata <- aggregate(
  	fwdata$t,
  	by=list(size=fwdata$size),
  	FUN=sum
  )
  fwdata <- setNames(fwdata, c("size", "t"))
  fwdata
}

arma.filter_copy_to_host <- function (data) {
  data[data["routine"] == "harts_copy_to_host",]
}

arma.add_text <- function (data, str, pos=4, off=1) {
  len <- length(data$t)
	text(data$size[[len-1]], data$t[[len-1]], str, pos=pos, offset=off)
}

arma.plot_realtime_data <- function (data, ...) {
  args <- list(...)
  openmp <- arma.aggregate_by_size(data, "openmp")
  opencl <- arma.aggregate_by_size(data, "opencl")
  opengl <- arma.aggregate_by_size(arma.filter_copy_to_host(data), "opencl")
  plot.new()
  plot.window(
    xlim=range(c(openmp$size, opencl$size, opengl$size)),
    ylim=range(ceiling(c(0, openmp$t, opencl$t, opengl$t))))
  lines(openmp$size, openmp$t, lty="solid", type="b")
  lines(opencl$size, opencl$t, lty="dashed", type="b")
  lines(opengl$size, opengl$t, lty="dotted", type="b")
  axis(1, at=2^c(7:14))
  axis(2, at=c(0:8))
  box()
  arma.add_text(openmp, "OpenMP", pos=3, off=1.5)
  arma.add_text(opencl, "OpenCL", pos=3, off=1.5)
  arma.add_text(opengl, "OpenCL/OpenGL")
}

arma.filter_by_framework_and_size <- function (data, size, framework) {
  data <- data[data["framework"]==framework & data["size"] == size, ]
  data <- data[c("routine", "t")]
  rownames(data) <- c(1:length(data$t))
  data
}

arma.print_table_for_realtime_data <- function (data, routine_names, column_names) {
  par(family="serif")
  openmp <- arma.filter_by_framework_and_size(data, 2^14, "openmp")
  opencl <- arma.filter_by_framework_and_size(data, 2^14, "opencl")
  all_data <- merge(openmp, opencl, by="row.names")
  all_data <- all_data[c("routine.x", "t.x", "t.y")]
  all_data <- setNames(all_data, c("routine", "openmp", "opencl"))
  # remove non-existent data copying
  all_data[all_data$routine=="harts_copy_to_host", "openmp"] <- NA
  all_data$routine <- sapply(all_data$routine, function (c) get(c, routine_names))
  all_data <- setNames(all_data, column_names)
  ascii(all_data, include.rownames=FALSE, digits=4)
}

arma.load_bscheduler_data <- function (all_test_cases) {
	all_data = data.frame(
		framework=rep(NA,0),
		size=rep(NA,0),
		t=rep(NA,0)
	)
	row <- 1
	for (size in seq(10000, 30000, 2500)) {
		for (test_case in all_test_cases) {
			attempt <- test_case[[1]]
			framework <- test_case[[2]]
			hostname <- test_case[[3]]
			data <- arma.load_events(
				file.path(
					"build",
					"arma-benchmarks",
					"output",
					hostname,
					attempt,
					size,
					framework,
					"ar"
				),
				c("programme")
			)
			ev_prog <- data[data$event == "programme",]
			all_data[row, 'framework'] <- paste(attempt, framework, sep="-")
			all_data[row, 'size'] <- size
			all_data[row, 't'] <- mean(ev_prog$t1 - ev_prog$t0)*1e-6
			row <- row + 1
		}
	}
	all_data
}

arma.load_bscheduler_performance_data <- function() {
	all_test_cases <- list(c("a9-single-node-direct", "openmp", "m1"),
						   c("a9-single-node-direct", "bscheduler", "m1"),
						   c("a9-two-nodes-direct", "bscheduler", "m1"))
	arma.load_bscheduler_data(all_test_cases)
}

arma.load_master_slave_failure_data <- function() {
	all_test_cases <- list(c("a10-failure-direct-slave", "bscheduler", "m1"),
						   c("a9-single-node-direct", "bscheduler", "m1"),
						   c("a10-failure-direct-master", "bscheduler", "m1"))
	arma.load_bscheduler_data(all_test_cases)
}

arma.plot_bscheduler_performance_data <- function (all_data, names) {
	plot.new()
	plot.window(xlim=range(all_data$size), ylim=range(0,all_data$t))
	conf <- list(
		a=list(
			framework='a9-single-node-direct-openmp',
			color='#000000',
			lty="solid",
			lwd=1,
			name=names$openmp
		),
		b=list(
			framework='a9-single-node-direct-bscheduler',
			color='#000000',
			lty="dashed",
			lwd=1,
			name=names$bsc1
		),
		c=list(
			framework='a9-two-nodes-direct-bscheduler',
			color='#000000',
			lty="dotted",
			lwd=1,
			name=names$bsc2
		)
	)
	for (c in conf) {
		data <- all_data[all_data$framework==c$framework, ]
		lines(data$size, data$t, col=c$color, lty=c$lty)
		points(data$size, data$t, col=c$color)
	}
	legend(
		"bottomright",
		legend=sapply(conf, function (c) c$name),
		col=sapply(conf, function (c) c$color),
		lty=sapply(conf, function (c) c$lty),
		lwd=sapply(conf, function (c) c$lwd)
	)
	axis(1)
	axis(2)
	box()
}

arma.plot_master_slave_failure_data <- function (all_data, names) {
	plot.new()
	plot.window(xlim=range(all_data$size), ylim=range(0,all_data$t))
	conf <- list(
		a=list(
			framework='a10-failure-direct-master-bscheduler',
			color='#000000',
			lty="solid",
			lwd=1,
			name=names$master
		),
		b=list(
			framework='a10-failure-direct-slave-bscheduler',
			color='#000000',
			lty="dashed",
			lwd=1,
			name=names$slave
		),
		c=list(
			framework='a9-single-node-direct-bscheduler',
			color='#000000',
			lty="dotted",
			lwd=1,
			name=names$nofailures
		)
	)
	for (c in conf) {
		data <- all_data[all_data$framework==c$framework, ]
		lines(data$size, data$t, col=c$color, lty=c$lty)
		points(data$size, data$t, col=c$color)
	}
	legend(
		"bottomright",
		legend=sapply(conf, function (c) c$name),
		col=sapply(conf, function (c) c$color),
		lty=sapply(conf, function (c) c$lty),
		lwd=sapply(conf, function (c) c$lwd)
	)
	axis(1)
	axis(2)
	box()
}
