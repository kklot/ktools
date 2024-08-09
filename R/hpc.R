#' Monitor SLURM queue
#'
#' @export
squeue <- function(user = "knguyen", iteration = 1) {
    system(paste("squeue -i", iteration, "-u", user))
}

#' Submit SLURM job
#'
#' @export
sbatch <- function(file) {
    system(paste("sbatch", file))
}

#' Prepare the R script and sh file to submit to HPC managed with SLURM
#'
#' The workflow is: you're working on a R script, and every lines from the
#' top of the file (or from `first_line` parameter) to the current line (or the last
# `last.line` parameter) is working fine. Now you want to
#' run a full-blown model on the HPC. Then adjust the r script and run this
#' function after the last line you want to send to the cluster (or outside the
#' range from `first.line` to `last.line`.).
#'
#' Adjust SLURM and conda configuration to your case.
#'
#' @export
slurm <- function(
    r.file = "job.r",
    sh.file = paste0(basename(r.file), ".sh"),
    job.name = as.character(r.file),
    time = "1:00:00",
    ntask = 1,
    partition = "fuchs",
    nodes = 1,
    home = "/home/fuchs/fias/knguyen/",
    conda.env = "kinh",
    working.dir = ".",
    submit = FALSE,
    monitor = FALSE,
    user = "knguyen",
    iteration = 1, 
    first.line = 1,
    last.line = Inf, 
    shift.line = 2
    ) {
    context <- rstudioapi::getSourceEditorContext()
    r.file <- paste0(working.dir, "/", r.file)
    sh.file <- paste0(working.dir, "/", sh.file)
    current_row <- context$selections[[1]]$range$start[[1]]
    if (is.infinite(last.line)) {
        last.line <- current_row
        shift.line <- 0
    }
    writeLines(context$contents[first.line:(last.line - shift.line)], r.file)
    writeLines(
        c(
            "#!/bin/bash",
            paste0("#SBATCH --job-name=", job.name),
            paste0("#SBATCH --partition=", partition),
            paste0("#SBATCH --ntasks=", ntask),
            paste0("#SBATCH --nodes=", nodes),
            paste0("#SBATCH --time=", time),
            paste0("source", home, ".bashrc"),
            paste0("conda activate ", conda.env),
            paste0("cd ", working.dir),
            paste0("srun R CMD BATCH --no-save --no-restore ", r.file)
        ),
        sh.file
    )
    if (submit) {
        sbatch(sh.file)
        squeue(user, iteration)
    } else {
        message("run ", paste("sbatch(", sh.file, ") to do it."))
    }
}
