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
#' adjust SLURM and conda configuration to your case
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
    iteration = 1) {
    r.file <- paste0(working.dir, "/", r.file)
    sh.file <- paste0(working.dir, "/", sh.file)
    context <- rstudioapi::getSourceEditorContext()
    current_row <- context$selections[[1]]$range$start[[1]]
    writeLines(context$contents[1:(current_row - 2)], r.file)
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
