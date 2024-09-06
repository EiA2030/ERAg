#' Determine Practice Set Differences and Commonalities
#'
#' This function takes a raw ERA dataset and determines set differences (practices in the experimental but not the control treatment) and
#' commonalities (practices shared by experimental and control treatments) for each row of the data. Control `h` codes are removed from the comparison.
#'
#' In the raw dataset treatment practice codes are recorded in `T` columns (e.g., `T1`,`T2`, etc.) for the experimental condition and `C` columns
#' for the control condition.
#'
#' @param Data A raw ERA dataset.
#' @param cores The number of logical cores to use for parallel processing.
#' @param N.Cols The number of T or C columns in the dataset.
#' @return Two columns are appended to `Data`:
#' 1) `plist` = a **list** of practice codes in the experimental treatment not in the control treatment.
#' 2) `base.list` = a **vector** of practice codes shared by the experimental and control treatments, codes are concatenated with a `-` delimiter.
#' @export
#' @importFrom future.apply future_lapply
#' @importFrom data.table data.table
#' @import future
ExtractTreatment <- function(Data, cores, N.Cols) {
  # Convert Data to a data.frame
  Data <- data.frame(Data)

  # Set up future plan for parallel processing
  future::plan(future::multisession, workers = cores)

  # Parallel computation for 'plist' (set differences)
  Data$plist <- future.apply::future_lapply(1:nrow(Data), function(i) {
    cset <- Data[i, paste0("C", 1:N.Cols)]
    tset <- Data[i, paste0("T", 1:N.Cols)]
    cset <- cset[!is.na(cset)]
    tset <- tset[!is.na(tset)]
    Y <- as.character(unlist(setdiff(tset, cset)))
    Y[!Y == ""]
  })

  # Parallel computation for 'base.list' (commonalities)
  Data$base.list <- unlist(future.apply::future_lapply(1:nrow(Data), function(i) {
    cset <- Data[i, paste0("C", 1:N.Cols)]
    tset <- Data[i, paste0("T", 1:N.Cols)]
    cset <- cset[!is.na(cset)]
    tset <- tset[!is.na(tset)]
    Y <- as.character(unlist(intersect(tset, cset)))
    Y <- Y[!(Y == "" | is.na(Y))]
    paste(Y, collapse = "-")
  }))

  # Reset the plan back to sequential processing (optional)
  future::plan(future::sequential)

  return(data.table::data.table(Data))
}
