#' Convert a Seurat Assay to a COO-formatted Data Frame
#'
#' @returns A `data.frame` with columns for the unnormalized (`counts`) and
#' normalized (`data`) data indexed by `feature`/`barcode` coordinates.
#'  @noRD
seurat_assay_to_dataframe <- function(object) {
  stopifnot(inherits(object, "Assay"))

  assay_data <- mapply(
    FUN = SeuratObject::GetAssayData,
    slot = c("counts", "data"),
    MoreArgs = list(object = object),
    SIMPLIFY = FALSE
  )

  assay_data <- lapply(assay_data, FUN = as, Class = "dgTMatrix")

  data.frame(
    feature = rownames(assay_data$counts)[assay_data$counts@i + 1],
    barcode = colnames(assay_data$counts)[assay_data$counts@j + 1],
    counts = assay_data$counts@x,
    data = assay_data$data@x
  )
}

