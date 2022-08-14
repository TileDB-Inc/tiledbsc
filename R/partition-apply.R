#' Partition-wise Processing
#'
#' Apply a function to user specified partitions of assay data in a SOMA X
#' layer.
#'
#' Reads all values from either the `obs` or `var` dimension the `SOMA` and
#' divides them into `partition_count` partitions. If a query was previously
#' applied to the `SOMA`, then that subset of dimensions is partitioned instead.
#'
#' The function `fun` must accept a [`dgTMatrix`], which is how each partition
#' is read into memory.
#'
#' *Note setting `combine` to `"rbind"` or `"cbind"` uses the internal
#' helper functions `rbind_matrix()`/`cbind_matrix()`, respectively, which
#' pad matrices to ensure non-binding dimensions have the same length and,
#' importantly, the same order. See [`bind_matrix`] for more information.
#'
#' @param x A [`SOMA`] object.
#' @param fun The callback function to apply to each partition of data.
#' @param partition_dim Should the data be partitioned by `"obs"` or `"var"`?
#' @param partition_count The number of partitions to create.
#' @param partition_index Optional partition index. If provided only the indexed
#' partition is processed—useful distributing computations across multiple
#' nodes/workers.
#' @param layer The name of the `X` layer to apply the function to (defaults to
#' the first layer).
#' @param combine How should the partitioned results be combined? Can be one of:
#' - `NULL`: (default) return the list of partitioned results without combining
#' - `"c"`: concatenate results into a vector
#' - `"rbind"`: combine matrices by rows
#' - `"cbind"`: combine matrices by columns
#'
#' @param verbose Whether to print progress messages.
#' @export
partition_apply <- function(
  x,
  fun,
  partition_dim,
  partition_count,
  partition_index = NULL,
  layer = NULL,
  combine = NULL,
  verbose = TRUE,
  tiledbcloud_namespace = NULL
) {

  stopifnot(
    "'x' must be a SOMA" = inherits(x, what = "SOMA"),
    "'partition_dim' must be a scalar character" =
      is_scalar_character(partition_dim),
    "'partition_dim' must be either 'obs' or 'var'" =
      partition_dim %in% c("obs", "var"),
    "'partition_count' must be a scalar numeric" =
      is_scalar_numeric(partition_count),
    "'partition_count' must be greater than 0" =
      partition_count > 0,
    "Invalid 'combine' value" =
      is.null(combine) || combine %in% c("c", "rbind", "cbind")
  )

  if (!is.null(partition_index)) {
    stopifnot(
      "'partition_index' must be a scalar numeric" =
        is_scalar_numeric(partition_index),
      "'partition_index' must be between 1 and 'partition_count'" =
        partition_index > 0 && partition_index <= partition_count
    )
  }

  layers <- names(x$X$members)
  if (is.null(layer)) layer <- layers[1]
  if (!layer %in% layers) {
    stop("'layer' must be one of: ", string_collapse(layers), call. = FALSE)
  }

  # clone the AssayMatrix to avoid side-effect of attaching the selected
  # ranges to the parent soma
  assay_mat <- x$X$members[[layer]]$clone(deep = FALSE)
  assay_mat$verbose <- FALSE

  # Retrieve dimension values for partitioning
  dimname <- paste0(partition_dim, "_id")
  if (verbose) message(sprintf("Building '%s' partitions", partition_dim))
  dim_df <- x[[partition_dim]]

  # Check for existing selected ranges so previous queries are respected
  sliced_values <- tiledb::selected_ranges(dim_df$object)[[dimname]]
  if (is_empty(sliced_values)) {
    if (verbose) message(sprintf("...reading all values from '%s'", partition_dim))
    dim_values <- dim_df$ids()
  } else {
    if (verbose) message(sprintf("...reading sliced values from '%s'", partition_dim))
    dim_values <- sliced_values[, 1]
  }

  # create object-appropriate function for combining a list of results
  # primarily for overriding cbind/rbind to pad matrices prior to combining
  combine_fun <- switch(combine %||% "identity",
    cbind = cbind_matrix,
    rbind = rbind_matrix,
    `c` = "c",
    # return the results as is
    function(...) list(...)
  )

  stopifnot(
    "'partitition_count' exceeds the length of the dimension" =
      partition_count <= length(dim_values)
  )

  partitions <- split(
    x = dim_values,
    f = cut(seq_along(dim_values), breaks = partition_count, labels = FALSE)
  )

  # isolate specified partition index
  if (is.null(partition_index)) {
    part_msg <- sprintf(
      "Applying function to %d partitions",
      partition_count
    )
  } else {
    part_msg <- sprintf(
      "Applying function to partition %d of %d",
      partition_index,
      partition_count
    )
    partitions <- partitions[partition_index]
  }
  if (verbose) message(part_msg)

  if (!is.null(tiledbcloud_namespace)) {
    if (verbose) message("Initiating TileDB Cloud mode")
    results <- tiledb_cloud_partition_apply(
      uri = assay_mat$uri,
      fun = fun,
      dimname = dimname,
      partitions = partitions,
      combine_fun = combine_fun,
      namespace = tiledbcloud_namespace,
      verbose = verbose)
  } else {
    results <- local_partition_apply(
      x = assay_mat,
      fun = fun,
      dimname = dimname,
      partitions = partitions,
      combine_fun = combine_fun,
      verbose = verbose)
  }

  results
}


#' Serially loop through each partition and apply the function
#' @param x AssayMatrix
#' @param fun Function applied to each slice of the array
#' @param dimname Name of the dimension to partition
#' @param partitions list of character vectors where each element is a
#' partition
#' @param combine_fun Function to apply to the list of results
#' @noRd
local_partition_apply <- function(x, fun, dimname, partitions, combine_fun, verbose) {
  stopifnot(
    "'x' must be an AssayMatrix" = inherits(x, what = "AssayMatrix"),
    is_scalar_character(dimname),
    is.list(partitions)
  )

  results <- vector(mode = "list", length = length(partitions))

  for (i in seq_along(partitions)) {
    if (verbose) message(sprintf("...retrieving partition %d", i))
    partition <- setNames(list(partitions[[i]]), nm = dimname)
    x$set_query(dims = partition)
    mat <- x$to_matrix()
    if (verbose) {
      message(sprintf("...retrieved matrix with dims (%d, %d)", nrow(mat), ncol(mat)))
    }
    if (verbose) message(sprintf("...applying fun to partition %d", i))
    results[[i]] <- do.call(fun, args = list(mat))
  }
  if (verbose) message("Combining results")
  do.call(combine_fun, args = results)
}

#' Build and execute task graph to slice each partition from the array in
#' parallel and apply the supplied function.
#' @param uri URI for an AssayMat
tiledb_cloud_partition_apply <- function(uri, fun, dimname, partitions, combine_fun, namespace, verbose) {
  check_package("tiledbcloud")

  if (verbose) message("Retrieving TileDB Cloud user profile")

  # create delayed node for each partition
  partition_nodes <- vector(mode = "list", length = length(partitions))
  for (i in seq_along(partitions)) {
    partition_nodes[[i]] <- tiledbcloud::delayed(
      func = assay_matrix_apply,
      args = list(
        uri = uri,
        fun = fun,
        dims = setNames(list(partitions[[i]]), nm = dimname)
      ),
      # result_format = "native",
      namespace = namespace,
      name = sprintf("Matrix partition %i", i),
      local = FALSE
    )
  }

  combine_node <- tiledbcloud::delayed(
    func = combine_fun,
    args  = partition_nodes,
    namespace = namespace,
    name = "Combine results",
    local = FALSE
  )

  tiledbcloud::compute(
    node = combine_node,
    namespace = namespace,
    verbose = TRUE,
    force_all_local = FALSE
  )
}
