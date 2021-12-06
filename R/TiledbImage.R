#' TileDB Image Data
#'
#' An object to store and manipulate images in TileDB.
#'
#' @importFrom png readPNG
#' @importFrom tools file_ext
#' @export

TiledbImage <- R6::R6Class(
  classname = "TiledbImage",
  public = list(
    array_uri = NULL,
    image_path = NULL,
    verbose = TRUE,

    #' @description Create a new TiledbImage object. A new array is created if
    #' an `image_path` is provided, otherwise an existing array is opened at
    #' the specified URI.
    #' @param image_path File path for the image to ingest.
    initialize = function(array_uri, image_path = NULL, verbose = TRUE) {

      self$array_uri <- array_uri
      self$image_path <- image_path
      self$verbose <- verbose

      if (!is.null(self$image_path)) {
        stopifnot(file.exists(self$image_path))
        private$build_tiledb_image_array(self$array_uri)
      } else {
        stopifnot(
          `No array found at array_uri` = tiledb::tiledb_vfs_is_dir(self$array_uri)
        )
      }
    },

    #' @description Return a [`tiledbarray`] object
    #' @param ... Optional arguments to pass to `tiledb::tiledb_array`
    #' @return A [`tiledbarray`] object
    tiledb_array = function(...) {
      args <- list(...)
      args$uri <- self$array_uri
      args$query_type <- "READ"
      do.call(tiledb::tiledb_array, args)
    },

    metadata = function() {
      private$get_metadata(self$tiledb_array())
    }
  ),

  private = list(
    #' @description Top-level function to create and populate the new array.
    build_tiledb_image_array = function(array_uri) {
      image_data <- private$read_image_data()
      image_array <- private$create_image_array(
        array_uri,
        height = dim(image_data)[1],
        width = dim(image_data)[2],
        attrs = dimnames(image_data)[[3]]
      )
      private$ingest_image_data(array_uri, image_data)
    },

    #' @description Read image data from a file and return a 3D array with
    #' proper names assigned to the Z dimension, and any additional metadata
    #' stored as attributes.
    read_image_data = function() {

      if (self$verbose) message("Loading image data from ", self$image_path)

      image_data <- switch(
        tools::file_ext(self$image_path),
        "png" = png::readPNG(self$image_path, info = TRUE),
        stop("Unsupported image format")
      )

      # TODO: Generalize, currently assuming the image contains RGB data
      dimnames(image_data) <- list(NULL, NULL, c("red", "green", "blue"))
      return(image_data)
    },

    #' @description Create an empty TileDB array suitable for storing pixel
    #' data.
    #' @param height,width Height and width of the image channels.
    #' @param attrs Names of the TileDB attributes.
    create_image_array = function(array_uri, height, width, attrs) {

      # expecting names to accomodate a 2D array with 3 attributes
      stopifnot(length(attrs) == 3)
      if (self$verbose) message("Creating new array at ", array_uri)

      # TODO: Switch dims to UINT16 after bug retrieving UINT dims is fixed
      tdb_dims <- mapply(
          tiledb::tiledb_dim,
          name = c("x", "y"),
          domain = list(
              c(1L, height),
              c(1L, width)
          ),
          MoreArgs = list(
              type = "INT16",
              tile = 100
          )
        )

      tdb_attrs <- mapply(
        tiledb::tiledb_attr,
        name = attrs,
        MoreArgs = list(
          type = "FLOAT64",
          ncells = 1,
          filter_list = tiledb::tiledb_filter_list()
        )
      )

      tdb_schema <- tiledb::tiledb_array_schema(
        domain = tiledb::tiledb_domain(tdb_dims),
        attrs = tdb_attrs,
        cell_order = "ROW_MAJOR",
        tile_order = "ROW_MAJOR",
        sparse = FALSE,
        offsets_filter_list = tiledb::tiledb_filter_list()
      )

      tiledb::tiledb_array_create(array_uri, schema = tdb_schema)
    },

    #' @description Ingest image data into the TileDB array.
    #' @param image_data 3D array containing the image pixel data.
    ingest_image_data = function(array_uri, image_data) {
      stopifnot(
        "Image data must be an array" = is.array(image_data)
      )

      # convert array to a list of matrices suitable for ingestion
      image_list <- sapply(
        X = dimnames(image_data)[[3]],
        FUN = function(x) image_data[,,x],
        simplify = FALSE
      )

      if (self$verbose) message("Ingesting image into ", array_uri)
      tdb_array <- tiledb::tiledb_array(array_uri, query_type = "WRITE")
      tdb_array[] <- image_list

      # store additional image info as metadata
      private$add_metadata(metadata = attr(image_data, which = "info"))
      return(NULL)
    },

    #' @description Retrieve metadata from a TileDB array.
    #' @param arr A [`tiledb_array`] object.
    #' @param key The name of the metadata attribute to retrieve.
    #' @param prefix Filter metadata using an optional prefix. Ignored if `key`
    #'   is not NULL.
    #' @return A list of metadata values.
    get_metadata = function(arr = NULL, key = NULL, prefix = NULL) {
      if (is.null(arr)) {
        arr <- self$tiledb_array()
      }

      tiledb::tiledb_array_open(arr, "READ")
      if (!is.null(key)) {
        metadata <- tiledb::tiledb_get_metadata(arr, key)
      } else {
        metadata <- tiledb::tiledb_get_all_metadata(arr)
        if (!is.null(prefix)) {
          metadata <- metadata[string_starts_with(names(metadata), prefix)]
        }
      }
      tiledb::tiledb_array_close(arr)
      return(metadata)
    },

    #' @description Add specify list of metadata to the specified TileDB array.
    #' @param arr A [`tiledb_array`] object.
    #' @param metadata Named list of metadata to add.
    #' @param prefix Optional prefix to add to the metadata attribute names.
    #' @return NULL
    add_metadata = function(arr = NULL, metadata, prefix = "") {
      stopifnot(
        "Metadata must be a named list" = is.list(metadata) && is_named(metadata)
      )
      if (is.null(arr)) {
        arr <- self$tiledb_array()
      }
      tiledb::tiledb_array_open(arr, "WRITE")
      mapply(
        FUN = tiledb::tiledb_put_metadata,
        key = paste0(prefix, names(metadata)),
        val = metadata,
        MoreArgs = list(arr = arr),
        SIMPLIFY = FALSE
      )
      tiledb::tiledb_array_close(arr)
      return(NULL)
    }
  )
)

