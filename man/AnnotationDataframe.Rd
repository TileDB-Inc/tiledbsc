% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AnnotationDataframe.R
\name{AnnotationDataframe}
\alias{AnnotationDataframe}
\title{Single-cell Annotation Data Frame}
\description{
Single-cell Annotation Data Frame

Single-cell Annotation Data Frame
}
\details{
Base class for data frames with rows aligned to the observations or features
of a \code{\link{SOMA}}. Used to store a heterogeneous collection of
annotations/measurements.
}
\section{Super classes}{
\code{\link[tiledbsc:TileDBObject]{tiledbsc::TileDBObject}} -> \code{\link[tiledbsc:TileDBArray]{tiledbsc::TileDBArray}} -> \code{\link[tiledbsc:AnnotationArray]{tiledbsc::AnnotationArray}} -> \code{AnnotationDataframe}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{verbose}}{Print status messages}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-AnnotationDataframe-ids}{\code{AnnotationDataframe$ids()}}
\item \href{#method-AnnotationDataframe-from_dataframe}{\code{AnnotationDataframe$from_dataframe()}}
\item \href{#method-AnnotationDataframe-to_dataframe}{\code{AnnotationDataframe$to_dataframe()}}
\item \href{#method-AnnotationDataframe-clone}{\code{AnnotationDataframe$clone()}}
}
}
\if{html}{\out{
<details><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="tiledbsc" data-topic="TileDBObject" data-id="class"><a href='../../tiledbsc/html/TileDBObject.html#method-TileDBObject-class'><code>tiledbsc::TileDBObject$class()</code></a></span></li>
<li><span class="pkg-link" data-pkg="tiledbsc" data-topic="TileDBObject" data-id="exists"><a href='../../tiledbsc/html/TileDBObject.html#method-TileDBObject-exists'><code>tiledbsc::TileDBObject$exists()</code></a></span></li>
<li><span class="pkg-link" data-pkg="tiledbsc" data-topic="TileDBArray" data-id="add_metadata"><a href='../../tiledbsc/html/TileDBArray.html#method-TileDBArray-add_metadata'><code>tiledbsc::TileDBArray$add_metadata()</code></a></span></li>
<li><span class="pkg-link" data-pkg="tiledbsc" data-topic="TileDBArray" data-id="array_exists"><a href='../../tiledbsc/html/TileDBArray.html#method-TileDBArray-array_exists'><code>tiledbsc::TileDBArray$array_exists()</code></a></span></li>
<li><span class="pkg-link" data-pkg="tiledbsc" data-topic="TileDBArray" data-id="attributes"><a href='../../tiledbsc/html/TileDBArray.html#method-TileDBArray-attributes'><code>tiledbsc::TileDBArray$attributes()</code></a></span></li>
<li><span class="pkg-link" data-pkg="tiledbsc" data-topic="TileDBArray" data-id="attrnames"><a href='../../tiledbsc/html/TileDBArray.html#method-TileDBArray-attrnames'><code>tiledbsc::TileDBArray$attrnames()</code></a></span></li>
<li><span class="pkg-link" data-pkg="tiledbsc" data-topic="TileDBArray" data-id="dimensions"><a href='../../tiledbsc/html/TileDBArray.html#method-TileDBArray-dimensions'><code>tiledbsc::TileDBArray$dimensions()</code></a></span></li>
<li><span class="pkg-link" data-pkg="tiledbsc" data-topic="TileDBArray" data-id="dimnames"><a href='../../tiledbsc/html/TileDBArray.html#method-TileDBArray-dimnames'><code>tiledbsc::TileDBArray$dimnames()</code></a></span></li>
<li><span class="pkg-link" data-pkg="tiledbsc" data-topic="TileDBArray" data-id="fragment_count"><a href='../../tiledbsc/html/TileDBArray.html#method-TileDBArray-fragment_count'><code>tiledbsc::TileDBArray$fragment_count()</code></a></span></li>
<li><span class="pkg-link" data-pkg="tiledbsc" data-topic="TileDBArray" data-id="get_metadata"><a href='../../tiledbsc/html/TileDBArray.html#method-TileDBArray-get_metadata'><code>tiledbsc::TileDBArray$get_metadata()</code></a></span></li>
<li><span class="pkg-link" data-pkg="tiledbsc" data-topic="TileDBArray" data-id="initialize"><a href='../../tiledbsc/html/TileDBArray.html#method-TileDBArray-initialize'><code>tiledbsc::TileDBArray$initialize()</code></a></span></li>
<li><span class="pkg-link" data-pkg="tiledbsc" data-topic="TileDBArray" data-id="print"><a href='../../tiledbsc/html/TileDBArray.html#method-TileDBArray-print'><code>tiledbsc::TileDBArray$print()</code></a></span></li>
<li><span class="pkg-link" data-pkg="tiledbsc" data-topic="TileDBArray" data-id="reset_query"><a href='../../tiledbsc/html/TileDBArray.html#method-TileDBArray-reset_query'><code>tiledbsc::TileDBArray$reset_query()</code></a></span></li>
<li><span class="pkg-link" data-pkg="tiledbsc" data-topic="TileDBArray" data-id="schema"><a href='../../tiledbsc/html/TileDBArray.html#method-TileDBArray-schema'><code>tiledbsc::TileDBArray$schema()</code></a></span></li>
<li><span class="pkg-link" data-pkg="tiledbsc" data-topic="TileDBArray" data-id="set_query"><a href='../../tiledbsc/html/TileDBArray.html#method-TileDBArray-set_query'><code>tiledbsc::TileDBArray$set_query()</code></a></span></li>
<li><span class="pkg-link" data-pkg="tiledbsc" data-topic="TileDBArray" data-id="tiledb_array"><a href='../../tiledbsc/html/TileDBArray.html#method-TileDBArray-tiledb_array'><code>tiledbsc::TileDBArray$tiledb_array()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-AnnotationDataframe-ids"></a>}}
\if{latex}{\out{\hypertarget{method-AnnotationDataframe-ids}{}}}
\subsection{Method \code{ids()}}{
Retrieves the values from the array's dimension.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{AnnotationDataframe$ids()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-AnnotationDataframe-from_dataframe"></a>}}
\if{latex}{\out{\hypertarget{method-AnnotationDataframe-from_dataframe}{}}}
\subsection{Method \code{from_dataframe()}}{
Ingest annotation data
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{AnnotationDataframe$from_dataframe(x, index_col)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{x}}{a \code{\link{data.frame}}}

\item{\code{index_col}}{Name to use for the TileDB array's dimension that will
contain the data.frame row names.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-AnnotationDataframe-to_dataframe"></a>}}
\if{latex}{\out{\hypertarget{method-AnnotationDataframe-to_dataframe}{}}}
\subsection{Method \code{to_dataframe()}}{
Retrieve the annotation data from TileDB
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{AnnotationDataframe$to_dataframe(attrs = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{attrs}}{A character vector of the attribute names to retrieve. By
default, all attributes are retrieved.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A \code{\link{data.frame}} with row names containing values from the index
dimension
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-AnnotationDataframe-clone"></a>}}
\if{latex}{\out{\hypertarget{method-AnnotationDataframe-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{AnnotationDataframe$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
