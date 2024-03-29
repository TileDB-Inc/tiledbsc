% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SOMA.R
\name{SCGroup}
\alias{SCGroup}
\title{Single-cell Group}
\description{
Class for representing the now-deprecated SCGroup object, which has been
renamed to \code{\link{SOMA}}.
}
\section{Super classes}{
\code{\link[tiledbsc:TileDBObject]{tiledbsc::TileDBObject}} -> \code{\link[tiledbsc:TileDBGroup]{tiledbsc::TileDBGroup}} -> \code{\link[tiledbsc:SOMA]{tiledbsc::SOMA}} -> \code{SCGroup}
}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{misc}}{An alias for \code{uns}.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-SCGroup-new}{\code{SCGroup$new()}}
\item \href{#method-SCGroup-clone}{\code{SCGroup$clone()}}
}
}
\if{html}{\out{
<details><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="tiledbsc" data-topic="TileDBObject" data-id="class"><a href='../../tiledbsc/html/TileDBObject.html#method-TileDBObject-class'><code>tiledbsc::TileDBObject$class()</code></a></span></li>
<li><span class="pkg-link" data-pkg="tiledbsc" data-topic="TileDBObject" data-id="exists"><a href='../../tiledbsc/html/TileDBObject.html#method-TileDBObject-exists'><code>tiledbsc::TileDBObject$exists()</code></a></span></li>
<li><span class="pkg-link" data-pkg="tiledbsc" data-topic="TileDBGroup" data-id="add_member"><a href='../../tiledbsc/html/TileDBGroup.html#method-TileDBGroup-add_member'><code>tiledbsc::TileDBGroup$add_member()</code></a></span></li>
<li><span class="pkg-link" data-pkg="tiledbsc" data-topic="TileDBGroup" data-id="add_metadata"><a href='../../tiledbsc/html/TileDBGroup.html#method-TileDBGroup-add_metadata'><code>tiledbsc::TileDBGroup$add_metadata()</code></a></span></li>
<li><span class="pkg-link" data-pkg="tiledbsc" data-topic="TileDBGroup" data-id="count_members"><a href='../../tiledbsc/html/TileDBGroup.html#method-TileDBGroup-count_members'><code>tiledbsc::TileDBGroup$count_members()</code></a></span></li>
<li><span class="pkg-link" data-pkg="tiledbsc" data-topic="TileDBGroup" data-id="get_member"><a href='../../tiledbsc/html/TileDBGroup.html#method-TileDBGroup-get_member'><code>tiledbsc::TileDBGroup$get_member()</code></a></span></li>
<li><span class="pkg-link" data-pkg="tiledbsc" data-topic="TileDBGroup" data-id="get_members"><a href='../../tiledbsc/html/TileDBGroup.html#method-TileDBGroup-get_members'><code>tiledbsc::TileDBGroup$get_members()</code></a></span></li>
<li><span class="pkg-link" data-pkg="tiledbsc" data-topic="TileDBGroup" data-id="get_metadata"><a href='../../tiledbsc/html/TileDBGroup.html#method-TileDBGroup-get_metadata'><code>tiledbsc::TileDBGroup$get_metadata()</code></a></span></li>
<li><span class="pkg-link" data-pkg="tiledbsc" data-topic="TileDBGroup" data-id="group_exists"><a href='../../tiledbsc/html/TileDBGroup.html#method-TileDBGroup-group_exists'><code>tiledbsc::TileDBGroup$group_exists()</code></a></span></li>
<li><span class="pkg-link" data-pkg="tiledbsc" data-topic="TileDBGroup" data-id="list_member_uris"><a href='../../tiledbsc/html/TileDBGroup.html#method-TileDBGroup-list_member_uris'><code>tiledbsc::TileDBGroup$list_member_uris()</code></a></span></li>
<li><span class="pkg-link" data-pkg="tiledbsc" data-topic="TileDBGroup" data-id="list_members"><a href='../../tiledbsc/html/TileDBGroup.html#method-TileDBGroup-list_members'><code>tiledbsc::TileDBGroup$list_members()</code></a></span></li>
<li><span class="pkg-link" data-pkg="tiledbsc" data-topic="TileDBGroup" data-id="list_object_uris"><a href='../../tiledbsc/html/TileDBGroup.html#method-TileDBGroup-list_object_uris'><code>tiledbsc::TileDBGroup$list_object_uris()</code></a></span></li>
<li><span class="pkg-link" data-pkg="tiledbsc" data-topic="TileDBGroup" data-id="list_objects"><a href='../../tiledbsc/html/TileDBGroup.html#method-TileDBGroup-list_objects'><code>tiledbsc::TileDBGroup$list_objects()</code></a></span></li>
<li><span class="pkg-link" data-pkg="tiledbsc" data-topic="TileDBGroup" data-id="print"><a href='../../tiledbsc/html/TileDBGroup.html#method-TileDBGroup-print'><code>tiledbsc::TileDBGroup$print()</code></a></span></li>
<li><span class="pkg-link" data-pkg="tiledbsc" data-topic="TileDBGroup" data-id="remove_member"><a href='../../tiledbsc/html/TileDBGroup.html#method-TileDBGroup-remove_member'><code>tiledbsc::TileDBGroup$remove_member()</code></a></span></li>
<li><span class="pkg-link" data-pkg="tiledbsc" data-topic="TileDBGroup" data-id="tiledb_group"><a href='../../tiledbsc/html/TileDBGroup.html#method-TileDBGroup-tiledb_group'><code>tiledbsc::TileDBGroup$tiledb_group()</code></a></span></li>
<li><span class="pkg-link" data-pkg="tiledbsc" data-topic="SOMA" data-id="add_seurat_dimreduction"><a href='../../tiledbsc/html/SOMA.html#method-SOMA-add_seurat_dimreduction'><code>tiledbsc::SOMA$add_seurat_dimreduction()</code></a></span></li>
<li><span class="pkg-link" data-pkg="tiledbsc" data-topic="SOMA" data-id="from_seurat_assay"><a href='../../tiledbsc/html/SOMA.html#method-SOMA-from_seurat_assay'><code>tiledbsc::SOMA$from_seurat_assay()</code></a></span></li>
<li><span class="pkg-link" data-pkg="tiledbsc" data-topic="SOMA" data-id="get_annotation_matrix_arrays"><a href='../../tiledbsc/html/SOMA.html#method-SOMA-get_annotation_matrix_arrays'><code>tiledbsc::SOMA$get_annotation_matrix_arrays()</code></a></span></li>
<li><span class="pkg-link" data-pkg="tiledbsc" data-topic="SOMA" data-id="get_annotation_pairwise_matrix_arrays"><a href='../../tiledbsc/html/SOMA.html#method-SOMA-get_annotation_pairwise_matrix_arrays'><code>tiledbsc::SOMA$get_annotation_pairwise_matrix_arrays()</code></a></span></li>
<li><span class="pkg-link" data-pkg="tiledbsc" data-topic="SOMA" data-id="get_seurat_dimreduction"><a href='../../tiledbsc/html/SOMA.html#method-SOMA-get_seurat_dimreduction'><code>tiledbsc::SOMA$get_seurat_dimreduction()</code></a></span></li>
<li><span class="pkg-link" data-pkg="tiledbsc" data-topic="SOMA" data-id="get_seurat_dimreductions_list"><a href='../../tiledbsc/html/SOMA.html#method-SOMA-get_seurat_dimreductions_list'><code>tiledbsc::SOMA$get_seurat_dimreductions_list()</code></a></span></li>
<li><span class="pkg-link" data-pkg="tiledbsc" data-topic="SOMA" data-id="reset_query"><a href='../../tiledbsc/html/SOMA.html#method-SOMA-reset_query'><code>tiledbsc::SOMA$reset_query()</code></a></span></li>
<li><span class="pkg-link" data-pkg="tiledbsc" data-topic="SOMA" data-id="set_query"><a href='../../tiledbsc/html/SOMA.html#method-SOMA-set_query'><code>tiledbsc::SOMA$set_query()</code></a></span></li>
<li><span class="pkg-link" data-pkg="tiledbsc" data-topic="SOMA" data-id="to_seurat_assay"><a href='../../tiledbsc/html/SOMA.html#method-SOMA-to_seurat_assay'><code>tiledbsc::SOMA$to_seurat_assay()</code></a></span></li>
<li><span class="pkg-link" data-pkg="tiledbsc" data-topic="SOMA" data-id="to_seurat_object"><a href='../../tiledbsc/html/SOMA.html#method-SOMA-to_seurat_object'><code>tiledbsc::SOMA$to_seurat_object()</code></a></span></li>
<li><span class="pkg-link" data-pkg="tiledbsc" data-topic="SOMA" data-id="to_single_cell_experiment"><a href='../../tiledbsc/html/SOMA.html#method-SOMA-to_single_cell_experiment'><code>tiledbsc::SOMA$to_single_cell_experiment()</code></a></span></li>
<li><span class="pkg-link" data-pkg="tiledbsc" data-topic="SOMA" data-id="to_summarized_experiment"><a href='../../tiledbsc/html/SOMA.html#method-SOMA-to_summarized_experiment'><code>tiledbsc::SOMA$to_summarized_experiment()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SCGroup-new"></a>}}
\if{latex}{\out{\hypertarget{method-SCGroup-new}{}}}
\subsection{Method \code{new()}}{
Create a new SCGroup.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SCGroup$new(uri, verbose = TRUE, config = NULL, ctx = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{uri}}{URI of the TileDB group}

\item{\code{verbose}}{Print status messages}

\item{\code{config}}{optional configuration}

\item{\code{ctx}}{optional tiledb context}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-SCGroup-clone"></a>}}
\if{latex}{\out{\hypertarget{method-SCGroup-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{SCGroup$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
