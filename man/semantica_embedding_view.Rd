\name{semantica_embedding_view}
\alias{semantica_embedding_view}
\title{Bind an alternate embedding view to its item wording and model}
\usage{
semantica_embedding_view(embeddings, item_texts, model, revision = NULL)
}
\arguments{
\item{embeddings}{Numeric item-by-feature matrix with unique item IDs as row names.}
\item{item_texts}{Exact encoded wording as a character vector named by item ID.}
\item{model}{Nonempty identifier for the encoder that produced the vectors.}
\item{revision}{Optional recorded revision of the encoder.}
}
\description{
Aligns embeddings by item ID, normalizes each row and computes item cosine
similarities. Pass the returned view as the matrix argument of
\code{\link{semantica_corroborate_structure}} to check selected wording and
record the source alongside PFA diagnostics. The caller declares the model
and revision.
}
\value{A \code{semantica_embedding_view} containing the cosine matrix, wording,
declared model and revision, and an artifact fingerprint.}
