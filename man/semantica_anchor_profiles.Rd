\name{semantica_anchor_profiles}
\alias{semantica_anchor_profiles}
\title{Build an experimental anchor-profile embedding representation}
\usage{
semantica_anchor_profiles(embed_result, anchor_ids, id_col = "item_id",
  text_col = "item_text")
}
\arguments{
\item{embed_result}{A result from \code{\link{semantica_embed}} or
\code{\link{semantica_import_embeddings}} containing candidates and anchors
encoded together with a recorded model and revision.}
\item{anchor_ids}{Ordered IDs of at least three anchors with unique wording
distinct from the candidate items.}
\item{id_col, text_col}{Columns containing item IDs and exact encoded wording.}
}
\description{
Computes each candidate item's cosine similarities to the common anchor set,
then centers and normalizes each profile. Cosine similarity between transformed
items equals Pearson correlation between their anchor-similarity profiles.
Choose the anchor set before comparing validation outcomes. Reuse the same
anchors and encoder revision across pools for a fixed representation.
}
\details{
The result retains the original encoder identity. Anchor profiles share
the same encoder evidence as raw embeddings. Candidates with numerically
flat profiles produce an error.
Apply the transformation explicitly to opt in. Cleaner grouping alone
does not establish response reliability or independent corroboration.
}
\value{
A \code{semantica_embedding_result} for the candidate items. Its provenance
contains the encoder model and revision, exact anchor wording and order,
and a fingerprint of the normalized anchor vectors. Pass the result to
\code{\link{semantica_wrap}} and use the resulting cosine matrix and item
table with \code{\link{ACO_with_ESEM}}.
}
\seealso{\code{\link{semantica_import_embeddings}}, \code{\link{semantica_wrap}}}
