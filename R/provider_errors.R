# Provider boundaries share one error contract across chat and embedding routes.
# Store plain, sanitized cause metadata rather than requests, responses or frames.
.semantica_error_text <- function(message, session = NULL) {
  value <- as.character(message)
  for (field in c("api_key", "embed_api_key", "hf_token", "embed_hf_token")) {
    secret <- session[[field]]
    if (is.character(secret) && length(secret) == 1L && !is.na(secret) && nzchar(secret))
      value <- gsub(secret, "[REDACTED]", value, fixed = TRUE)
  }
  urls <- regmatches(value, gregexpr("https?://[^[:space:]<>]+", value, perl = TRUE))
  for (url in unique(unlist(urls, use.names = FALSE)))
    value <- gsub(url, .semantica_sanitize_url(url), value, fixed = TRUE)
  value
}

.semantica_rethrow <- function(error, context, failure_stage, session = NULL, ...) {
  details <- .semantica_failure_details(error)
  details$condition_class <- details$message <- NULL
  additions <- list(...)
  for (field in names(additions)) details[field] <- additions[field]
  details$failure_stage <- details$failure_stage %||% failure_stage
  details$cause <- list(class = class(error), message = .semantica_error_text(conditionMessage(error), session))
  classes <- setdiff(class(error), c("error", "condition"))
  transport <- any(grepl("^httr2_failure$|^curl_error", classes))
  if (transport && !inherits(error, "semantica_error_config") && !inherits(error, "semantica_error_integrity"))
    classes <- unique(c("semantica_error_provider", classes))
  stop(.semantica_condition(paste0(context, ": ", details$cause$message),
    subclass = classes, fields = details))
}

.semantica_perform_request <- function(session, url, body, stage) {
  request <- .build_request(session, url, body) |>
    httr2::req_error(is_error = function(r) FALSE)
  tryCatch(httr2::req_perform(request), error = function(e)
    .semantica_rethrow(e, paste(stage, "request failed"), stage, session,
      backend = session$backend, protocol = session$protocol))
}

.semantica_check_http <- function(response, session, stage, label) {
  status <- httr2::resp_status(response)
  if (status < 400L) return(invisible(response))
  # Exhausted service/rate-limit failures can be ordinary attempted-unit failures.
  # Other HTTP errors need configuration/deployment review before spending more.
  temporary <- status %in% c(408L, 425L, 429L) || status >= 500L
  .semantica_abort(paste0(label, ": HTTP ", status),
    subclass = c("semantica_error_provider_http",
      if (temporary) "semantica_error_provider" else "semantica_error_config"),
    http_status = status, backend = session$backend, protocol = session$protocol,
    failure_stage = stage)
}

.semantica_response_json <- function(response, session, stage) {
  value <- tryCatch(httr2::resp_body_json(response, simplifyVector = FALSE), error = function(e)
    .semantica_abort(paste(stage, "response is not valid JSON."),
      subclass = c("semantica_error_provider_response", "semantica_error_integrity"),
      backend = session$backend, protocol = session$protocol, failure_stage = stage,
      cause = list(class = class(e), message = "JSON decoding failed")))
  if (!is.list(value)) .semantica_abort(paste(stage, "response must be a JSON object."),
    subclass = c("semantica_error_provider_response", "semantica_error_integrity"), failure_stage = stage)
  value
}

.semantica_embedding_response_error <- function(message) {
  .semantica_abort(message,
    subclass = c("semantica_error_embedding_response", "semantica_error_integrity"),
    failure_stage = "embedding")
}
