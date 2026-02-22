package io.hookline

/** Error hierarchy for all HookLine client operations. */
sealed trait HookLineError extends Exception

object HookLineError:

  /** Resource not found (HTTP 404). */
  final case class NotFound(resource: String)
    extends Exception(s"Not found: $resource")
    with HookLineError

  /** Authentication failed (HTTP 401) or forbidden (HTTP 403). */
  final case class Unauthorized(message: String)
    extends Exception(s"Unauthorized: $message")
    with HookLineError

  /** Non-2xx response that is not 401/403/404. */
  final case class ApiError(status: Int, body: String)
    extends Exception(s"API error $status: $body")
    with HookLineError

  /** Network / transport failure (connection refused, timeout, etc.). */
  final case class NetworkError(cause: Throwable)
    extends Exception("Network error", cause)
    with HookLineError
