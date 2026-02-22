package io.hookline.bridge

import zio.*
import zio.http.Client
import zio.logging.backend.SLF4J

object Main extends ZIOAppDefault:

  override val bootstrap: ZLayer[ZIOAppArgs, Any, Any] =
    Runtime.removeDefaultLoggers >>> SLF4J.slf4j

  val run: ZIO[Any, Any, Any] =
    BridgeApp.start
      .provide(
        BridgeConfig.layer,
        HooklineClient.live,
        Client.default,
        Scope.default
      )
      .catchAll { err =>
        ZIO.logError(s"Bridge failed: ${err.getMessage}") *> ZIO.fail(err)
      }
