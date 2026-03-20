package fr.maif.daikoku.utils

import play.api.Logger

import scala.concurrent.{ExecutionContext, Future}

object Time {
  val logger = Logger("Tic-Tac")

  def time[R](block: => R, extraLog: String = ""): R = {
    val t0 = java.lang.System.nanoTime()
    val result = block // call-by-name
    val t1 = java.lang.System.nanoTime()
    logger.debug(s"Elapsed time [${extraLog}]: ${formatDuration(t1 - t0)}")
    result
  }

  def concurrentTime[R](block: => Future[R], extraLog: String = "")(implicit ex: ExecutionContext): Future[R] = {
    val t0 = java.lang.System.nanoTime()
    block.andThen { case _ =>
      val t1 = java.lang.System.nanoTime()
      logger.debug(s"Elapsed time [$extraLog]: ${formatDuration(t1 - t0)}")
    }
  }

  private def formatDuration(ns: Long): String = {
    if (ns < 1_000) s"${ns} ns"
    else if (ns < 1_000_000) f"${ns / 1_000.0}%.3f µs"
    else if (ns < 1_000_000_000) f"${ns / 1_000_000.0}%.3f ms"
    else if (ns < 60_000_000_000L) f"${ns / 1_000_000_000.0}%.3f s"
    else {
      val seconds = ns / 1_000_000_000.0
      val minutes = (seconds / 60).toInt
      val remaining = seconds % 60
      f"${minutes}m ${remaining}%.2fs"
    }
  }
}
