import java.io.File

import Environments.RootEnvironment

import scala.annotation.tailrec

object Utils {

  def topologicalSort[A](edges: Traversable[(A, A)], rootEnvironment: RootEnvironment): Iterable[A] = {
    var counter: Int = 0
    @tailrec
    def tsort(toPreds: Map[A, Set[A]], done: Iterable[A]): Iterable[A] = {
      val (noPreds, hasPreds) = toPreds.partition {
        _._2.isEmpty
      }
      if (noPreds.isEmpty) {
        if (hasPreds.isEmpty) done
        else {
          throw new Exception("Cycle detected: " + hasPreds.toString)
        }
      } else {
        val found = noPreds.keys
        tsort(hasPreds.mapValues {
          _ -- found
        }, done ++ found)
      }
    }

    val toPred = edges.foldLeft(Map[A, Set[A]]()) { (acc, e) =>
      acc + (e._1 -> acc.getOrElse(e._1, Set())) + (e._2 -> (acc.getOrElse(e._2, Set()) + e._1))
    }
    tsort(toPred, Seq())
  }


  def allFilesInDir(dir: String): Seq[File] = {
    val d = new File(dir)

    def recur(d: File): Seq[File] = {
      if (d.exists && d.isDirectory) {
        d.listFiles.filter(_.isFile).filter(_.getName.endsWith(".java")).toSeq ++
          d.listFiles.filter(_.isDirectory).flatMap(recur)
      } else if (d.exists && d.isFile) {
        Seq(d)
      } else {
        Seq.empty
      }
    }
    recur(d)
  }

}
