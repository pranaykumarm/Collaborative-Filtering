

package com.caspida.algorithms.generic.collabfiltering


import java.util
import java.util.ArrayList

import org.slf4j.{LoggerFactory, Logger}

import com.caspida.algorithms.security.clusters.CounterModel
import com.caspida.algorithms.generic.clusters.AlgorithmFlow

import scala.util.parsing.json.JSONObject


class CollabFilteringFlow (filter : String, training_count : Int, window : Int, options : Int)
  extends AlgorithmFlow (filter : String, training_count : Int, window : Int, options : Int) with Serializable {

  val modelName = "collabFiltering"

  var aggressive = if (options == 1) true else false
  aggressive = false

  /* do not raise duplicate anomalies */
  var anomalyMap = collection.mutable.Map[String, Integer]()

  logger.info("aggression: {}", aggressive)

  logger.info("instantiating CF")
  val CF = new CFHashMap()
  //logger.info("calling build_model with nothing")
  //CF.build_model()

  var scores = Array[Float](0)

  val build_win: Int = window
  val bins: Int = 1000
  val use_bins: Int = 10
  val perc = 0.0005

  var thr: Float = 0

  override def compute(line: String, counter: Int, training: Boolean): String = {

    if (line == "") return ("")

    val json = CounterModel.getMyJSON(line)
    val stream = json.get("stream")
    val A = json.get("valueA")
    val B = json.get("valueB")
    val date = json.get("date")

    //logger.info("calling upd_access when counter {}",counter)
    //CF.upd_access(A,B)

    if (counter % build_win == 0) {

      logger.info("calling breaks when counter {}",counter)
      thr = CF.breaks(scores, build_win, bins, use_bins, perc)

      logger.info("calling cleanup at counter {}", counter)
      CF.cleanup()

      scores = new Array[Float](0)

      /*logger.info("calling build_model when counter {}",counter)
      CF.build_model()
      logger.info("window, counter {} training {}", counter, training)*/

    }

    line
  }


  override def check(line: String, counter: Int, training: Boolean): String = {

    var returnVal: String = ""

    if (line == "") return line

    val json = CounterModel.getMyJSON(line)
    val stream = json.get("stream")
    val A = json.get("valueA")
    val B = json.get("valueB")
    val date = json.get("date")

    val f = CF.score(A,B)
    logger.debug("score_fac {} counter {}", f, counter)

    if (f == -1){
      logger.debug("New event at counter {}", counter)

      CF.upd_access(A,B)

      if (aggressive){
        logger.info("New event anomaly at counter {}", counter)

        //returnVal = "user "+A+ " accessing machine "+B+" is anomalous"

        val outMap = Map(
          "date" -> date,
          "stream" -> stream,
          "key" -> A,
          "score" -> 1,
          "B" -> B,
          "description" -> (" new user/machine ")
        )

        val returnJson = new JSONObject(outMap)
        returnVal += returnJson.toString() + "\n"
      }
     }

    else {

      val key = A + "::" + B

      // anomalous event
      if (f < thr) {

        if (!anomalyMap.contains(key)) {
          logger.info("Anomaly at counter {} with score {}", counter, f)

          /* modulate the score based on how far off the threshold we are */
          val score = 1 + (4 * (thr - f) / thr).floor.toInt

          val outMap = Map(
            "date" -> date,
            "stream" -> stream,
            "key" -> A,
            "score" -> score,
            "B" -> B,
            "description" -> (" profile mismatch ")
          )

          anomalyMap += key -> 1

          val returnJson = new JSONObject(outMap)
          returnVal += returnJson.toString() + "\n"
        }
      }

      // update the tables with the normal event
      else{
        CF.upd_access(A,B)
      }

      // add new edge scores to the list to compute breaks
      if (f!=2) scores = scores :+ f

    }

    logger.debug("Checking stream {} A {} B {}", stream, A, B)


    returnVal
  }
}

class CFHashMap extends Serializable {

  var access_u = collection.mutable.HashMap[String, collection.mutable.HashMap[String, Int]]()
  var access_m = collection.mutable.HashMap[String, collection.mutable.HashMap[String, Int]]()
  var RP = collection.mutable.HashMap[String, collection.mutable.HashMap[String, Float]]()
  var tot_u = collection.mutable.HashMap[String, Int]()
  var tot_m = collection.mutable.HashMap[String, Int]()

  var U: Int = 0
  var M: Int = 0
  var U_mod: Int = 0
  var M_mod: Int = 0
  var total: Int = 0

  val rp_thr = 0.0001
  var sco_thr = 0.0

  final val logger: Logger = LoggerFactory.getLogger(classOf[CFHashMap])

  def upd_access(user: String, mach: String) {

    // updating access_u
    if (access_u.contains(user)) {
      val machMap = access_u(user)
      machMap.update(mach, machMap.getOrElseUpdate(mach, 0) + 1)
    }
    else {
      access_u += user -> collection.mutable.HashMap[String, Int]()
      var machMap = access_u(user)
      machMap += mach -> 1

      U += 1
    }

    // updating access_m
    if (access_m.contains(mach)) {
      val userMap = access_m(mach)
      userMap.update(user, userMap.getOrElseUpdate(user, 0) + 1)
    }
    else {
      access_m += mach -> collection.mutable.HashMap[String, Int]()
      var userMap = access_m(mach)
      userMap += user -> 1

      M += 1
    }

    // updating total counts
    tot_u.update(user, tot_u.getOrElseUpdate(user, 0) + 1)
    tot_m.update(mach, tot_m.getOrElseUpdate(mach, 0) + 1)
    total += 1

  }

  def breaks(scores: Array[Float], build_win: Int, bins: Int, use_bins: Int, perc: Double): Float = {

    val N = scores.length

    if (N == 0) return 0

    val min = scores.min
    val l = (scores.max - min)/bins

    var counts = new Array[Int](0)
    for (i <- 1 to use_bins){
      counts = counts :+ 0
    }

    var b = 0

    for (i <- 0 until N){
      b = ((scores(i) - min)/l).asInstanceOf[Int]
      if (b<10){
        counts(b) += 1
      }
    }

    val k = counts.indexOf(counts.min)

    var sum = 0
    var p = 0
    while (sum < build_win*perc && p < use_bins){
      sum += counts(p)
      p += 1
    }

    val break: Float = min + l*(math.min(k+1, p))

    logger.info("computed breaks {}", break)
    return break

  }

  def cleanup() {
    RP = collection.mutable.HashMap[String, collection.mutable.HashMap[String, Float]]()
  }

  def score(user: String, mach: String): Float = {

    // new user/machine
    if (!tot_u.contains(user) || !tot_m.contains(mach)) return -1

    // user-machine pair already seen
    if (access_u(user).contains(mach)) return 2

    // else
    if (!RP.contains(user)) {
      val rp_user = collection.mutable.HashMap[String, Float]()
      logger.debug("building RP with users {} machines {}", U, M)
      for ((mach2, cnt_u) <- access_u(user)) {
        val tot_mach2 = tot_m(mach2)
        for ((v, cnt_v) <- access_m(mach2)) {
          rp_user.update(v, rp_user.getOrElseUpdate(v, 0) + ((cnt_u * cnt_v * 1.0) / tot_mach2).asInstanceOf[Float])
        }
      }

      RP += user -> rp_user
    }


    var sco = 0.0
    for ((v, rp) <- RP(user)) {
      sco += (rp * access_u(v).getOrElse(mach, 0) * 1.0) /tot_u(v)
    }


    return (sco/tot_u(user)).asInstanceOf[Float]
  }
}
