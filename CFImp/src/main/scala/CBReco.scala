/**
 * Created by bartimaeus on 8/5/14.
 */
class CBReco {

  var profile = collection.mutable.HashMap[String, collection.mutable.HashMap[String, Int]]()
  var total = collection.mutable.HashMap[String, Int]()

  def upd_profile(user: String, features: Array[String]){

    //updating profile
    if (profile.contains(user)) {
      val feat_pro = profile(user)

      for (i <- 0 until features.length){
        feat_pro.update(features(i), feat_pro.getOrElseUpdate(features(i), 0) + 1)
      }
    }

    else{
      profile += user -> collection.mutable.HashMap[String, Int]()
      var feat_pro = profile(user)

      for (i <- 0 until features.length){
        feat_pro += features(i) -> 1
      }
    }

    //updating total
    total.update(user, total.getOrElseUpdate(user, 0) + 1)
  }

  def score(user: String, features: Array[String], F: Int): Float = {

    if (!profile.contains(user)) return -1

    var sco: Double = 0
    for (i <- 0 until features.length){
      sco += profile(user).getOrElse(features(i), 0)
    }
    sco = (1.0 * sco)/ (total(user) * F)

    return sco.asInstanceOf[Float]
  }

}

object CBReco {

  def main(args: Array[String]) {

    val test = new CBReco

    val user1 = "Pranay"
    val features1 = Array("Caspida", "UCSD", "AP")
    val user2 = "Prabhav"
    val features2 = Array("Caspida", "UCSD", "Raj")

    test.upd_profile(user1, features1)
    println(test.profile)
    println(test.score(user2, features2, 3))
    test.upd_profile(user2, features2)
    println(test.profile)
    println(test.score(user1, features2, 3))
  }
}
