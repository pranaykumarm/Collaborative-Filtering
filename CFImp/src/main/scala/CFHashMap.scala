/**
 * Created by bartimaeus on 8/1/14.
 */
class CFHashMap {

  var access_u = collection.mutable.HashMap[String, collection.mutable.HashMap[String, Int]]()
  var access_m = collection.mutable.HashMap[String, collection.mutable.HashMap[String, Int]]()
  var RP = collection.mutable.HashMap[String, collection.mutable.HashMap[String, Double]]()
  var tot_u = collection.mutable.HashMap[String, Int]()
  var tot_m = collection.mutable.HashMap[String, Int]()

  var U: Int = 0
  var M: Int = 0
  var U_mod: Int = 0
  var M_mod: Int = 0
  var total: Int = 0

  def upd_access(user: String, mach: String) {

    //updating access_u
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

    //updating access_m
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

    //updating total counts
    tot_u.update(user, tot_u.getOrElseUpdate(user, 0) + 1)
    tot_m.update(mach, tot_m.getOrElseUpdate(mach, 0) + 1)
    total += 1

  }

  def build_model() {

    RP = collection.mutable.HashMap[String, collection.mutable.HashMap[String, Double]]()

    for ((user1, machMap1) <- access_u) {
      val tot_user1 = tot_u(user1)
      RP += user1 -> collection.mutable.HashMap[String, Double]()

      for ((user2, machMap2) <- access_u) {
        var rp = 0.0

        for ((mach, cnt) <- machMap1) {
          rp += cnt * machMap2.getOrElse(mach, 0)*1.0 / tot_m(mach)
        }

        rp = rp / tot_user1
        if (rp != 0.0) {
          var userMap = RP(user1)
          userMap += user2 -> rp
        }
      }
    }

    U_mod = U
    M_mod = M
  }

  def score(user: String, mach: String): Double = {

    if (!RP.contains(user) || !tot_m.contains(mach)) {
      return -1.0
    }

    var sco = 0.0
    for ((user2, rp) <- RP(user)) {
      sco += rp * access_u(user2).getOrElse(mach, 0)*1.0 /tot_u(user2)
    }

    return sco
  }
}

object CFHashMap{

  def main(args: Array[String]) {

    val test: CFHashMap = new CFHashMap

    val user1 = "Pranay"
    val mach1 = "Caspida1"

    val user2 = "Prabhav"
    val mach2 = "Caspida1"

    val user3 = "Pranay"
    val mach3 = "Caspida2"

    test.upd_access(user1, mach1)
    test.upd_access(user2, mach2)
    test.upd_access(user3, mach3)
    println(test.access_u)
    println(test.access_m)
    println(test.tot_u)
    println(test.tot_m)

    test.build_model()
    println(test.RP)

    val f = test.score(user1, mach1)
    println(f)
  }
}