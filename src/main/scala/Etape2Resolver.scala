import fr.ippon.rh.permut.impl._

/**
 *
 *
 * @author dwu
 */
object Etape2Resolver {

  sealed trait Rule {
    def poid: Int

    def transformation(txt: String): String

    def isValid(txt: String): Boolean
  }

  class Rule1 extends Rule {
    def poid = 1

    def transformation(txt: String): String = txt + "U"

    def isValid(txt: String) = txt.endsWith("I")
  }

  class Rule2 extends Rule {
    def poid = 2

    def transformation(txt: String): String = txt + txt.substring(1)

    def isValid(txt: String) = txt.startsWith("M")
  }

  class Rule3 extends Rule {
    def poid = 3

    def transformation(txt: String): String = txt.replace("III", "U")

    def isValid(txt: String) = txt.contains("III")
  }


  class Rule4 extends Rule {
    def poid = 4

    def transformation(txt: String): String = txt.replace("UU", "")

    def isValid(txt: String) = txt.contains("UU")
  }


  def main(args: Array[String]) {

    println(new Rule1().transformation("MI") + " == MUI ?")
    println(new Rule2().transformation("MIU") + " == MIUIU ?")
    println(new Rule3().transformation("MUIIIU") + " == MUUU ?")
    println(new Rule3().transformation("MIIII") + " == MUI  ?")
    println(new Rule4().transformation("MUUU") + " == MU  ?")
    println(new Rule4().transformation("MUUUU") + " == M ?")

    val encoded: String = "rudNp !"


    val encoder: List[Pair[Int, BaseEncodeService]] = List((1, new EncodeService1()), (2, new EncodeService2()), (3, new EncodeService3()), (4, new EncodeService4()))

    def decodeThis(txt: String, dec: List[Pair[Int, BaseEncodeService]]): List[Pair[Int, BaseEncodeService]] = (txt, dec) match {
      case ("Bravo !", result) => result
      case (_, result) if result.length > 10 => List()
      case (s, d) => encoder.map(p => decodeThis(p._2.decode(s), d ::: List(p))).filter(l => !l.isEmpty) match {
        case head :: Nil => head
        case _ => List()

      }
    }
    // c'est comme ça qu'on a gagné la Second Guerre mondiale !
    decodeThis(encoded, List()).foreach(p => println(p._1))


    val rules = List(new Rule1(), new Rule2(), new Rule3(), new Rule4())
    val worstList = List(new Rule4(), new Rule4(), new Rule4(), new Rule4(), new Rule4(), new Rule4(), new Rule4(), new Rule4())
    def findSolution0(txt: String, acc: List[Rule]): List[Rule] = (txt, acc) match {
      case ("MUIIU", result) => {
        println("op, trouvé ! ")
        result
      }
      case (_, result) if result.length > 7 => worstList
      case (s, result) => rules.map(r => {
        if (r.isValid(s)) {
          findSolution0(r.transformation(s), result ::: List(r))
        } else {
          worstList
        }
      }).minBy(subList => subList.foldLeft(0)((x, y) => x + y.poid))

    }

    def findSolution(): List[Rule] = {
      findSolution0("MI", List())
    }

    findSolution().foreach(r => println("rule" + r.poid))


  }

}
