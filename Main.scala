
import java.io.FileNotFoundException
import scala.io.Source
import scala.util.control.{Breaks, NonFatal}
import Breaks.{break, breakable}
import scala.:+
/*import class Tondeuse*/
object Main extends App {
  def ftfgh(): Unit = {
    try {
    val bufferedSource = Source.fromFile("C:\\Users\\imsd\\IdeaProjects\\SCALA_IMSD\\src\\file.txt")

    val line = bufferedSource.getLines()
    var a=0
    val l=line.toList
    /*liste des tondeuses*/

    println(l.head)

      val x = l.head.split(" ").toList.head
      val y = l.head.split(" ").toList(1)
      val dimension_pelouse = List[Int](x.toInt, y.toInt)
      var parc_de_tondeuse = List[Tondeuse]()


    for (i<-0 to l.length-1) {

      print(i + "\n")
      a = i + 1 /*on compte le nombre de ligne*/
      println("Ligne numéro=" + a)
      if (a == 1) {
        /*intialisation de la pelouse, on récupère les dimensions de celle-ci qui sont sur la première ligne du fichier */

        val c = l.head
        val dimension_pelouse = List[Int](l.head.split(" ").toList.head.toInt/*première élément (transformé en Int) de la première ligne du texte
        transformée en ligne selon les espaces*/, l.head.split(" ").toList(1).toInt/*première élément (transformé en Int) de la première ligne du texte
         transformée en ligne selon les espaces*/)
        if (!IsInitialisation(dimension_pelouse)) {/*On teste la forme de la ligne d'intialisation.
         En cas d'erreur de type ou de forme, on sort de la fonction*/
          break()
        }

        println("Le terrain est de " + dimension_pelouse.head + " case de largeur et  " + dimension_pelouse(1) + " case de longueur") /*Liste composée des 2 éléments de la dimension de la pelouse*/




        print("Dimension pelouse:" + dimension_pelouse + "\n")
        print(c + "\n")

      }
      else if (a % 2 == 0) {

        val c = l(i).split(" ").toList/*c est la i+  ligne du fichier.
        On la transforme en liste en la séparant selon les espaces*/
        print("Nouvelle tondeuse " + c)
        if (!IsaTondeuse(c,dimension_pelouse.head,dimension_pelouse(1)) ) {break()}
        /*intialisation de la a/2-ième tondeuse, on récupère la position ainsi que l'orientation*/
        print("A")
         var tondeuse = new Tondeuse(c.head.toInt, c(1).toInt, c(2), dimension_pelouse.head, dimension_pelouse(1)) /*initialisationde la tondeuse*/

        parc_de_tondeuse = parc_de_tondeuse :+ tondeuse /*ajout de la tondeuse à la liste des tondeuse*/
        print("parc_de_tondeuse"+parc_de_tondeuse)

      }
      else {
        val c = l(i).toList
        print(c)
        if (!IsInstruction(c)) {
          /*On applique la fonction IsInstruction. Si elle renvoie
        False, alors on sort de la fonction*/

          break()

        }
        /*On récupère les instructions pour la tondeuse numéro a/2 et on exécute*/
        for (j <- c) {
          if (j != ' ') {

              println(parc_de_tondeuse(i-2).GetPosition())
              println(parc_de_tondeuse(i-2).GetOrientation())
              parc_de_tondeuse(i - 2).MouvementTondeuse(j)
              var (x, y) = parc_de_tondeuse(a - 2).GetPosition()
              if (x > dimension_pelouse.head || y > dimension_pelouse(1)) {
                /*Si la tondeuse est hors-limite
                * alors on stoppe*/
                println("La tondeuse est hors limite, elle est sortie de la case" + parc_de_tondeuse(i - 2).GetPosition + ",Orientée vers" + parc_de_tondeuse(i - 2).GetOrientation)

              }


          }
        }
        println("La tondeuse"+a/2+1+"est à la position: "+parc_de_tondeuse(i-2).GetPosition()+", orientée vers :" +parc_de_tondeuse(i-2).GetOrientation())

              }
    }} catch {case numberFormatException: NumberFormatException =>println("La ligne d'initialisation doit n'être composé que")
    case indexOutOfBoundsException : IndexOutOfBoundsException => println("Attention, la ligne d'initialisation de la pelouse doit être de la forme [Int Int],"+ "\n"+ "et les lignes d'initialisations des tondeuses de la forme [Int Int Str]")
    case matchError: MatchError  => println("Parc_de_tondeuse a -depuis une modif- la position et l'orientation qui s'annulent après une itération. Je ne sais pas pourquoi,impossible de résoudre le problème")
    case fileNotFoundException : FileNotFoundException => println("Le fichier est introuvable, vérifiez l'orthographe ou l'emplacement du fichier")
    }


  }
  def IsInitialisation(commande:List[Int]): Boolean ={
    /*Cette fonction teste si l'initialisation  de la pelouse  correspond à la forme standard*/

    if (commande.length!=2){/*on regarde si on a bien que 2 valeurs pour les dimensions de
    la pelouse sinon on renvoie faux*/
      println("Souci d'initialisation de ta pelouse, ton fichier a trop de paramètre en intialisation")
      return false
    }
    /*Si les paramêtres rentrés dans la liste d'intialisation sont bien 2 entiers, on vérifie qu'ils soient >0*/
     if (commande.head<0 && commande(1)<0){false}

true
  }
  def IsaTondeuse(commande:List[String],x_max:Int,y_max:Int): Boolean ={

    var coordonnées = List[Int](commande.head.toInt,commande(1).toInt)
    /*On transforme la position intiales en Int pour pouvoir les comparer*/
      if (coordonnées.head>x_max || coordonnées(1)>y_max){
        println("Erreur!!, la position initiale de la tondeuse est hors-limite!!")
        break()
      }

      if (coordonnées.head>0 && coordonnées(1)>0){
        return commande(2) =="N" || commande(2) =="S" || commande(2) =="E" || commande(2) =="W"
      }
      println("La position initiale n'est pas correct. x et y doivent être positifs")
      return false


    false
  }
  def IsInstruction(commande:List[Char]): Boolean ={
    for (i<-commande){
      if (i.getType!=1){
        /*si les commandes de la tondeuse ne sont pas des caractères, ça ne marche pas*/
        print(i.getType)
        println("Les instructions de la tondeuse ne sont pas du bon type, Elles doivent être des Char")
        return false
      }
      else{if (i!='A'&& i!='D'&& i!='G'){
        /*Si les instructions nes sont ni A,ni D,ni G ,alors on renvoie un avertissement et on sort de la fonction*/
        println("Les instructions de la tondeuse doivent être soit A,D ou G")
        return false
      }

      }
    }
true
  }
  ftfgh()

}
