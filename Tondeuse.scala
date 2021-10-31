

class Tondeuse(x: Int, y: Int, o:String,x_max: Int, y_max: Int) extends App{
  /*x:abscisse ;y:ordonnée de la position;o : orientation intiale; x_max+y_max: dimension de la pelouse*/
  if (x>x_max || y>y_max){
    print("La tondeuse commence en dehors de la pelouse, qu'as-tu fait??")

  }
  private var position = (x,y)
  private var orientation = o

  def MouvementTondeuse(instruction: Char): Unit = {/*Modifie la position ou l'orientation de la
  tondeuse selon l'instruction */
    instruction match {/*On regarde si instruction est A,G ou D*/
      /*Si l'instruction est d'avancer (A), on modifie la position de la tondeuse selon son orientation*/
      case 'A' => this.orientation match {
        case "N" => position = (x, y + 1)
        case "E" => position = (x + 1, y)
        case "W" => position = (x - 1, y)
        case "S" => position = (x, y - 1)
      }
      /*Si l'instruction est de tourner à gauche, alors on remplace l'orientation par l'orientation adéquate
      N=>W;W=>S;S=>E;E=>N*/
      case 'G' => this.orientation match{
        case "N" => orientation= "W"
        case "E" => orientation = "N"
        case "W" => orientation = "S"
        case "S"=> orientation = "E"
      }/*Pareil que pour G mais avec la rotation opposée*/
      case 'D' => this.orientation match{
        case "N" => orientation= "E"
        case "E" => orientation = "S"
        case "W" => orientation = "N"
        case "S" => orientation = "W"
      }
      case _ => println("Instruction non conforme")

    }

  }
def GetPosition():(Int,Int)={/*renvoie la position de la tondeuse*/
 position
}
  def GetOrientation():String={/*renvoie l'orientation' de la tondeuse*/
    orientation
  }
}

