import math._
import scala.util._
import scala.io.StdIn._

/**
 * Auto-generated code below aims at helping you parse
 * the standard input according to the problem statement.
 **/
object Player extends App {

    var boostUsed: Boolean = false

    // game loop
    while(true) {
        // nextCheckpointX: x position of the next check point
        // nextCheckpointY: y position of the next check point
        // nextCheckpointDist: distance to the next checkpoint
        // nextCheckpointAngle: angle between your pod orientation and the direction of the next checkpoint
        val Array(x, y, nextCheckpointX, nextCheckpointY, nextCheckpointDist, nextCheckpointAngle) = (readLine split " ").map (_.toInt)
        val Array(opponentX, opponentY) = (readLine split " ").map (_.toInt)

        // To debug: Console.err.println("Debug messages...")

        val thrust = thrustValue(nextCheckpointDist, nextCheckpointAngle)

        // i.e.: "x y thrust"
        println(nextCheckpointX + " " + nextCheckpointY + " " + thrust)
    }

    def thrustValue(dist: Int, angle: Int): String = {
        if (boostConditions(dist, angle)) {
            boostUsed = true
            "BOOST"
        }
        else {
            val thrust =
                if (angle > 90 ||
                    angle < -90) 0
                else if (dist < 50) 0
                else if (dist < 100) 30
                else if (dist < 200) 60
                else 100
            thrust.toString
        }
    }

    def boostConditions(dist: Int, angle: Int): Boolean = {
        !boostUsed &&
            (dist > 500) &&
            angleLT(angle, 20)
    }

    def angleLT(angle: Int, max: Int): Boolean =
        -max < angle && angle < max

}
