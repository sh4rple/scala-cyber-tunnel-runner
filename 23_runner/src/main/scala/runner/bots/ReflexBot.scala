package runner.bots

import runner.*
import scala.util.Random

/**
 * Рефлексивний бот (поки що на випадковій моделі моторики).
 * Студенти замінюють цю реалізацію на чисті патерн-матчинги в Лекції 2.
 */
class ReflexBot(seed: Long = 202L) extends CyberBot:
  private val rng = new Random(seed)

  override def name: String = "ReflexBot"

  override def decide(observation: Observation): Action =
  observation.upcoming.headOption match
    case None =>
      Action.KeepRunning

    case Some(slice) =>
      val hero = observation.hero

      val low = slice.itemAt(hero.lane, Height.Low) == CellItem.Obstacle
      val mid = slice.itemAt(hero.lane, Height.Mid) == CellItem.Obstacle
      val high = slice.itemAt(hero.lane, Height.High) == CellItem.Obstacle

      def isSafe(lane: Lane): Boolean =
        slice.itemAt(lane, Height.Low) != CellItem.Obstacle &&
        slice.itemAt(lane, Height.Mid) != CellItem.Obstacle

      val canMoveLeft = hero.lane match
        case Lane.Left => false
        case Lane.Center => isSafe(Lane.Left)
        case Lane.Right => isSafe(Lane.Center)
      
      val canMoveRight = hero.lane match
        case Lane.Left => isSafe(Lane.Center)
        case Lane.Center => isSafe(Lane.Right)
        case Lane.Right => false

      if (!low && !mid) then
        Action.KeepRunning

      else if (!low) then
        Action.Duck

      else if (!high) then
        Action.Jump

      else if (canMoveLeft) then
        Action.MoveLeft

      else if (canMoveRight) then
        Action.MoveRight

      else
        Action.KeepRunning
