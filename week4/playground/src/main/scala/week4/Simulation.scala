package week4

trait Simulation {

  type Action = () => Unit

  case class Event(time: Int, action: Action)
  private type Agenda = List[Event]
  private var agenda: Agenda = List()

  private var currTime = 0
  def currentTime: Int = currTime

  def afterDelay(delay: Int)(block: => Unit): Unit = {
    val item = Event(currentTime + delay, () => block)
    agenda = insert(agenda, item)
  }

  private def insert(ag: List[Event], item: Event): List[Event] = ag match {
    case first :: tail if first.time <= item.time => first :: insert(tail, item)
    case _ => item :: ag
  }

  def run(): Unit = {
    afterDelay(0) {
      println("**simstarted***")
    }
  }

  private def loop(): Unit = agenda match {
    case first :: tail =>
      agenda = tail
      currTime = first.time
      first.action()
      loop()
    case Nil => ()
  }
}
