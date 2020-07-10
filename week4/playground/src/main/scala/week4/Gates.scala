package week4

abstract class Gates extends Simulation {

  def InverterDelay: Int
  def AndGateDelay: Int
  def OrGateDelay: Int

  class Wire {

    private var state = false
    private var actions: List[Action] = List()

    def getSignal: Boolean = state

    def setSignal(sig: Boolean): Unit =
      if (sig != state) {
        state = sig
        actions foreach (_())
      }

    def addAction(a: Action): Unit = {
      actions = a :: actions
      a()
    }
  }

  def probe(name: String, wire: Wire): Unit = {
    def probeAction(): Unit =
      println(s"$name $currentTime value = ${wire.getSignal}")
    wire addAction probeAction
  }

  def inverter(input: Wire, output: Wire): Unit = {
    def invertAction(): Unit = {
      val inputSig = input.getSignal
      afterDelay(InverterDelay) { output setSignal !inputSig }
    }
    input addAction invertAction
  }

  def and(in1: Wire, in2: Wire, output: Wire): Unit = {
    def andAction(): Unit = {
      val in1Sig = in1.getSignal
      val in2Sig = in2.getSignal
      afterDelay(AndGateDelay) { output setSignal (in1Sig & in2Sig)}
    }
    in2 addAction andAction
    in1 addAction andAction
  }

  def or(in1: Wire, in2: Wire, output: Wire): Unit = {
    def orAction(): Unit = {
      val in1Sig = in1.getSignal
      val in2Sig = in2.getSignal
      afterDelay(OrGateDelay) { output setSignal (in1Sig | in2Sig)}
    }
    in1 addAction orAction
    in2 addAction orAction
  }
}
