package lab4

case class Transaction(amt: Double, fromAcct: Int, toAcct: Int)

class ledger {

  def balance(acct: Int, ledger: List[Transaction]): Double =
    val neg = ledger.filter(_.fromAcct == acct).map(_.amt).reduce(_ + _)
    val pos = ledger.filter(_.toAcct == acct).map(_.amt).reduce(_ + _)
    pos-neg
}
