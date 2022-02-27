package lab4

class ledger {

  def balance(acct: Int, ledger: List[Transaction]): Double =
    val neg = ledger.filter(_.fromAcct == acct).reduce(_+_)
    val pos = ledger.filter(_.toAcct == acct).reduce(_+_)
    pos-neg

}

class Transaction(val amt: Double, fromAcct: Int, toAcct: Int)
