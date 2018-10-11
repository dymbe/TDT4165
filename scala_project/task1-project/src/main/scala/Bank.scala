import scala.concurrent.forkjoin.ForkJoinPool
import java.util.concurrent.atomic.AtomicInteger

class Bank(val allowedAttempts: Integer = 3) {

    private val uid = new AtomicInteger()
    private val transactionsQueue: TransactionQueue = new TransactionQueue()
    private val processedTransactions: TransactionQueue = new TransactionQueue()
    private val executorContext = ???

    def addTransactionToQueue(from: Account, to: Account, amount: Double): Unit = {
      transactionsQueue push new Transaction(
        transactionsQueue, processedTransactions, from, to, amount, allowedAttempts)
    }

    // Hint: use a counter
    def generateAccountId: Int = uid.getAndIncrement()


        /*uidCount.synchronized {
        val newUid = uidCount + 1
        uidCount = newUid
        newUid
    }*/

    private def processTransactions: Unit = ???

    def addAccount(initialBalance: Double): Account = {
        new Account(this, initialBalance)
    }

    def getProcessedTransactionsAsList: List[Transaction] = {
        processedTransactions.iterator.toList
    }

}
