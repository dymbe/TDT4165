import java.util.concurrent.Executors

import scala.concurrent.forkjoin.ForkJoinPool
import java.util.concurrent.atomic.AtomicInteger

class Bank(val allowedAttempts: Integer = 3) {

    private val uid = new AtomicInteger()
    private val transactionsQueue: TransactionQueue = new TransactionQueue()
    private val processedTransactions: TransactionQueue = new TransactionQueue()
    private val executorContext = Executors.newCachedThreadPool()

    def addTransactionToQueue(from: Account, to: Account, amount: Double): Unit = {
      transactionsQueue synchronized {
          transactionsQueue push new Transaction(
              transactionsQueue, processedTransactions, from, to, amount, allowedAttempts)
      }

      processTransactions()
    }

    // Hint: use a counter
    def generateAccountId: Int = uid.getAndIncrement()

    private def processTransactions(): Unit = {
      executorContext.execute(() => {
        var transaction: Transaction = null

        transactionsQueue synchronized {
          transaction = transactionsQueue.pop
        }

        if (transaction != null) {
          transaction.run()

          processedTransactions synchronized {
            processedTransactions push transaction
          }
        }
      })
    }

    def addAccount(initialBalance: Double): Account = {
        new Account(this, initialBalance)
    }

    def getProcessedTransactionsAsList: List[Transaction] = {
        processedTransactions.iterator.toList
    }

}
