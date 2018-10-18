import exceptions._
import scala.collection.mutable

object TransactionStatus extends Enumeration {
  val SUCCESS, PENDING, FAILED = Value
}

class TransactionQueue {
    private val queue: mutable.Queue[Transaction] = mutable.Queue()

    // Remove and return the first element from the queue
    def pop: Transaction = queue.dequeue

    // Return whether the queue is empty
    def isEmpty: Boolean = queue.isEmpty

    // Add new element to the back of the queue
    def push(t: Transaction): Unit = queue.enqueue(t)

    // Return the first element from the queue without removing it
    def peek: Transaction = queue.front

    // Return an iterator to allow you to iterate over the queue
    def iterator: Iterator[Transaction] = queue.iterator
}

class Transaction(val transactionsQueue: TransactionQueue,
                  val processedTransactions: TransactionQueue,
                  val from: Account,
                  val to: Account,
                  val amount: Double,
                  val allowedAttemps: Int) extends Runnable {

  var status: TransactionStatus.Value = TransactionStatus.PENDING

  override def run: Unit = {
      def doTransaction() = {
          from withdraw amount
          to deposit amount
      }

      var tries = 0

      while (tries < allowedAttemps && status != TransactionStatus.SUCCESS) {
          try {
              if (from.uid < to.uid) from synchronized {
                  to synchronized {
                      doTransaction
                  }
              } else to synchronized {
                  from synchronized {
                      doTransaction
                  }
              }

              status = TransactionStatus.SUCCESS
          } catch {
              case _: Throwable => Thread.sleep(100)
          }

          tries += 1
      }

      if (status != TransactionStatus.SUCCESS) {
          status = TransactionStatus.FAILED
      }
  }
}
