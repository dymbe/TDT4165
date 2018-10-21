import java.util.NoSuchElementException

import akka.actor._
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration._
import akka.util.Timeout

case class GetAccountRequest(accountId: String)

case class CreateAccountRequest(initialBalance: Double)

case class IdentifyActor()

class Bank(val bankId: String) extends Actor {

    val accountCounter = new AtomicInteger(1000)

    def createAccount(initialBalance: Double): ActorRef = {
        // Should create a new Account Actor and return its actor reference. Accounts should be assigned with unique ids (increment with 1).
        val accountId = accountCounter.incrementAndGet().toString
        BankManager.createAccount(accountId, bankId, initialBalance)
    }

    def findAccount(accountId: String): Option[ActorRef] = {
        // Use BankManager to look up an account with ID accountId
        try {
            Some(BankManager.findAccount(bankId, accountId))
        } catch {
            case _: NoSuchElementException => None
        }
    }

    def findOtherBank(bankId: String): Option[ActorRef] = {
        // Use BankManager to look up a different bank with ID bankId
        try {
            Some(BankManager.findBank(bankId))
        } catch {
            case _: NoSuchElementException => None
        }
    }

    override def receive = {
        case CreateAccountRequest(initialBalance) => sender ! createAccount(initialBalance) // Create a new account
        case GetAccountRequest(id) => sender ! findAccount(id) // Return account
        case IdentifyActor => sender ! this
        case t: Transaction => processTransaction(t)

        case t: TransactionRequestReceipt =>
            // Forward receipt
            val to = t.toAccountNumber
            val isInternal = to.length <= 4
            val toBankId = if (isInternal) bankId else to.substring(0, 4)
            val toAccountId = if (isInternal) to else to.substring(4)

            if (isInternal || toBankId == bankId) {
                findAccount(toAccountId) match {
                    case Some(account) => account ! t
                    case None => throw new Exception(s"No account with id ${toAccountId} in bank $bankId")
                }
            } else {
                findOtherBank(toBankId) match {
                    case Some(bank) => bank ! t
                    case None => throw new Exception(s"No bank with id $toBankId")
                }
            }

        case msg => ???
    }

    def processTransaction(t: Transaction): Unit = {
        implicit val timeout = new Timeout(5 seconds)
        val isInternal = t.to.length <= 4
        val toBankId = if (isInternal) bankId else t.to.substring(0, 4)
        val toAccountId = if (isInternal) t.to else t.to.substring(4)
        val transactionStatus = t.status
        
        // This method should forward Transaction t to an account or another bank, depending on the "to"-address.
        // HINT: Make use of the variables that have been defined above.
        if (isInternal || toBankId == bankId) {
            findAccount(toAccountId) match {
                case Some(account) => account ! t
                case None =>
                    t.status = TransactionStatus.FAILED
                    sender() ! TransactionRequestReceipt(t.from, t.id, t)
            }
        } else {
            findOtherBank(toBankId) match {
                case Some(bank) => bank ! t
                case None =>
                    t.status = TransactionStatus.FAILED
                    sender() ! TransactionRequestReceipt(t.from, t.id, t)
            }
        }
    }
}