
import java.io._
import scala.io.BufferedSource
import java.util.Date

object Main {
  /**
   * historyMap: Collect the History File Object respect to the Customer ID
   * userMap: Collect the Customer Object respect to the Customer ID
   * adminMap: Collect the Admin Object respect to the Customer ID
   * historyMap: Collect the Product Object respect to the Customer ID
   */
  val historyMap: scala.collection.mutable.Map[Int, History] =
    scala.collection.mutable.Map()
  val userMap: scala.collection.mutable.Map[Int, Customer] =
    scala.collection.mutable.Map()
  val adminMap: scala.collection.mutable.Map[Int, Admin] =
    scala.collection.mutable.Map()
  val productMap: scala.collection.mutable.Map[Int, Product] =
    scala.collection.mutable.Map()

  def main(args: Array[String]): Unit = {

    val path1 = "/home/local/ZOHOCORP/pravin-pt4798/IdeaProjects/" +
      "BillingApps/inputs/credentials"
    val path2 = "/home/local/ZOHOCORP/pravin-pt4798/IdeaProjects/" +
      "BillingApps/inputs/input2"
    val input1 = scala.io.Source.fromFile(path1)
    val input2 = scala.io.Source.fromFile(path2)
    initOfCredentials(input1)
    initOfProducts(input2)
    var ch = 0
    do {
      login()
      println("=>Press 1 to continue")
      println("=>Press 0 to exit")
      ch = scala.io.StdIn.readInt()
    } while (ch != 0)
    val fileObj =
      new PrintWriter(new FileOutputStream(new File(path1)))
    fileObj.close()
    writeCredentials("Admin")
    writeCredentials("Customer")
    writeProductDetails()
  }

  /**
   * this method  load the Credentials to UserMap
   *
   * @param input1 Buffered Source of Credential file
   */
  def initOfCredentials(input1: BufferedSource): Unit = {
    for (i <- input1.getLines() if !i.contains("CusID")) {
      val temp = i.trim.split("\\s+")
      if (temp.length >= 5) {
        if (!temp(3).equalsIgnoreCase("Admin")) {
          try {
            val _ = scala.io.Source.fromFile("/home/local/" +
              "ZOHOCORP/pravin-pt4798/IdeaProjects/" +
              "BillingApps/outputs/History/History-" + temp(0).trim)
            val tempHis = new File("/home/local/" +
              "ZOHOCORP/pravin-pt4798/IdeaProjects/" +
              "BillingApps/outputs/History/History-" + temp(0).trim)
            userMap.put(temp(0).trim.toInt,
              new Customer(temp(0).trim.toInt, temp(1).trim,
              temp(2).trim, temp(3).trim, temp(4).trim))
            historyMap.put(temp(0).trim.toInt, new History(tempHis))
          }
          catch {
            case _: Exception =>
              val tempHis = new File("/home/local/ZOHOCORP" +
                "/pravin-pt4798/IdeaProjects/" +
                "BillingApps/outputs/History/History-" + temp(0).trim)
              userMap.put(temp(0).toInt, new Customer(temp(0).trim.toInt,
                temp(1).trim,
                encrypt(temp(2).trim), temp(3).trim, "PROMO10"))
              val hisObj = new History(tempHis)
              hisObj.createTemplate("Customer")
              historyMap.put(temp(0).trim.toInt, hisObj)
          }
        }
        else {
          try {
            val _ = scala.io.Source.fromFile("/home/local/ZOHOCORP" +
              "/pravin-pt4798/IdeaProjects/" +
              "BillingApps/outputs/History/History-" + temp(0).trim)
            val tempHis = new File("/home/local/ZOHOCORP" +
              "/pravin-pt4798/IdeaProjects/" +
              "BillingApps/outputs/History/History-" + temp(0).trim)
            adminMap.put(temp(0).trim.toInt, new Admin(
              temp(0).trim.toInt, temp(1).trim,
              temp(2).trim, temp(3).trim, temp(4).trim))
            historyMap.put(temp(0).trim.toInt, new History(tempHis))
          }
          catch {
            case _: Exception =>
              val tempHis = new File("/home/local/ZOHOCORP" +
                "/pravin-pt4798/IdeaProjects/" +
                "BillingApps/outputs/History/History-" + temp(0).trim)
              adminMap.put(temp(0).trim.toInt, new Admin(
                temp(0).trim.toInt, temp(1).trim,
                encrypt(temp(2).trim), temp(3).trim, temp(4).trim))
              val hisObj = new History(tempHis)
              hisObj.createTemplate("Admin")
              historyMap.put(temp(0).trim.toInt, hisObj)
          }
        }
      }
    }
  }

  /**
   * this method  load the Credentials to UserMap
   *
   * @param input2 Buffered Source of Product file
   */
  def initOfProducts(input2: BufferedSource): Unit = {
    for (i <- input2.getLines() if !i.contains("ProductID")) {
      val temp = i.trim.split("\\s+")
      productMap.put(temp(0).toInt, new Product(temp(0).trim.toInt,
        temp(1).trim,
        temp(2).trim, temp(3).trim.toInt, temp(4).trim.toInt,
        temp(5).trim))
    }
  }

  /**
   * this method perform Encryption of password
   *
   * @param password password to encrypt
   * @return encrypted password
   */
  def encrypt(password: String): String = {
    password.map(x => {
      if (x.equals('z')) 'a' else if (x.equals('Z')) 'A'
      else if (x.equals('9')) '0' else (x + 1).toChar
    })
  }

  /**
   * this method perform Decryption of password
   *
   * @param password password to decrypt
   * @return decrypted password
   */
  def decrypt(password: String): String = {
    password.map(x => {
      if (x.equals('a')) 'z' else if (x.equals('A')) 'Z' else if (x.equals('0')) '9'
      else (x - 1).toChar
    })
  }

  /**
   * this method  return the echo for password
   *
   * @param password password to echo
   * @return echoed string
   */
  def echo(password: String): String = {
    password.map(_ => "*").toString()
  }

  /**
   * this method get the fileName and return ID
   *
   * @param filename fileName of the customer
   * @return ID of the customer
   */
  def getID(filename: String): Int = {
    filename.trim.split("-").last.trim.toInt
  }

  /**
   * Login option are displayed to User
   */
  def login(): Unit = {
    var userID: String = ""
    var passWord: String = ""

    println("Online Billing Application")
    println("===========LOGIN============")

    println("=>Enter Your UserID:")
    userID = scala.io.StdIn.readLine()
    println("=>Enter Your Password:")
    passWord = scala.io.StdIn.readLine()
    println("============********===========")
    val cur = authentication(userID)

    if (cur != null) {
      if (findRole(cur).equalsIgnoreCase("Customer")) {
        if (decrypt(cur.asInstanceOf[Customer].
          password.trim).equals(passWord.trim)) {
          cur.asInstanceOf[Customer].performCusOperation()
        }
        else {
          println("Incorrect Password")
        }
      }
      else if (findRole(cur).equalsIgnoreCase("Admin")) {
        if (decrypt(cur.asInstanceOf[Admin].
          password.trim).equals(passWord.trim)) {
          cur.asInstanceOf[Admin].performAdminOperation()
        }
        else {
          println("Incorrect password")
        }
      }
    }
  }

  /**
   * This method Authenticates the Particular UserName
   *
   * @param userName UserName of the User
   * @return returns the Particular Applicant's Object
   */
  def authentication(userName: String): Object = {
    val db = userMap ++ adminMap
    for ((i, _) <- db) {

      if (authenticateHelper(db(i), userName)) {
        return db(i)
      }
    }
    println("UserName :" + userName + " not found! or Check " +
      "your Password")
    null
  }

  /**
   * This Method Helps the authentication to compare the UserName of
   * Actual with User Input
   *
   * @param anyObject Customer or Admin class Object
   * @param userName  User Input UserName
   * @return true if Same else false
   */
  def authenticateHelper(anyObject: Object, userName: String)
  : Boolean = {
    try {
      val obj = anyObject.asInstanceOf[Customer]
      if (obj.UserName.equals(userName)) {
        true
      }
      else {
        false
      }
    }
    catch {
      case _: ClassCastException =>
        val obj = anyObject.asInstanceOf[Admin]
        if (obj.UserName.equals(userName)) {
          true
        }
        else {
          false
        }
    }
  }

  /**
   * This Method help to Identify the Roles of User to Application
   *
   * @param anyObject Customer or Admin class Object
   * @return Role of the User
   */
  def findRole(anyObject: Object): String = {
    try {
      if (anyObject.asInstanceOf[Customer].
        role.equalsIgnoreCase("Customer")) {
        "Customer"
      }
      else ""
    }
    catch {
      case _: ClassCastException =>
        if (anyObject.asInstanceOf[Admin].
          role.equalsIgnoreCase("Admin")) {
          "Admin"
        }
        else ""
    }
  }

  /**
   * this Method returns the current Date for History
   *
   * @return Date
   */
  def getDate: String = {
    String.format("%1$s %2$tB %2$td, %2$tY", "",
      new Date())
  }

  /**
   * This Method Writes the Alter Data again to File
   *
   * @param string specifies the Role of Users
   */
  def writeCredentials(string: String): Unit = {
    val path1 = "/home/local/ZOHOCORP/pravin-pt4798/IdeaProjects/" +
      "BillingApps/inputs/credentials"
    val fileObj = new PrintWriter(new FileOutputStream(new File(path1),
      true))
    if (string.equalsIgnoreCase("Customer")) {
      fileObj.printf("%12s%30s%22s%20s%18s\n", "CusID",
        "UserName", "Password", "Role", "Discount")
      for ((_, y) <- userMap) {
        fileObj.println(String.format("%12d%30s%22s%20s%18s\n",
          Int.box(y.cusId), y.UserName, y.password, y.role, y.Discount))
      }
      fileObj.close()
    }
    else {
      fileObj.printf("%12s%30s%22s%20s%18s\n", "CusID",
        "UserName", "Password", "Role", "Discount")
      for ((_, y) <- adminMap) {
        fileObj.println(String.format("%12d%30s%20s%20s%18s\n",
          Int.box(y.cusId), y.UserName, y.password, y.role, y.Discount))
      }
      fileObj.close()
    }
  }

  /**
   * This Method Writes the Products Data again to File
   */
  def writeProductDetails(): Unit = {
    val path2 = "/home/local/ZOHOCORP/pravin-pt4798/IdeaProjects/" +
      "BillingApps/inputs/input2"
    val fileObj = new PrintWriter(new FileOutputStream(new File(path2),
      false))
    fileObj.printf("%12s%30s%22s%20s%18s\n", "ProductID",
      "Product", "Price", "Quantity", "Discount")
    for ((_, y) <- productMap) {
      fileObj.println(String.format("%12d%20s%22s%18d%18d%20s",
        Int.box(y.pid), y.productName, y.category, Int.box(y.price),
        Int.box(y.quantity), y.discount))
    }
    fileObj.close()
  }

  /**
   * This Method Writes the Every transaction done by Customer
   */
  def writeProductTrans(id: Int, amount: Double): Unit = {
    val path2 = "/home/local/ZOHOCORP/pravin-pt4798/IdeaProjects/" +
      "BillingApps/outputs/AllTransaction"
    val fileObj = new PrintWriter(new FileOutputStream(new File(path2),
      true))
    fileObj.println("------------------------------------------------------------------------------------------------------")
    fileObj.println("=>" + Main.getDate)
    fileObj.println("------------------------------------------------------------------------------------------------------")
    fileObj.println(Main.userMap(id))
    fileObj.println(String.format("%20s==>%.2f",
      "Amount Received=", Double.box(amount)))
    fileObj.println("------------------------------------------------------------------------------------------------------")
    fileObj.close()
  }
}
