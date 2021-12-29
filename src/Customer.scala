class Customer(id: Int, name: String, pass: String, roles: String,
               Dis: String) {
  /**
   *Customer Class Constructor
   */
  val cusId: Int = id
  val UserName: String = name
  var password: String = pass
  val Discount: String = Dis
  val role: String = roles

  /**
   * this Method prints the Customer Information
   * @return Returns All Information of Object as a String
   */
  override def toString: String = "CustomerID => " + cusId + "\n" +
    "UserName => " + UserName + "\n" + "Password=>" +
    Main.echo(Main.encrypt(password)) + "\nDiscount=>" +
    Discount+"\n"


  /**
   * This method performs the Customer Specific Operations
   * Place an Order,
   * view My  Order,
   * Logout
   */


  def performCusOperation(): Unit = {
    var choice: Int = 0
    do {
      println("Hello Customer....," +this.UserName + "...,\nWelcome!!! " +
        "\nplease Select the Operation to perform")
      println("=>Press 1 for Place an Order")
      println("=>Press 2 for View My Order History")
      println("=>Press 3 for LogOut/exit")
      println("Enter option")
      choice = scala.io.StdIn.readInt()
      choice match {
        case 1 =>
          Product.productDetails()
          writeHistory(this.cusId, "=>" + Main.getDate)
          this.placeAnOrder()
        case 2 =>
          println("History...")
          Main.historyMap(this.cusId).showHistory()
        case _=>
          writeHistory(this.cusId, "----------------------------------------------" +
            "-------------------------------------------------------------------")
      }
    } while (choice <= 2)

  }

  /**
   * This Method Place the Order for the Customer  By
   * Prompt the Information
   */

  def placeAnOrder(): Unit = {
    var conf = 0
    println("================================" +
      "=====================================" +
      "=======================")
    println("**********************************BILLING APPLICATION**" +
      "*************************************")
    println("Welcome.., " + this.UserName)
    println("Select your products.....,\nNote: Maximum Product can " +
      "Choose is 100 at a time")
    var pid = 0
    var choices=""
    var quantity = 0
    var sum = 0.0f

    do {
      println("Enter Product ID")
      pid = scala.io.StdIn.readInt()
      if (Product.checkOrder(pid)) {
        println(Product.getProductInfo(pid) + " Required Quantity")
        quantity = scala.io.StdIn.readInt()
        if (Product.checkQuantity(pid, quantity)) {
          sum += (Product.getPrice(pid)*quantity)
          println("Added to Cart")
          println("Current Cart Price =>" + sum)
          choices+= pid + "-" + quantity+","
        }
        else {
          println("Product ID : " + pid + " is Out of stock..." +
            "\nAvailable Stock is " + Product.getQuantity(pid))
          println("Enter the Same Product Id to Get Available Stocks.")
        }
      }
      else {
        println("Please Enter a Valid product ID...Again")
      }
    } while (pid != 0)

    Product.printBill(this,choices.split(",+"), discount = false)
    println("press 1 for confirm")
    println("press 0 to Cancel Order")
    conf = scala.io.StdIn.readInt()
    if (conf == 1) {
      confirmOrder(this, choices.split(",+"))
      if (!this.Discount.equals("-")) {
        println("Do you Want to Apply the coupons?\n press 1 for yes " +
          "\n press 0 for No")
        conf = scala.io.StdIn.readInt()
      }
        if (conf == 1) {
         val total =  Product.printBill(this,choices.split(",+"),
           discount = true)
          Main.writeProductTrans(this.cusId,total)
        }
        else{
          Product.printBill(this, choices.split(",+"),
            discount = true)
        }
    }
    else println("Order Cancelled!!!")
  }

  /**
   * This Method Confirms the Order of Customer for the Selected Items
   * @param customer Object of Particular Customer
   * @param pidArray Array of String Denoting the PID and Quantity
   */

  def confirmOrder(customer: Customer, pidArray: Array[String]): Unit = {
    Product.updateOrder(customer, pidArray)
  }

  /**
   * This Method write History of Customer in their Respective File
   * @param cusId Customer ID
   * @param string String that has to Write
   */

  def writeHistory(cusId: Int, string: String): Unit = {
    Main.historyMap(cusId).writeOnHistory(string)
  }

  /**
   * This Method return Amount Mentioned in the Discount coupons
   * @return Discount
   */

  def discountCoupons(): Int = {
    if (this.Discount.equals("")) {
      0
    }
    else {
      this.Discount.toLowerCase().filter(x => x >= '0' && x <= '9').toInt
    }
  }
}
