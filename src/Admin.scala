class Admin(id: Int, name: String, pass: String, roles: String, Dis: String) {
  /**
   * Admin Class Constructor
   */
  val cusId: Int = id
  val UserName: String = name
  val password: String = pass
  val role: String = roles
  val Discount: String = Dis

  /**
   * this Method prints theAdmin Information
   *
   * @return Returns All Information of Object as a String
   */
  override def toString: String = "AdminID |" + cusId + "|" +
    "Admin UserName:|" + UserName + "|" + "Admin Password:|" +
    Main.encrypt(password) + "DiscountStatus" + Discount

  /**
   * Performs Admin Operation
   * Update Operation
   * View History
   * Top selling
   * LogOut/exit
   */

  def performAdminOperation(): Unit = {
    var choice = 0
    do {
      println("Hello Admin  " + this.UserName + "...,\nWelcome!!! " +
        "\nplease Select the Operation to perform")
      println("=>Press 1 for Update Operation")
      println("=>Press 2 for View History")
      println("=>Press 3 for Top selling")
      println("=>Press 4 for LogOut/exit")
      println("Enter option")
      choice = scala.io.StdIn.readInt()
      choice match {
        case 1 =>
          writeHistory(this.cusId, "=>" + Main.getDate)
          Product.productDetails()
          this.updateProducts()
        case 2 =>
          println("History...")
          println("Enter CusID to see the History ")
          val ch = scala.io.StdIn.readInt()
          writeHistory(this.cusId, "visited to See History of " + ch)

          if (!Main.userMap.contains(ch)) {
            println("ID not found")
          }
          else
            Main.historyMap(ch).showHistory()
          writeHistory(this.cusId, "------------------------------------" +
            "-------------------------")
        case 3 =>
          println("Top 3 selling Items")
          Product.sortByHike()
        case _ =>
          writeHistory(this.cusId, "------------------------------------------" +
            "------- ******----------------------------------------------------------------")
      }
    } while (choice <= 3)

  }

  /**
   * Update Operation for the Admin
   */
  def updateProducts(): Unit = {
    var pid = 0
    var choice = 0
    //var choice1=0
    var quantity = 0
    var price = 0
    var cat = ""
    var pro = ""
    var dis = ""
    var strings = ""
    do {
      println("===============>Admin " +
        "Operations<==================")
      println("Hi Admin ,.. "+this.UserName+"\n Please Use " +
        "Underscore ,avoid space while enter Details")
      println("1) Add New Product")
      println("2) Update Product Price")
      println("3) Update Product Quantity")
      println("4) Remove Products")
      println("5) Exit ")
      println("Enter your Choice:")
      choice = scala.io.StdIn.readInt()
      choice match {
        case 1 =>
          writeHistory(this.cusId, "=>Updated new Item")

          println("===============>Add new Product<====" +
            "=============")
          println("Enter the product ID")
          pid = scala.io.StdIn.readInt()
          println("Enter the product Name")
          pro = scala.io.StdIn.readLine()
          println("Enter the product Category")
          cat = scala.io.StdIn.readLine()
          println("Enter the Price per Unit")
          price = scala.io.StdIn.readInt()
          println("Enter the Quantity")
          quantity = scala.io.StdIn.readInt()
          println("Enter Discount Status * for Yes - for No")
          dis = scala.io.StdIn.readLine()

          if (Product.addNewProduct(pid, pro, cat, price, quantity, dis)) {
            println("Added... ")
            writeHistory(this.cusId, "Details: " +
              Main.productMap(pid))
            writeHistory(this.cusId, "------------------------------------------")
          } else println("Failed to Add")
        case 3 =>
          var ch = 0
          writeHistory(this.cusId, "=>Updated Quantity of Item")

          Product.productDetails()
          strings = ""
          println("==============>Update Product Quantity<=" +
            "=" +
            "===============")
          do {
            println("Enter the product ID")
            pid = scala.io.StdIn.readInt()
            println("Enter the Quantity")
            quantity = scala.io.StdIn.readInt()
            println("Added to Change List")
            strings += (pid + "-" + quantity + ",")
            writeHistory(this.cusId, "ProductID= " + pid +
              " Quantity changed to " + quantity)
            writeHistory(this.cusId, "------------------------------------------")
            println("Press 0 for Exit")
            println("Press 1 for Continue....")
            ch = scala.io.StdIn.readInt()
          } while (ch != 0)
          if (Product.updateStocksQuantity(strings.split(",+"))) {
            println("Products Quantity are Updated")
          }
          else {
            println("Failed to update")
          }
        case 2 =>
          var ch = 0
          writeHistory(this.cusId, "=>Updated Price of Item")
          Product.productDetails()
          strings = ""
          println("==============>Update Product price<===" +
            "==============")
          do {
            println("Enter the product ID")
            pid = scala.io.StdIn.readInt()
            println("Enter the Price")
            price = scala.io.StdIn.readInt()
            println("Added to Change List")
            strings += (pid + "-" + price + ",")
            writeHistory(this.cusId, "ProductID= " + pid + " Price " +
              "changed to " + price)
            writeHistory(this.cusId, "------------------------------------------")
            println("Press 0 for Exit")
            ch = scala.io.StdIn.readInt()
          } while (ch != 0)
          if (Product.updateStocksPrice(strings.split(",+"))) {
            println("Products price are Updated")
          }
          else {
            println("Failed to Update")
          }
        case 4 =>
          Product.productDetails()
          println("==============Remove Product=====" +
            "============")
          println("Enter the product ID to Remove")
          pid = scala.io.StdIn.readInt()
          if (removeProducts(pid)) {
            writeHistory(this.cusId, "=>Remove " +
              "Operation performed")
            writeHistory(this.cusId, "=>Product ID " + pid +
              " Removed")
            writeHistory(this.cusId, "------------------------------------------")
            println("Product ID " + pid + " Removed")
          }
          else {
            println("Unable remove / enter valid ID")
          }
        case _ =>
          writeHistory(this.cusId, "-------------------------------------------" +
            "----*********------------------------------------------------")
      }
    } while (choice <= 4)
  }

  /**
   * Remove the Particular Product
   *
   * @param pid Product ID
   * @return true if Remove is Success
   */
  def removeProducts(pid: Int): Boolean = {
    if (Main.productMap.contains(pid)) {
      Main.productMap.-=(pid)
      true
    }
    else {
      false
    }
  }

  /**
   * Write the History for admin Specific Operations
   *
   * @param cusId  Customer ID
   * @param string Operation Performed
   */
  def writeHistory(cusId: Int, string: String): Unit = {
    Main.historyMap(cusId).writeOnHistory(string)
  }
}
