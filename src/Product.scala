class Product(id: Int, pro: String, cat: String, prices: Int, quan: Int,
              Discount: String) {
  /**
   * Product constructor
   */
  var pid: Int = id
  var productName: String = pro
  var category: String = cat
  var price: Int = prices
  var quantity: Int = quan
  var discount: String = Discount
  var hike = 0

  /**
   * this Method prints the Product  Information
   *
   * @return Returns All Information of Object as a String
   */
  override def toString: String = "ProductID =>  " + pid +
    "  ProductName=> " + productName + " Category=> " +
    category + " Quantity =>" + quantity + " Hike =>" + hike + "\n"
}

object Product {
  /** This Method Get Quantity of product
   *
   * @param pid product ID
   * @return Quantity of Product
   */
  def getQuantity(pid: Int): Int = {
    Main.productMap(pid).quantity
  }

  /** This Method Get  price of product
   *
   * @param pid product ID
   * @return Price of Product
   */
  def getPrice(pid: Int): Int = {
    Main.productMap(pid).price
  }

  /** This Method Get ProductInformation
   *
   * @param pid product ID
   * @return Information of  Product
   */

  def getProductInfo(pid: Int): String = {
    Main.productMap(pid).productName + " (category: " +
      Main.productMap(pid).category + ")"
  }

  /**
   * prints the Bill for Selected Items By the Customer
   *
   * @param customer Customer Object
   * @param pidArray Array of Selected Product and Quantity
   * @param discount Willing for Discount
   */
  def printBill(customer: Customer, pidArray: Array[String],
                discount: Boolean): Double = {
    var total = 0
    var discounts = 0.0
    println("=================================" +
      "=============")
    println("************BILLING APPLICATION***************")
    println("Dear," + customer.UserName + "(ID:" + customer.cusId + ")")
    printf("|%30s%22s%18s%10s%12s|\n", "Product Details",
      "Quantity", "Price per Unit", "Cost", "Discount")
    for (i <- pidArray) {
      val temp = i.split("-")
      val pid = temp(0).toInt
      val quantity = temp(1).toInt
      var dis = 0.0
      if (discount && isAvailDiscount(pid)) {
        dis = applyDiscount(pid, quantity, customer)
      }
      printf("|%30s%12d%15d%15d%16s|\n", Product.getProductInfo(pid),
        quantity, Product.getPrice(pid), Product.getPrice(pid) * quantity,
        discountStatus(discount, pid, dis))
      discounts += dis
      total += (Product.getPrice(pid) * quantity)
    }
    printf(String.format("\n%76s%5d", "Total Amount",
      Int.box(total)))
    printf(String.format("\n%78s%.2f", "Applied Discount",
      Double.box(discounts)))
    printf("\n%87s", "-------------------------------")
    val grandTotal = total - discounts
    printf(String.format("\n%79s%.3f", "GRAND TOTAL:",
      Double.box(grandTotal)))
    printf("\n%87s", "-------------------------------\n")
    grandTotal
  }

  /**
   * This Method Returns the String Denoting the Discount Status of
   * Particular Product
   *
   * @param discount willing for Discount
   * @param pid      Product ID
   * @param dis      Discount Amount
   * @return Status of Discount Applied or not
   */
  def discountStatus(discount: Boolean, pid: Int, dis: Double): String = {
    if (discount && isAvailDiscount(pid)) "Applied ==>" + dis
    else "NotApplied"
  }

  /**
   * Updates theConfirmed Order to dataBase i,e productMap
   *
   * @param customer Customer Object
   * @param pidArray Array of Selected Product and Quantity
   */

  def updateOrder(customer: Customer, pidArray: Array[String]):
  Unit = {
    for (i <- pidArray) {
      val temp = i.split("-").toList
      val pid = temp.head.toInt
      val quantity = temp.last.toInt
      hikeOfProduct(pid, quantity)

      val writeTrans = String.format("%30s%4d%20d\n",
        getProductInfo(pid), Int.box(getPrice(pid)), Int.box(quantity))
      Main.productMap(pid).quantity -= quantity
      customer.writeHistory(customer.cusId, writeTrans)
    }
  }

  /**
   * This method Calculate the Discount Amount for the Particular Order
   *
   * @param pid      Product ID
   * @param quantity Quantity of Product
   * @param customer Customer  Object to get Coupon Information
   * @return Amount of Discount
   */
  def applyDiscount(pid: Int, quantity: Int, customer: Customer):
  Double = {
    (Product.getPrice(pid) * quantity) * (customer.
      discountCoupons() / 100.0)
  }

  /**
   * This Method Maintains the Hike of the product
   *
   * @param i        product ID
   * @param quantity Quantity of product
   */
  def hikeOfProduct(i: Int, quantity: Int): Unit = {
    Main.productMap(i).hike += quantity
  }

  /**
   * This Method Sort the productMap by ID and Display it
   */
  def sortByID(): Unit = {
    val db = Main.productMap
    Map(db.toSeq.sortBy(_._1): _*)
    for ((i, _) <- db) {
      print(db(i))
    }
  }

  /**
   * This Method Sort the productMap by Hike and Display it
   */
  def sortByHike(): Unit = {
    val db = Main.productMap
    db.toSeq.sortWith(_._2.hike > _._2.hike).toList.take(3).
      toMap.foreach(y => {
      println(y._2)
    })
  }

  /**
   * This Method Check  the Available of Discount for Product
   *
   * @param pid Product ID
   * @return true if Product is Available for Discount
   */

  def isAvailDiscount(pid: Int): Boolean = {
    !Main.productMap(pid).discount.equals("-")
  }

  /**
   * This Method Update the Quantity of Product
   *
   * @param pidArray Array of Selected Product ID and
   *                 Quantity to change
   * @return true if it is Completed Successfully
   */
  def updateStocksQuantity(pidArray: Array[String]): Boolean = {
    for (i <- pidArray) {
      val temp = i.split("-")
      val pid = temp(0).toInt
      if (!Main.productMap.contains(pid)) {
        return false
      }
      else {
        val quantity = temp(1).toInt
        Main.productMap(pid).quantity += quantity
      }
    }
    true
  }

  /**
   * This Method Update the Price  of Product
   *
   * @param pidArray Array of Selected Product ID and
   *                 Price  to change
   * @return true if it is Completed Successfully
   */
  def updateStocksPrice(pidArray: Array[String]): Boolean = {

    for (i <- pidArray) {
      val temp = i.split("-")
      val pid = temp(0).toInt
      if (!Main.productMap.contains(pid)) {
        return false
      }
      else {
        val price = temp(1).toInt
        Main.productMap(pid).price = price
      }
    }
    true
  }

  /**
   * Adds the New Product to ProductMap
   *
   * @param pid      Product ID
   * @param pro      product Name
   * @param cat      Product category
   * @param prices   Product Price
   * @param quan     Product Quantity
   * @param Discount Product Discount
   * @return true if it is Completed Successfully
   */
  def addNewProduct(pid: Int, pro: String, cat: String, prices: Int,
                    quan: Int, Discount: String): Boolean = {
    if (!Main.productMap.contains(pid)) {
      Main.productMap.put(pid, new Product(pid, pro, cat, prices,
        quan, Discount))
      true
    }
    else false
  }

  /**
   * Displays the Available Items in ProductMap
   */
  def productDetails(): Unit = {

    println("================================" +
      "==============")
    println("************BILLING APPLICATION***************")
    println("Available....")
    printf("%-10s%-40s%-25s%-25s\n", "PID", "PRODUCT DETAILS",
      "PRICE", "QUANTITY AVAILABLE")
    for (i <- Main.productMap.keys.toList) {

      printf("%-10s%-40s%-25s%-25s\n", i, getProductInfo(i),
        getPrice(i), getQuantity(i))

    }
  }

  /**
   * This Method check the Product is present
   *
   * @param pid productID
   * @return true if the product is present else false
   */
  def checkOrder(pid: Int): Boolean = {
    Main.productMap.contains(pid)
  }

  /**
   * This Method checks the Available Quantity of Product
   *
   * @param pid      Product ID
   * @param quantity Product Quantity
   * @return true if the product is present else false
   */
  def checkQuantity(pid: Int, quantity: Int): Boolean = {
    if (quantity < 0 || quantity > Product.getQuantity(pid)) {
      false
    }
    else true
  }

}
