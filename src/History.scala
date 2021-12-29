import java.util.Date
import java.io._
class History(History:File){
  /**History constructor
   */
  var fileData:File = History
  var date:String=String.format("%1$s %2$tB %2$td, %2$tY",
    "",new Date())

  /**
   * Write the History on the called  User File Object
   * @param transLine String to write
   */
  def writeOnHistory(transLine:String): Unit ={
    val tempHis = new PrintWriter(new FileOutputStream(this.fileData,true))
    tempHis.println(transLine)
    tempHis.close()

  }

  /**
   * creates the templat for History file
   * @param string specify Role of the User
   */
  def createTemplate(string:String):Unit={
    val tempHis = new PrintWriter(new FileOutputStream(this.fileData,true))
    tempHis.println("========================" +
      "=============" +
      "===================================" +
      "====================")
    tempHis.println("**********************************BILLING " +
      "APPLICATION***************************************")
    tempHis.println("========================" +
      "==================================" +
      "==================================")
    if(string.equalsIgnoreCase("Customer")) {
      tempHis.printf("Name Of the Customer :%10s",
        Main.userMap(Main.getID(this.fileData.getPath.trim)).UserName)
      tempHis.printf(String.format("\nCustomer ID:%d",
        Int.box(Main.userMap(Main.getID(this.fileData.getPath.trim)).cusId)))
      tempHis.printf("\nCreated on: %15s", this.date)
      tempHis.println("Dear Customer,....")

    }
    else
      {
        tempHis.printf("Name Of the Customer :%10s\n",
          Main.adminMap(Main.getID(this.fileData.getPath.trim)).UserName)
        tempHis.printf(String.format("\nAdmin ID:%d",
          Int.box(Main.adminMap(Main.getID(this.fileData.getPath.trim)).cusId)))
        tempHis.printf("Created on: %15s\n", this.date)
        tempHis.println("Hi Admin,....")
      }


    tempHis.println("Your History,.")
    tempHis.println("==============================" +
      "=======================================" +
      "========================")
    if(string.equalsIgnoreCase("Customer"))
    tempHis.printf("%30s%22s%20s\n",
      "Product", "Price", "Quantity")
    else
      tempHis.println("ADMIN OPERATION HISTORY")

    tempHis.println("==============================" +
      "=======================================" +
      "========================")
    tempHis.close()
  }

  /**
   * Displays the History of Particular User
   */
  def showHistory():Unit={
    val tempFile = scala.io.Source.fromFile(this.fileData.getPath)
    for(i<-tempFile.getLines())
      {
        println(i)
      }
      tempFile.close()
  }
}
