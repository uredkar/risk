import com.typesafe.config.{Config, ConfigFactory}

object ConfigReader {
  // Load the configuration file
  private val config: Config = ConfigFactory.load()

  // Access specific properties
  val appName: String = config.getString("app.name")
  val appVersion: String = config.getString("app.version")

  val enableLogging: Boolean = config.getBoolean("app.features.enableLogging")
  val maxConnections: Int = config.getInt("app.features.maxConnections")

  val dbUrl: String = config.getString("app.database.url")
  val dbUser: String = config.getString("app.database.user")
  val dbPassword: String = config.getString("app.database.password")

  // Example of accessing a nested configuration object
  val databaseConfig: Config = config.getConfig("app.database")

  val dataFileLocation: String = config.getString("app.files.dataFileLocation")
  val outputDirectory: String = config.getString("app.files.outputDirectory")
  val rulesEngineLogDirectory: String = config.getString("app.files.rulesEngineLogDirectory")

  def main(args: Array[String]): Unit = {
    // Print the loaded configuration values
    println(s"Application Name: $appName")
    println(s"Application Version: $appVersion")
    println(s"Enable Logging: $enableLogging")
    println(s"Max Connections: $maxConnections")

    println(s"Database URL: $dbUrl")
    println(s"Database User: $dbUser")
    println(s"Database Password: $dbPassword")
    println(s"dataFileLocation $dataFileLocation")
    println(s"outputDirectory $outputDirectory")
    
  }
}
