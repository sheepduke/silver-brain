package com.sheepduke.silver_brain

package common

case class AppConfig(
    database: DatabaseConfig = DatabaseConfig()
)

case class DatabaseConfig(
    rootDir: String = s"${os.home}/temp/silver-brain",
    defaultDatabaseName: String = "silver-brain"
)
