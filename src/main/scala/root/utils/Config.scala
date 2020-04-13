package root.utils

import pureconfig._
import pureconfig.generic.auto._

case class dbConfig(driver: String,
                    hostname: String,
                    user: String,
                    password: String)

case class Config(db: dbConfig)
