package crudkhalnaya.utils

case class dbConfig(driver: String,
                    hostname: String,
                    user: String,
                    password: String)

case class Config(db: dbConfig)
