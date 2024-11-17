package com.definerisk.core.utils.PrettyPrinter

trait PrettyPrinter[T]: 
    def prettyPrint(value: T): String

// Generic pretty printer for sequences
given [T](using printer: PrettyPrinter[T]): PrettyPrinter[Seq[T]] with
  def prettyPrint(values: Seq[T]): String =
    values.map(printer.prettyPrint).mkString("\n")

// Generic helper to print any object
def printPretty[T](value: T)(using PrettyPrinter[T]): Unit =
  println(summon[PrettyPrinter[T]].prettyPrint(value))

extension [T](value: T)(using printer: PrettyPrinter[T])
    def pretty: String = printer.prettyPrint(value)