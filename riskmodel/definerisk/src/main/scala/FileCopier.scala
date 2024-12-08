import java.nio.file._
import java.nio.file.attribute.{BasicFileAttributes, FileTime}
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Success, Failure}
import scala.jdk.CollectionConverters._

import java.nio.file._
import java.nio.file.attribute.BasicFileAttributes
import scala.jdk.CollectionConverters._

object FileChecker:

  // Collect files recursively from the given directory
  def collectFiles(directory: Path): Set[Path] = {
    Files.walk(directory).iterator().asScala
      .filter(path => Files.isRegularFile(path))
      .toSet
  }

  // Function to show files in the source that are not present in the target
  def showMissingFiles(sourceDir: Path, targetDir: Path): Unit = {
    val sourceFiles = collectFiles(sourceDir)
    val targetFiles = collectFiles(targetDir)

    val missingFiles = sourceFiles.filterNot(file => targetFiles.contains(file))

    if missingFiles.isEmpty then
      println("No files are missing from the target directory.")
    else
      println("Files in the source that are not present in the target:")
      missingFiles.foreach(file => println(file))
  }

  def main(args: Array[String]): Unit = 
    if (args.length != 2) then
      println("Usage: FileChecker <source_dir> <target_dir>")
    else
      val sourceDir = Paths.get(args(0))
      val targetDir = Paths.get(args(1))

      if !Files.isDirectory(sourceDir) || !Files.isDirectory(targetDir) then
        println("Both source and target paths must be directories.")
      else
        showMissingFiles(sourceDir, targetDir)



object FileCopier:

  // Data class for tracking file information
  case class FileInfo(path: Path, size: Long, lastModified: FileTime)

  // Collect files lazily from a directory and its subdirectories, filtered by extension
  def collectFiles(sourceDir: Path, extension: String): LazyList[FileInfo] =
    Files.walk(sourceDir).iterator().asScala
      .filter(path => Files.isRegularFile(path) && path.toString.endsWith(extension))
      .map { path =>
        val attrs = Files.readAttributes(path, classOf[BasicFileAttributes])
        FileInfo(path, attrs.size(), attrs.lastModifiedTime())
      }
      .to(LazyList)

  // Resolve duplicates based on size and lastModified
  def resolveDuplicates(files: Seq[FileInfo]): FileInfo =
    files.maxBy(file => (file.size, file.lastModified.toMillis))

  // Process files in plan or run mode
  def processFiles(
      files: LazyList[FileInfo],
      sourceDir: Path,
      targetDir: Path,
      mode: String
  ): Future[Unit] =
    Future.sequence {
      files.groupBy(_.path.getFileName).map { case (fileName, group) =>
        Future {
          val resolvedFile = resolveDuplicates(group)
          val relativePath = sourceDir.relativize(resolvedFile.path)
          val targetPath = targetDir.resolve(relativePath)

          mode match
            case "plan" =>
              // Display planned copy action
              println(s"Plan: ${resolvedFile.path} -> $targetPath")
            case "run" =>
              // Perform the copy
              Files.createDirectories(targetPath.getParent) // Ensure directory exists
              if !Files.exists(targetPath) then
                Files.copy(resolvedFile.path, targetPath, StandardCopyOption.REPLACE_EXISTING)
                println(s"Copied: ${resolvedFile.path} -> $targetPath")
              else
                println(s"Skipped (duplicate): ${resolvedFile.path}")
            case _ =>
              throw new IllegalArgumentException(s"Unknown mode: $mode")
        }
      }.toSeq
    }.map(_ => ())

  def main(args: Array[String]): Unit =
    if args.length < 4 then
      println("Usage: FileCopier <source_dir> <target_dir> <extension> <mode(plan|run)>")
    else
      val sourceDir = Paths.get(args(0))
      val targetDir = Paths.get(args(1))
      val extension = args(2)
      val mode = args(3)

      if !Files.isDirectory(sourceDir) || !Files.isDirectory(targetDir) then
        println("Both source and target paths must be directories.")
        return

      if mode != "plan" && mode != "run" then
        println("Mode must be either 'plan' or 'run'.")
        return

      try
        val files = collectFiles(sourceDir, s".$extension")
        println(s"Found ${files.size} files with extension .$extension in $sourceDir")

        processFiles(files, sourceDir, targetDir, mode).onComplete {
          case Success(_) =>
            if mode == "run" then println("File copying completed successfully.")
            else println("Plan completed successfully.")
          case Failure(exception) =>
            println(s"Error during file processing: $exception")
        }

        // Wait for completion to prevent the program from exiting
        Thread.sleep(5000)
      catch
        case ex: Exception =>
          println(s"Error: ${ex.getMessage}")
