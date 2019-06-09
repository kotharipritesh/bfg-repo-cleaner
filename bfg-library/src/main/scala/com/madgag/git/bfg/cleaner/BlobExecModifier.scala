package com.madgag.git.bfg.cleaner

import java.io._
import java.nio.file.{Files, StandardCopyOption}

import com.google.common.io
import com.madgag.git.ThreadLocalObjectDatabaseResources
import com.madgag.git.bfg.model.{BlobFileMode, TreeBlobEntry}
import org.eclipse.jgit.lib.Constants.OBJ_BLOB
import org.eclipse.jgit.lib.ObjectId
import org.eclipse.jgit.util.FileUtils

import scala.sys.process._
import scala.util.matching.Regex

trait BlobExecModifier extends TreeBlobModifier {

  def command: String

  def fileMask: Regex

  def keepInput: Boolean

  def cacheOnly: Boolean

  def minSizeReduction: Long

  val threadLocalObjectDBResources: ThreadLocalObjectDatabaseResources

  def execute(entry: TreeBlobEntry): (BlobFileMode, ObjectId) = {
    val blobId = entry.objectId.name()
    val fileName = entry.filename.string
    val cacheFolder = new File("blobExecCache", io.Files.getNameWithoutExtension(command) + "/" + blobId.substring(0, 2))

    val cacheFile = new File(cacheFolder, blobId + "_" + fileName)
    if (cacheFile.exists()) {
      val contents = Files.readAllBytes(cacheFile.toPath)
      val objectId = threadLocalObjectDBResources.inserter().insert(OBJ_BLOB, contents)
      return entry.copy(objectId = objectId).withoutName
    }

    if (cacheOnly) {
      return entry.withoutName
    }

    val loader = threadLocalObjectDBResources.reader().open(entry.objectId)
    val inputStream = loader.openStream

    FileUtils.mkdirs(cacheFolder, true)
    val inputFile = new File(cacheFolder, blobId + "_in_" + fileName)
    Files.copy(inputStream, inputFile.toPath, StandardCopyOption.REPLACE_EXISTING)
    inputStream.close()

    val logFile = new File(cacheFolder, blobId + "_" + fileName + ".log")
    val exitCode = Process(
      command, None,
      "BFG_BLOB" -> entry.objectId.name,
      "BFG_FILENAME" -> fileName,
      "BFG_INPUT" -> inputFile.getAbsolutePath,
      "BFG_OUTPUT" -> cacheFile.getAbsolutePath
    ) ! new FileProcessLogger(logFile)

    val inputLength = inputFile.length()
    val outputLength = cacheFile.length()

    if (!keepInput) {
      inputFile.delete()
    }

    if (logFile.length() == 0) {
      logFile.delete()
    }

    if (exitCode != 0) {
      cacheFile.delete()
      println(s"Warning: error executing command [$command] on blob ${entry.objectId.name} with filename {$fileName}: error code {$exitCode}")
      // in case of error ignore
      cacheFile.delete()
      return entry.withoutName
    }

    if (inputLength - outputLength < minSizeReduction) {
      cacheFile.delete()
      return entry.withoutName
    }

    // replace blob
    val newContents = Files.newInputStream(cacheFile.toPath)
    val objectId = threadLocalObjectDBResources.inserter().insert(OBJ_BLOB, outputLength, newContents)
    newContents.close()
    if (objectId.equals(entry.objectId)) {
      println(s"  [$command] ${entry.objectId.name}: identical, {$fileName}")
      cacheFile.delete()
      // file output is identical, ignore
      entry.withoutName
    } else {
      println(s"  [$command] ${entry.objectId.name}: size ${inputLength-outputLength} = $inputLength - $outputLength, {$fileName}")
      entry.copy(objectId = objectId).withoutName
    }
  }

  def fix(entry: TreeBlobEntry): (BlobFileMode, ObjectId) = {
    val fileName = entry.filename.toString

    fileMask.findFirstIn(fileName) match {
      case Some(_) => execute(entry)
      case _ => entry.withoutName
    }
  }
}
