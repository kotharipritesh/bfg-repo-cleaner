/*
 * Copyright (c) 2012, 2013 Vladimir Sitnikov
 *
 * This file is part of 'BFG Repo-Cleaner' - a tool for removing large
 * or troublesome blobs from Git repositories.
 *
 * BFG Repo-Cleaner is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * BFG Repo-Cleaner is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see http://www.gnu.org/licenses/ .
 */

package com.madgag.git.bfg.cleaner

import java.io.{BufferedReader, ByteArrayOutputStream, InputStreamReader, OutputStreamWriter}
import java.nio.charset.Charset

import com.madgag.git.ThreadLocalObjectDatabaseResources
import com.madgag.git.bfg.model.{BlobFileMode, TreeBlobEntry}
import org.apache.commons.io.IOUtils
import org.eclipse.jgit.lib.Constants.OBJ_BLOB
import org.eclipse.jgit.lib.ObjectId
import org.eclipse.jgit.util.io.{AutoCRLFOutputStream, AutoLFOutputStream}

import scala.util.matching.Regex

trait EolModifier extends TreeBlobModifier {
  val threadLocalObjectDBResources: ThreadLocalObjectDatabaseResources

  val include: Regex

  val exclude: Regex

  val eol: String

  def fix(entry: TreeBlobEntry): (BlobFileMode, ObjectId) = {
    val fileName = entry.filename.string

    if ((exclude != null && exclude.pattern.matcher(fileName).matches())
        || (include != null && !include.pattern.matcher(fileName).matches())) {
      return entry.withoutName
    }

    val loader = threadLocalObjectDBResources.reader().open(entry.objectId)
    val inputStream = loader.openStream

    val baos = new ByteArrayOutputStream()
    val filter = if ("lf".equals(eol)) new AutoLFOutputStream(baos) else new AutoCRLFOutputStream(baos)

    IOUtils.copy(inputStream, filter)
    filter.close()

    val oid = threadLocalObjectDBResources.inserter().insert(OBJ_BLOB, baos.toByteArray)

    entry.copy(objectId = oid).withoutName
  }
}
