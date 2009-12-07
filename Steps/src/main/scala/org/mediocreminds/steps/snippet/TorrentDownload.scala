/**
 *   Copyright 2009 Emil Hellman
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *

 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 **/
/**
 * User: archevel
 * Date: 2009-nov-14
 * Time: 00:27:21
 */

package org.mediocreminds.steps.snippet

import org.mediocreminds.steps.model.Torrent
import net.liftweb._
import common.{Failure, Box, Full, Empty}
import http._
import mapper._

import org.mediocreminds.util.{BenString}
import collection.immutable.{TreeMap}

/**
 * Handles the download of a torrent file.
 */
object TorrentDownload {
  def torrent(strId: String): Box[LiftResponse] = {
    try {
      val id = strId.toLong
      Torrent.findAll(By(Torrent.primaryKeyField, id)) match {
        case List(t) => { // Since we get the torrent by the primaryKey there will only be one...
          Full(InMemoryResponse(
            t.metaInfoFileContent.is,
            ("Content-Type" -> "application/x-bittorrent") ::
                    ("Content-Disposition" -> ("attachment; filename=\"" + t.fileName.is + "\"")) ::
                    Nil,
            Nil, 200))
        }
        case Nil => Empty
      }
    } catch {
      case ex: java.lang.NumberFormatException => Failure("Not a valid torrent id.", Full(ex), Empty)
    }
  }
}