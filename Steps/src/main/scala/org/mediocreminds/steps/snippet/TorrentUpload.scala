/**
 *    Copyright 2009 Emil Hellman
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
 * Date: 2009-nov-13
 * Time: 21:59:09
 */

package org.mediocreminds.steps.snippet

import org.mediocreminds.steps._
import model._
import net.liftweb._
import common.{Failure, Full, Empty, Box}
import http._
import SHtml._

import net.liftweb.util.Helpers._
import org.mediocreminds.util._
import scala.xml.{NodeSeq}

import java.security.MessageDigest
import java.security.NoSuchAlgorithmException

import scala.collection.immutable.SortedMap
import util._

class TorrentUpload {
  def uploadForm =
    <lift:TorrentUpload.add form="POST" multipart="true">
      <dl>
        <dt>Torrent Name (max 32 characters)</dt>
        <dd>
          <torrent:name>Torrent Name</torrent:name>
        </dd>
        <dt>File</dt>
        <dd>
          <torrent:file>Torrent File</torrent:file>
        </dd>
        <dt>Description</dt>
        <dd>
          <torrent:description>Torrent Description</torrent:description>
        </dd>
        <dt>
          &nbsp;
        </dt>
        <dd>
          <torrent:submit>
            <button>Upload</button>
          </torrent:submit>
        </dd>
      </dl>
    </lift:TorrentUpload.add>

  var fileHolder: Box[FileParamHolder] = Empty
  import BenConversions._

  /**
   * Handles the uploading of a .torrent file.
   * It checks that the file contains a bencoded dictionary and computes the
   * torrents infoHash value.
   */
  def add(form: NodeSeq) = {
    val torrent = Torrent.create

    def checkAndSave(): Unit = {
      fileHolder match {
        case Full(FileParamHolder(_, _, fileName, data)) => {
          Log.trace("Received and will attempt to decode the file: " + fileName)
          val box = Bencoder.decode(data) match {
            case Some(BenMap(map)) => {
              Log.trace("Decoded file content. Calculating info hash of file: " + fileName)
              calculateInfoHash(map) // All ok!
            }
            case _ => Failure("Torrent file is not valid", Empty, Empty)
          }
          box match {
            case Full(info_hash) => {
              torrent.metaInfoFileContent(data);
              torrent.fileName(fileName)
              torrent.info_hash(info_hash)
              torrent.validate match {
                case Nil => {
                  torrent.save
                  Log.trace("Stored the file: " + fileName)

                }
                case xs => S.error(xs)
              }
            }
            case Failure(e, _, _) => S.error(List(FieldError(torrent.metaInfoFileContent, <b>e</b>)))
            case _ => S.error(List(FieldError(torrent.metaInfoFileContent, <b>An error occured when parsing uploaded file</b>)))
          }
        }
        case _ => {
          S.error("Invalid torrent file upload")

        }
        S.mapSnippet("TorrentSnip.add", doBind)
      }
    }
    def doBind(form: NodeSeq) =
      bind("torrent", form,
        "name" -> torrent.name.toForm,
        "description" -> torrent.description.toForm,
        "file" -> fileUpload(fh => fileHolder = Full(fh)),
        "submit" -> submit("Upload", checkAndSave))
    doBind(form)
  }


  private def calculateInfoHash(map: SortedMap[BenString, Benable]): Box[String] = {
    Log.trace("Reencoding info part of map")

    val bytes = Bencoder.encode(map("info"))
    try {
      val md = MessageDigest.getInstance("SHA-1");

      md.update(bytes, 0, bytes.length);
      val sha1hash = md.digest
      var sha1hashStr = sha1hash.foldLeft("")((str, e) => str + ", " + e)

      val encoder = new org.apache.commons.codec.net.URLCodec
      val encodedBytes = encoder.encode(sha1hash);
      var encodedBytesStr = encodedBytes.foldLeft("")((str, e) => str + ", " + e)

      val info_hash = new String(encodedBytes)
      Log.trace("infoHash computed to:" + info_hash)
      Full(info_hash)
    } catch {
      case e: NoSuchAlgorithmException => Failure("Not possible to perform SHA-1 hashing", Full(e), Empty)
    }

  }
}
