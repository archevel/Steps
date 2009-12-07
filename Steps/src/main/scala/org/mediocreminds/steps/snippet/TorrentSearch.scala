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
 * Date: 2009-nov-13
 * Time: 22:51:47
 */

package org.mediocreminds.steps.snippet

import net.liftweb._
import http._
import SHtml._
import util._
import Helpers._

import scala.xml._
import net.liftweb.util.Log

import org.mediocreminds.steps.model.Torrent

class TorrentSearch {
  /**
   * Returns the XHTML for the Search form for torrents along with the result area...
   */
  def searchForm =
    <lift:TorrentSearch.search form="POST">
      <dl>
        <dt>Search:</dt>
        <dd>
          <search:searchWords>Search words</search:searchWords>
        </dd>
        <dt>
          &nbsp;
        </dt>
        <dd>
          <search:submit>
            <button>Search</button>
          </search:submit>
        </dd>
      </dl>
      <table>
        <thead>
          <tr>
            <th>File Name</th>
            <th>Torrent file</th>
          </tr>
        </thead>
        <tbody id="search_result">
          <lift:TorrentSearch.renderResult>
            <tr>
              <td>
                <result:name>A file name...</result:name>
              </td>
              <td>
                <a result:file_href=" ">
                    <img src="/images/document-save.png" alt="Download file"/>
                </a>
              </td>

            </tr>
          </lift:TorrentSearch.renderResult>
        </tbody>
      </table>
    </lift:TorrentSearch.search>

  object resultListVar extends RequestVar[List[Torrent]](List[Torrent]())

  def search(form: NodeSeq): NodeSeq = {
    Log.trace("In search")
    var searchWords = ""
    var res = List[Torrent]()

    def searchForTorrents(): Unit = {
      Log.trace("In searchForTorrents")
      Log.trace("searchWords are: " + searchWords)

      val searchStrings = (searchWords.split(" ").toList.foldLeft(List[String]())(
        (list, elem) => {
          if (list.contains(elem) || "".equals(elem)) {
            list
          } else {
            elem :: list
          }
        }
        )).map(a => "%" + a + "%")
      if (searchStrings.length == 0) {
        resultListVar(List[Torrent]()) // skip search...
      } else {
        doSearch(searchStrings)
      }
    }

    def doSearch(searchStrings: List[String]) = {
      val sb = new _root_.java.lang.StringBuilder(
        "SELECT DISTINCT * FROM Torrent WHERE " +
                "fileName LIKE ? OR " +
                "name LIKE ? OR description LIKE ? ")
      val toAddForEachAdditionalWord = " OR fileName LIKE ? OR name LIKE ? OR description LIKE ? "

      // Add three placeholders for each search word
      searchStrings.tail.foreach(
        x => {
          sb.append(toAddForEachAdditionalWord)
        })

      Log.trace("sb is: " + sb.toString)

      // Find all torrents that have any of the search words in
      // either the filename, name or description
      res = Torrent.findAllByPreparedStatement(supercon => {
        val stmt = supercon.connection.prepareStatement(sb.toString)

        // Prepare the statment by adding the elems
        searchStrings.foldLeft(1)((pos, elem) => {
          stmt.setString(pos, elem)
          stmt.setString(pos + 1, elem)
          stmt.setString(pos + 2, elem)
          pos + 3
        })
        stmt
      })
      Log.trace("Search result length is: " + res.length);

      resultListVar(res)
    }

    bind("search", form,
      "searchWords" -> SHtml.text(searchWords, searchWords = _),
      "submit" -> submit("Search", searchForTorrents))
  }

  def renderResult(content: NodeSeq): NodeSeq = {
    val result = resultListVar.is
    if (result.isEmpty) {
        <tr/>
    } else {
      result.flatMap(a => bind("result", content,
        "name" -> Text(a.name),
        FuncAttrBindParam("file_href", _ =>
          Text("TorrentDownload/" + (a.id)), "href")))
    }
  }
}