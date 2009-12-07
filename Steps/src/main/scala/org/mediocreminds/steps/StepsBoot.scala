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
 * Time: 22:00:56
 */

package org.mediocreminds.steps

import model.{Peer, Torrent}
import net.liftweb.http.{GetRequest, Req, LiftRules}
import peer.{PeerWatcher, Announcer}
import snippet.TorrentDownload
import net.liftweb.mapper.Schemifier
import net.liftweb.util.Log

object StepsBoot {
  var trackerInterval = 90;
  var trackerMinInterval = 60;
  var trackerName = "Steps"
  var trackerAnnouncePath = "Announce" :: Nil

  /**
   * Should be called <b>after</b> the database has been setup.
   */
  def boot() = {
    // where to search snippet
    LiftRules.addToPackages("org.mediocreminds.steps")
    val announcer = new Announcer(trackerName, trackerInterval, trackerMinInterval)
    LiftRules.dispatch.append {
      case Req("TorrentDownload" :: strId :: Nil, _, GetRequest) =>
        () => TorrentDownload.torrent(strId)
      case Req("Announce" :: Nil, _, GetRequest) =>
        () => announcer.announce()
    }

    Schemifier.schemify(true, Log.infoF _, Peer, Torrent)

    // Causes the removal of old peers that did not stop gracefully
    PeerWatcher.checkIn(trackerInterval)
  }

}