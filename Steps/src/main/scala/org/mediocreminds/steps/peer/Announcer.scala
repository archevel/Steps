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
 * Time: 19:10:11
 */

package org.mediocreminds.steps.peer

import net.liftweb.actor.LiftActor
import java.util.Date
import net.liftweb.http._
import net.liftweb.common._
import collection.immutable.{SortedMap, TreeMap}
import org.mediocreminds.steps.model.{Torrent, Peer}
import net.liftweb.mapper._
import org.mediocreminds.util.BenConversions._
import javax.servlet.ServletRequest
import org.mediocreminds.util.{BenMap, Benable, Bencoder, BenString}
import net.liftweb.util.{ActorPing, Log}


class Announcer(val trackerId: String, val interval: Int, val minInterval: Int) {

  /**
   * This is used to get an non-urldecoded version of the url parameters where %00-ff are uppercased.
   */
  private object UrlHandler {
    def getUpperCasedUrlParameters(): Map[String, String] = {
      var queryString = S.request match {
        case Full(r: Req) => r.request.queryString.openOr("") //getParameter("infoHash") //req.getParam("infoHash"))
      }
      queryString.split("&").foldLeft(Map[String, String]())(
        (m, e) => e.split("=", 2).toList match {
          case k :: v :: Nil => {
            Log.trace("Adding to map: (" + k + "," + v + ")")
            m + ((k, toUppercaseUrlEncoding(v)))
          }
          case _ => m // skipit!
        })
    }

    private def toUppercaseUrlEncoding(urlEncoding: String) = {
      "(%..)".r.findAllIn(urlEncoding).foldLeft(urlEncoding)((newStr, aMatch) => newStr.replaceAll(aMatch, aMatch.toUpperCase))
    }
  }


  def announce(): Box[LiftResponse] = {
    Log.trace("In announce")
    val peerHandler = new PeerHandler();
    //val req: HttpServletRequest = S.request.map(r => (r.asInstanceOf[net.HTTPRequestServlet]).req)


    val map = UrlHandler.getUpperCasedUrlParameters()

    val info_hash = map("infoHash")
    Log.trace("infoHash url parameter is: '" + info_hash + "'")

    val peer_id = map("peerId")
    Log.trace("peerId url parameter is: '" + peer_id + "'")

    val key = map("key")
    Log.trace("key url parameter is: '" + key + "'")

    val ip = S.param("ip") match {
      case Full(x) if x != "" => x
      case _ => S.request.open_!.remoteAddr
    }

    val event = S.param("event") match {
      case Full(x) => x
      case _ => ""
    }

    val numwant = S.param("numwant") match {
      case Full(x) => x.toInt
      case _ => 30
    }



    def findOrCreateCallingPeer(peer_id: String, infoHash: String, key: String): Box[Peer] = {
      // Check if it is a new peer
      if (event.equals("started") && !ip.equals("")) {
        try {
          val port = map("port").toInt
          peerHandler.create(peer_id, info_hash, key, ip, port, false, 0, 0)
        } catch {
          case nfe: NumberFormatException =>
            Log.trace("Bad value for port parameter")
            Failure("Port parameter did not have a valid value", Empty, Empty)
          case x =>
            Log.trace("Failed to create a new peer: \n" + x)
            Failure("Could not create peer", Empty, Empty)
        }
      } else {
        try {
          peerHandler.findUniquePeerBy(peer_id, infoHash, key) match {
            case Full(peer) => peerHandler.peerConnected(peer)
            case x => x // let calling method handle errors
          }

        } catch {
          case _ => Failure("No key parameter sent un url", Empty, Empty)
        }
      }
    }

    def handleEvent(peer: Peer): Box[SortedMap[BenString, Benable]] = {
      // Using S.param incase event param is not present...
      val event = S.param("event") match {
        case Full(x) => x
        case _ => ""
      }

      if ("stopped".equals(event)) {
        // If event is stopped removePeer the peer with the peerId and return an empty result with a OK (200)
        Log.trace("Removing peer")
        peerHandler.removePeer(peer)
      } else if ("completed".equals(event)) {
        // If event is completed update the Peer and set it to completed
        Log.trace("Setting peer to seeder")
        peerHandler.peerComplete(peer)
      }
      if (!"stopped".equals(event)) {
        // Retrieve torrent statistics (ie number of seeders and leechers)
        val stats = peerHandler.retrieveStatistics(peer.infoHash.toString)

        // Check if retrieving peers is necessary
        if (stats._1 + stats._2 > 1) {
          // Retrieve a list of peers
          val peers = peerHandler.findPeersTo(peer, numwant)
          Full(createResponseMap(stats, peers, peer))
        } else {
          // If there are only 1 seeder or leecher (i.e. only the calling peer is connected)
          Full(createResponseMap(stats, List[Peer](), peer))
        }
      } else {
        // Return an empty map, nothing needs to be sent to client that has stopped
        Full(TreeMap[BenString, Benable]())
      }
    }


    // Retrieve the peer with peerId
    (findOrCreateCallingPeer(peer_id, info_hash, key) match {
      case Full(peer: Peer) => {
        val validatedPeer = peerHandler.ensurePeerMatch(peer, ip)
        // Send message to PeerWatcher...
        // PeerWatcher ! PeerConnected(peer.peerId, peer.key, peer.infoHash)
        handleEvent(validatedPeer)
      }
      case x => x // let other match take care of result...
    }) match {
      case Full(toEncode: Benable) => {
        Log.trace("Encoding SortedMap with successful response")
        //  encode a tracker response with interval, tracker id, completed (seeders), incomplete (leechers), peers
        val array = Bencoder.encode(toEncode)
        Full(InMemoryResponse(array, ("Content-Type" -> "plain/text") :: Nil, Nil, 200))

      }
      case Failure(msg, _, _) => {
        Log.trace("Encoding SortedMap with failure_reason: " + msg)
        // encode msg as failure reason
        val array = Bencoder.encode(TreeMap(BenString("failure_reason", "failure_reason".getBytes("UTF-8")) -> BenString(msg, msg.getBytes("UTF-8"))))
        Full(InMemoryResponse(array, ("Content-Type" -> "plain/text") :: Nil, Nil, 200))
      }
      case Empty => {
        Log.trace("Returning internal server error")
        Full(InternalServerErrorResponse())
      }
    }
  }

  private def createResponseMap(stats: (Int, Int), peers: List[Peer], callingPeer: Peer): SortedMap[BenString, Benable] = {
    def createEncodablePeerList(): Benable = {
      peers.map(p => TreeMap[BenString, Benable](
        (BenString("peer id") -> BenString(p.peerId.toString)),
        (BenString("ip") -> BenString(p.ip.toString)),
        (BenString("port") -> BenString(p.port.toString))))
    }

    // Create response map
    TreeMap[BenString, Benable](
      (BenString("complete") -> stats._1),
      (BenString("incomplete") -> stats._2),
      (BenString("interval") -> interval),
      (BenString("min interval") -> minInterval),
      (BenString("peers") -> createEncodablePeerList()),
      (BenString("tracker id") -> BenString(trackerId)))
  }
}


/**
 * Handels tasks Peer related.
 */
class PeerHandler extends PeerCreator with PeerModifier with PeerFinder with PeerRemover


/**
 * Finds peers stored in the database.
 */
trait PeerFinder {
  /**
   * Finds the peer that has peerId and infoHash.
   * @param peerId the peerId of the sought peer
   * @param infoHash the infoHash of the sought peer
   * @return a Full box with a Peer in it if the Peer was successfully found.
   */
  def findUniquePeerBy(peerId: String, infoHash: String, key: String): Box[Peer] = {
    Peer.findAll(By(Peer.peerId, peerId), By(Peer.infoHash, infoHash), By(Peer.key, key)) match {
      case List(p: Peer) => {
        Full(p)
      }
      case _ => Failure("No unique peer found with given peerId, infoHash and key", Empty, Empty)
    }
  }

  /**
   * Retrieves a list of suitable peers based on the Peer.
   * @param Peer the state that the peer selection should be based on
   * @return a List of Peers if the Peers were successfully found.
   */
  def findPeersTo(peer: Peer, numwant: Int): List[Peer] = {
    def retrieveSomePeers(): List[Peer] = {
      if (peer.completed) {
        // If peer is completed retrieve 'numwant' number of peers
        // which are not completed and does not match peerId
        Peer.findAll(By(Peer.infoHash, peer.infoHash), NotBy(Peer.peerId, peer.peerId),
          OrderBy(Peer.completed, Descending),
          OrderBy(Peer.downloaded, Descending),
          OrderBy(Peer.uploaded, Descending),
          MaxRows(numwant))
      } else {
        // Else retrieve 'numwant' number of peers which have a higher download than the peer ordered by lowest upload(/download?)
        Peer.findAll(By(Peer.infoHash, peer.infoHash), NotBy(Peer.peerId, peer.peerId),
          By_>(Peer.downloaded, peer.downloaded),
          OrderBy(Peer.completed, Ascending),
          OrderBy(Peer.downloaded, Descending),
          OrderBy(Peer.uploaded, Ascending),
          MaxRows(numwant))
      }
    }
    val somePeers = retrieveSomePeers()
    // Check if we should add peers that have downloaded less than the calling peer
    if (somePeers.length < numwant) {
      val peers = (Peer.findAll(By(Peer.infoHash, peer.infoHash), NotBy(Peer.peerId, peer.peerId),
        OrderBy(Peer.completed, Ascending),
        OrderBy(Peer.downloaded, Descending),
        OrderBy(Peer.uploaded, Ascending),
        MaxRows(numwant - somePeers.length))
              )

      (peers ++ somePeers).foldLeft(List[Peer]())((list, p) => {
        if (list.contains(p)) {
          list
        } else {
          p :: list
        }
      })
    } else {
      somePeers
    }
  }

  /**
   * Retrieves the number of seeders and leechers connected to a particular torrent.
   * @param infoHash the infoHash of the torrent to retrieve statistics for
   * @return a tuple with seeder and leechers (seeders, leechers)
   */
  def retrieveStatistics(infoHash: String): (Int, Int) = {
    // Count seeders and leechers
    val seeders = Peer.count(By(Peer.infoHash, infoHash), By(Peer.completed, true)).toInt
    val leechers = Peer.count(By(Peer.infoHash, infoHash), By(Peer.completed, false)).toInt
    (seeders, leechers)
  }
}


/**
 * Handles the updating of the data stored about a peer in the database.
 */
trait PeerModifier {
  /**
   * Updates the values of the peer with those in the newState.
   * @param peerId the peerId of the peer that should be updated
   * @param infoHash the infoHash of the peer that should be updated
   * @param newState the state that the peer should be in after update.
   * @return a Full box with a Peer in it if the Peer was successfully updated, otherwise a Failure
   */
  def ensurePeerMatch(peer: Peer, ip: String): Peer = {
    // Ensure correct IP is sent
    if (peer.ip.equals(ip)) {
      Log.trace("Found peer")
      peer
    } else {
      // If ip in the Peer differs from the ip sent verify that
      // the sent key matches the stored Peer's and if so update
      // the peer's ip.
      Log.trace("Found peer, but with new IP")
      val savePeer = peer.ip(ip)
      savePeer.save
      savePeer
    }
  }

  def peerComplete(peer: Peer) = {
    val savePeer = peer.completed(true)
    savePeer.save
    savePeer
  }

  def peerConnected(peer: Peer): Box[Peer] = {
    val savePeer = peer.connectedAt(new _root_.java.util.Date())
    savePeer.save
    Full(savePeer)
  }
}

/**
 * Responsible for creating new peers in the database.
 */
trait PeerCreator {
  /**
   * Adds a new peer to the database.
   * @param peerId
   * @param infoHash
   * @param key
   * @param ip
   * @param port
   * @param completed
   * @param downloaded
   * @param uploaded
   * @return a Full box with a Peer in it if the Peer was successfully created, otherwise a Failure
   */
  def create(peerId: String, infoHash: String, key: String, ip: String,
             port: Int, completed: Boolean, downloaded: Long, uploaded: Long): Box[Peer] = {
    val p: Peer = Peer.create.peerId(peerId).
            infoHash(infoHash).key(key).
            ip(ip).port(port).completed(false).
            downloaded(0).uploaded(0).connectedAt(new _root_.java.util.Date())
    p.save
    Log.trace("Peer created")
    Full(p)

  }
}

/**
 * Handles the removal of a peer that is no longer connected.
 */
trait PeerRemover {
  /**
   * Removes the peer from the database
   * @param peerDbKey the peer to delete
   * @return true if successfull otherwise false.
   */
  def removePeer(peer: Peer): Boolean = {
    peer.delete_!
  }
}


/**
 * An actor that monitors the peers and removes them when they have not
 * announced their presence in time.
 */
object PeerWatcher extends LiftActor with PeerRemover {
  import net.liftweb.util.Helpers._
  private case class CheckForRemoval
  private var interval: Int = 90

  def checkIn(interval: Int) = {
    this.interval = interval
    ActorPing schedule (this, CheckForRemoval, interval seconds) //new TimeSpan(interval * 1000))
  }

  override protected def messageHandler = {
    case CheckForRemoval => {
      Log.debug("PeerWatcher removing old peers")
      try {
        DB.use(DefaultConnectionIdentifier) {
          con =>
            DB.prepareStatement("DELETE FROM Peer WHERE connectedAt < ?", con)(
              stmt => {
                Log.trace("PeerWatcher: Preparing statment...")
                stmt.setDate(1, new _root_.java.sql.Date(new _root_.java.util.Date().getTime - ((3 * interval) seconds).millis))
                val r = stmt.executeUpdate()
                Log.trace("PeerWatcher: Statment executed: " + r)
              })
            Log.trace("PeerWatcher has removed old peers")
        }
      } catch {
        case x: Exception => Log.error("PeerWatcher: Exception occured: " + x.getMessage)
      }
      checkIn(interval) // the interval value might get overwritten if some other thread accesses it
    }
    case _ => () // ignore
  }
}