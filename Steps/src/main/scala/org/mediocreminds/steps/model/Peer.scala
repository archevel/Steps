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
 * Time: 22:24:07
 */

package org.mediocreminds.steps.model

import net.liftweb.mapper._

/**
 * Represents a Peer connected to the tracker
 */
class Peer extends LongKeyedMapper[Peer] with IdPK {
  def getSingleton = Peer

  object peerId extends MappedString(this, 60) {
    override def dbIndexed_? = true
  }
  object key extends MappedString(this, 60)
  object ip extends MappedString(this, 60)
  object port extends MappedInt(this)
  object infoHash extends MappedString(this, 90) {
    override def dbIndexed_? = true
  }
  object completed extends MappedBoolean(this)
  object downloaded extends MappedLong(this)
  object uploaded extends MappedLong(this)

  object connectedAt extends MappedDate(this)
}

object Peer extends Peer with LongKeyedMetaMapper[Peer]