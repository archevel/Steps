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
 * Time: 21:59:53
 */

package org.mediocreminds.steps.model

import net.liftweb.mapper.MappedString
import net.liftweb.mapper.MappedBinary
import net.liftweb.mapper.MappedPoliteString
import net.liftweb.mapper.LongKeyedMapper
import net.liftweb.mapper.LongKeyedMetaMapper
import net.liftweb.mapper.IdPK

/**
 * Represents a Torrent file.
 */
class Torrent extends LongKeyedMapper[Torrent] with IdPK {
  def getSingleton = Torrent

  // the name of the entry
  object name extends MappedString(this, 32) {
  }

  object fileName extends MappedString(this, 100)
  object info_hash extends MappedString(this, 90) {
    override def dbIndexed_? = true
  }
  object description extends MappedPoliteString(this, 2000)

  object metaInfoFileContent extends MappedBinary(this)

}

object Torrent extends Torrent with LongKeyedMetaMapper[Torrent]