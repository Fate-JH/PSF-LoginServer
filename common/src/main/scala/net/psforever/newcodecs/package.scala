// Copyright (c) 2016 PSForever.net to present
package net.psforever.newcodecs

import scodec.Attempt
import scodec.Attempt.{Failure, Successful}
import scodec.Codec

package object newcodecs {

  def q_double(min: Double, max: Double, bits: Int): Codec[Double] = new QuantizedDoubleCodec(min, max, bits)

  def q_float(min : Double, max : Double, bits : Int): Codec[Float] = q_double(min, max, bits).narrow(v => Attempt.successful(v.toFloat), _.toDouble)

}
