
package sifive.blocks.devices.pinctrl

import Chisel.{defaultCompileOptions => _, _}
import freechips.rocketchip.util.CompileOptions.NotStrictInferReset

// This is the base class of things you "always"
// want to control from a HW block.
class PinCtrl extends Bundle {
  val oval = Bool()
  val oe   = Bool()
  val ie   = Bool()
}

// Package up the inputs and outputs
// for the Pin
abstract class Pin extends Bundle {
  val i = new Bundle {
    val ival = Bool(INPUT)
    val po   = Option(Bool(INPUT))
  }
  val o: PinCtrl

  // Must be defined by the subclasses
  def default(): Unit
  def inputPin(mode: Bool = Bool(false), pullup_en: Bool = Bool(false), pulldown_en: Bool = Bool(false)): Bool
  def outputPin(signal: Bool,
    mode: Bool = Bool(false), 
    pullup_en: Bool = Bool(false),
    pulldown_en: Bool = Bool(false),
    ds0: Bool = Bool(false),
    ds1: Bool = Bool(false),
    ds2: Bool = Bool(false),
    ie: Bool = Bool(false)
  ): Unit
  
}


////////////////////////////////////////////////////////////////////////////////////

class BasePin extends Pin() {
  val o = new PinCtrl().asOutput

  def default(): Unit = {
    this.o.oval := Bool(false)
    this.o.oe   := Bool(false)
    this.o.ie   := Bool(false)
  }

  def inputPin(mode: Bool = Bool(false), pullup_en: Bool = Bool(false), pulldown_en: Bool = Bool(false)): Bool = {
    this.o.oval := Bool(false)
    this.o.oe   := Bool(false)
    this.o.ie   := Bool(true)
    this.i.ival
  }

  def outputPin(signal: Bool,
    mode: Bool = Bool(false), 
    pullup_en: Bool = Bool(false),
    pulldown_en: Bool = Bool(false),
    ds0: Bool = Bool(false),
    ds1: Bool = Bool(false),
    ds2: Bool = Bool(false),
    ie: Bool = Bool(false)
  ): Unit = {
    this.o.oval := signal
    this.o.oe   := Bool(true)
    this.o.ie   := ie
  }
}

/////////////////////////////////////////////////////////////////////////
class EnhancedPinCtrl extends PinCtrl {
  val mode        = Bool()
  val pullup_en   = Bool()
  val pulldown_en = Bool()
  val prog_slew   = Bool()
  val ds0         = Bool()
  val ds1         = Bool()
  val ds2         = Bool()
  val poe         = Bool()
}

class EnhancedPin extends Pin() {
  val o = new EnhancedPinCtrl().asOutput

  def default(): Unit = {
    this.o.mode         := Bool(false)
    this.o.pullup_en    := Bool(false)
    this.o.pulldown_en  := Bool(false)
    this.o.prog_slew    := Bool(false)
    this.o.ds0          := Bool(false)
    this.o.ds1          := Bool(false)
    this.o.ds2          := Bool(false)
    this.o.poe          := Bool(false)
  }

  def inputPin(mode: Bool = Bool(false),
    pullup_en: Bool = Bool(false),
    pulldown_en: Bool = Bool(false)
  ): Bool = {
    this.o.ie           := Bool(true)
    this.o.oe           := Bool(false)
    this.o.oval         := Bool(false)
    this.o.mode         := mode
    this.o.pullup_en    := pullup_en
    this.o.pulldown_en  := pulldown_en
    this.o.prog_slew    := Bool(false)
    this.o.ds0          := Bool(false)
    this.o.ds1          := Bool(false)
    this.o.ds2          := Bool(false)
    this.o.poe          := Bool(false)

    this.i.ival
  }

  def outputPin(signal: Bool,
    mode: Bool = Bool(false), 
    pullup_en: Bool = Bool(false),
    pulldown_en: Bool = Bool(false),
    ds0: Bool = Bool(false),
    ds1: Bool = Bool(false),
    ds2: Bool = Bool(false),
    ie: Bool = Bool(false)
  ): Unit = {
    this.o.ie           := ie
    this.o.oe           := Bool(false)
    this.o.oval         := signal
    this.o.mode         := mode
    this.o.pullup_en    := pullup_en
    this.o.pulldown_en  := pulldown_en
    this.o.prog_slew    := Bool(false)
    this.o.ds0          := ds0
    this.o.ds1          := ds1
    this.o.ds2          := ds2
    this.o.poe          := Bool(false)
  }

  def toBasePin(): BasePin = {
    val base_pin = Wire(new BasePin())
    base_pin <> this
    base_pin
  }

  def nandInput(poe: Bool = Bool(true)) : Bool = {
    this.i.po.get
  }
}

/*
   Copyright 2016 SiFive, Inc.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/
