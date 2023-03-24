package sifive.blocks.devices.gpio

import Chisel.{defaultCompileOptions => _, _}
import chisel3.{VecInit}
import freechips.rocketchip.util.CompileOptions.NotStrictInferReset

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.prci._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.util._




import sifive.blocks.devices.pinctrl.{PinCtrl, Pin, BasePin, EnhancedPin, EnhancedPinCtrl}
import sifive.blocks.util.{DeviceParams,DeviceAttachParams}

// This is sort of weird because
// the IOF end up at the RocketChipTop
// level, and we have to do the pinmux
// outside of RocketChipTop.
// It would be better if the IOF were here and
// we could do the pinmux inside.

class GPIOPortIO(val c: GPIOParams) extends Bundle {
  val pins = Vec(c.width, new EnhancedPin())
}

class IOFPortIO(val w: Int) extends Bundle {
  val iof_0 = Vec(w, new IOFPin).flip
  val iof_1 = Vec(w, new IOFPin).flip
}

case class GPIOParams(
  address: BigInt,
  width: Int,
  includeIOF: Boolean = false,
  dsWidth: Int = 3,
  hasPS: Boolean = false,
  hasPOE: Boolean = false) extends DeviceParams

/** The base GPIO peripheral functionality, which uses the regmap API to
  * abstract over the bus protocol to which it is being connected
  */
abstract class GPIO(busWidthBytes: Int, c: GPIOParams)(implicit p: Parameters)
    extends IORegisterRouter(
      RegisterRouterParams(
        name = "GPIO",
        compat = Seq("sifive, GPIO0", "sifive, GPIO1"),
        base = c.address,
        beatBytes = busWidthBytes),
      new GPIOPortIO(c))
    with HasInterruptSources {
  val iofNode = c.includeIOF.option(BundleBridgeSource(() => new IOFPortIO(c.width)))
  val iofPort = iofNode.map { node => InModuleBody { node.bundle } }

  def nInterrupts = c.width
  override def extraResources(resources: ResourceBindings) = Map(
    "gpio-controller"      -> Nil,
    "#gpio-cells"          -> Seq(ResourceInt(2)),
    "interrupt-controller" -> Nil,
    "#interrupt-cells"     -> Seq(ResourceInt(2)))

  lazy val module = new LazyModuleImp(this) {

  //--------------------------------------------------
  // CSR Declarations
  // -------------------------------------------------

  // SW Control only.
  val input_en_reg      = Module(new AsyncResetRegVec(c.width, 0))
  
  val output_en_reg     = Module(new AsyncResetRegVec(c.width, 0))
  val output_val_reg    = Reg(init = UInt(0, c.width))

  val mode_reg          = Module(new AsyncResetRegVec(c.width, 0))
  val pullup_en_reg     = Module(new AsyncResetRegVec(c.width, 0))
  val pulldown_en_reg   = Module(new AsyncResetRegVec(c.width, 0))
  val output_xor_reg    = Reg(init = UInt(0, c.width))

  val ds_reg            = RegInit(VecInit(Seq.fill(c.dsWidth)(UInt(0, c.width))))
  val prog_slew_reg     = Reg(init = UInt(0, c.width))
  val poeReg            = Module(new AsyncResetRegVec(c.width, 0))

  // Synchronize Input to get valueReg
  val input_val         = Wire(UInt(0, width=c.width))
  input_val := Vec(port.pins.map(_.i.ival)).asUInt
  val inSyncReg         = SynchronizerShiftReg(input_val, 3, Some("inSyncReg"))
  val valueReg          = Reg(init = UInt(0, c.width), next = inSyncReg)

  // Interrupt Configuration
  val highIeReg         = Reg(init = UInt(0, c.width))
  val lowIeReg          = Reg(init = UInt(0, c.width))
  val riseIeReg         = Reg(init = UInt(0, c.width))
  val fallIeReg         = Reg(init = UInt(0, c.width))
  val highIpReg         = Reg(init = UInt(0, c.width))
  val lowIpReg          = Reg(init = UInt(0, c.width))
  val riseIpReg         = Reg(init = UInt(0, c.width))
  val fallIpReg         = Reg(init = UInt(0, c.width))
  val passthruHighIeReg = Reg(init = UInt(0, c.width))
  val passthruLowIeReg  = Reg(init = UInt(0, c.width))

  // HW IO Function
  val iofEnReg          = Module(new AsyncResetRegVec(c.width, 0))
  val iofSelReg         = Reg(init = UInt(0, c.width))

  //--------------------------------------------------
  // CSR Access Logic (most of this section is boilerplate)
  // -------------------------------------------------

  val rise = ~valueReg & inSyncReg;
  val fall = valueReg & ~inSyncReg;

  val iofEnFields =  if (c.includeIOF) (Seq(RegField.rwReg(c.width, iofEnReg.io,
                        Some(RegFieldDesc("IOF_EN","HW I/O functon enable", reset=Some(0))))))
                     else (Seq(RegField(c.width)))
  val iofSelFields = if (c.includeIOF) (Seq(RegField(c.width, iofSelReg,
                        RegFieldDesc("IOF_SEL","HW I/O function select", reset=Some(0)))))
                     else (Seq(RegField(c.width)))
  
  val poeFields = if (c.hasPOE) Seq(RegField.rwReg(c.width, poeReg.io,
                                  Some(RegFieldDesc("poe"," Nandtree enable", reset=Some(0)))))
                  else (Seq(RegField(c.width)))
  val dsRegsAndDescs = Seq.tabulate(c.dsWidth)( i =>
                        Seq(RegField(c.width, ds_reg(i),
                              RegFieldDesc(s"DS$i", s"Pin drive strength $i selection", reset=Some(0)))))
  val dsRegMap = for ((rd, i) <- dsRegsAndDescs.zipWithIndex)
                   yield (GPIOCtrlRegs.DRIVE + 4*i -> Seq(RegField(c.width, ds_reg(i),
                          RegFieldDesc(s"DS$i", s"Pin drive strength $i selection", reset=Some(0)))))

  // shift other register offset when c.dsWidth > 1
  val dsOffset = (c.dsWidth - 1) * 4

  // Note that these are out of order.
  val mapping = Seq(
    GPIOCtrlRegs.INPUT_EN       -> Seq(RegField.rwReg(c.width, input_en_reg.io,
                                  Some(RegFieldDesc("INPUT_EN", "Input enable control", reset=Some(0))))),
    GPIOCtrlRegs.INPUT_VAL      -> Seq(RegField.r(c.width, valueReg,
                                  RegFieldDesc("INPUT_VAL", "Input value", volatile=true))),
    GPIOCtrlRegs.OUTPUT_EN      -> Seq(RegField.rwReg(c.width, output_en_reg.io,
                                  Some(RegFieldDesc("OUTPUT_EN", "Output enable control", reset=Some(0))))),
    GPIOCtrlRegs.OUTPUT_VAL     -> Seq(RegField(c.width, output_val_reg,
                                  RegFieldDesc("OUTPUT_VAL", "Output value", reset=Some(0)))),
    GPIOCtrlRegs.MODE           -> Seq(RegField.rwReg(c.width, mode_reg.io,
                                  Some(RegFieldDesc("MODE", "Output mode select", reset=Some(0))))),
    GPIOCtrlRegs.PULLUP_EN      -> Seq(RegField.rwReg(c.width, pullup_en_reg.io,
                                  Some(RegFieldDesc("PULLUP_EN", "Internal pull-up resistor enable", reset=Some(0))))),
    GPIOCtrlRegs.PULLDOWN_EN    -> Seq(RegField.rwReg(c.width, pulldown_en_reg.io,
                                  Some(RegFieldDesc("PULLDOWN_EN", "Internal pull-down resistor enable", reset=Some(0))))),
    GPIOCtrlRegs.OUTPUT_XOR     -> Seq(RegField(c.width, output_xor_reg,
                                  RegFieldDesc("OUTPUT_XOR", "Output XOR (invert) enable", reset=Some(0)))),
    GPIOCtrlRegs.PROG_SLEW      -> Seq(RegField(c.width, prog_slew_reg,
                                  RegFieldDesc("PROG_SLEW", "Programmable slew rate", reset=Some(0)))),
    GPIOCtrlRegs.rise_ie + dsOffset -> Seq(RegField(c.width, riseIeReg,
                                  RegFieldDesc("rise_ie","Rise interrupt enable", reset=Some(0)))),
    GPIOCtrlRegs.rise_ip + dsOffset -> Seq(RegField.w1ToClear(c.width, riseIpReg, rise,
                                  Some(RegFieldDesc("rise_ip","Rise interrupt pending", volatile=true)))),
    GPIOCtrlRegs.fall_ie + dsOffset -> Seq(RegField(c.width, fallIeReg,
                                  RegFieldDesc("fall_ie", "Fall interrupt enable", reset=Some(0)))),
    GPIOCtrlRegs.fall_ip + dsOffset -> Seq(RegField.w1ToClear(c.width, fallIpReg, fall,
                                  Some(RegFieldDesc("fall_ip","Fall interrupt pending", volatile=true)))),
    GPIOCtrlRegs.high_ie + dsOffset -> Seq(RegField(c.width, highIeReg,
                                  RegFieldDesc("high_ie","High interrupt enable", reset=Some(0)))),
    GPIOCtrlRegs.high_ip + dsOffset -> Seq(RegField.w1ToClear(c.width, highIpReg, valueReg,
                                  Some(RegFieldDesc("high_ip","High interrupt pending", volatile=true)))),
    GPIOCtrlRegs.low_ie  + dsOffset -> Seq(RegField(c.width, lowIeReg,
                                  RegFieldDesc("low_ie","Low interrupt enable", reset=Some(0)))),
    GPIOCtrlRegs.low_ip  + dsOffset -> Seq(RegField.w1ToClear(c.width,lowIpReg, ~valueReg,
                                  Some(RegFieldDesc("low_ip","Low interrupt pending", volatile=true)))),
    GPIOCtrlRegs.iof_en  + dsOffset -> iofEnFields,
    GPIOCtrlRegs.iof_sel + dsOffset -> iofSelFields,
    GPIOCtrlRegs.passthru_high_ie + dsOffset -> Seq(RegField(c.width, passthruHighIeReg,
                                         RegFieldDesc("passthru_high_ie", "Pass-through active-high interrupt enable", reset=Some(0)))),
    GPIOCtrlRegs.passthru_low_ie  + dsOffset -> Seq(RegField(c.width, passthruLowIeReg,
                                         RegFieldDesc("passthru_low_ie", "Pass-through active-low interrupt enable", reset=Some(0)))),
  )
  regmap(mapping ++ dsRegMap :_*)

  //--------------------------------------------------
  // Actual Pinmux
  // -------------------------------------------------

  val swPinCtrl = Wire(Vec(c.width, new EnhancedPinCtrl()))

  // This strips off the valid.
  val iof0Ctrl = Wire(Vec(c.width, new IOFCtrl()))
  val iof1Ctrl = Wire(Vec(c.width, new IOFCtrl()))

  val iofCtrl = Wire(Vec(c.width, new IOFCtrl()))
  val iofPlusSwPinCtrl = Wire(Vec(c.width, new EnhancedPinCtrl()))

  for (pin <- 0 until c.width) {

    // Software Pin Control
    swPinCtrl(pin).ie           := input_en_reg.io.q(pin)
    swPinCtrl(pin).oe           := output_en_reg.io.q(pin)
    swPinCtrl(pin).oval         := output_val_reg(pin)
    swPinCtrl(pin).mode         := mode_reg.io.q(pin)
    swPinCtrl(pin).pullup_en    := pullup_en_reg.io.q(pin)
    swPinCtrl(pin).pulldown_en  := pulldown_en_reg.io.q(pin)
    swPinCtrl(pin).prog_slew    := prog_slew_reg(pin)
    swPinCtrl(pin).ds0          := ds_reg(0)(pin)
    swPinCtrl(pin).ds1          := ds_reg(1)(pin)
    swPinCtrl(pin).ds2          := ds_reg(2)(pin)
    swPinCtrl(pin).poe          := poeReg.io.q(pin)

    val pre_xor = Wire(new EnhancedPinCtrl())

    if (c.includeIOF) {
      // Allow SW Override for invalid inputs.
      iof0Ctrl(pin)      <> swPinCtrl(pin)
      when (iofPort.get.iof_0(pin).o.valid) {
        iof0Ctrl(pin)    <> iofPort.get.iof_0(pin).o
      }

      iof1Ctrl(pin)      <> swPinCtrl(pin)
      when (iofPort.get.iof_1(pin).o.valid) {
        iof1Ctrl(pin)    <> iofPort.get.iof_1(pin).o
      }

      // Select IOF 0 vs. IOF 1.
      iofCtrl(pin)       <> Mux(iofSelReg(pin), iof1Ctrl(pin), iof0Ctrl(pin))

      // Allow SW Override for things IOF doesn't control.
      iofPlusSwPinCtrl(pin) <> swPinCtrl(pin)
      iofPlusSwPinCtrl(pin) <> iofCtrl(pin)
   
      // Final XOR & Pin Control
      pre_xor  := Mux(iofEnReg.io.q(pin), iofPlusSwPinCtrl(pin), swPinCtrl(pin))
    } else {
      pre_xor := swPinCtrl(pin)
    }

    port.pins(pin).o      := pre_xor
    port.pins(pin).o.oval := pre_xor.oval ^ output_xor_reg(pin)

    // Generate Interrupts
    interrupts(pin) := (riseIpReg(pin) & riseIeReg(pin)) |
                         (fallIpReg(pin) & fallIeReg(pin)) |
                         (highIpReg(pin) & highIeReg(pin)) |
                         (lowIpReg(pin) & lowIeReg(pin)) |
                         (valueReg(pin) & passthruHighIeReg(pin)) |
                         (~valueReg(pin) & passthruLowIeReg(pin))

    if (c.includeIOF) {
      // Send Value to all consumers
      iofPort.get.iof_0(pin).i.ival := inSyncReg(pin)
      iofPort.get.iof_1(pin).i.ival := inSyncReg(pin)
    }
  }}
}

class TLGPIO(busWidthBytes: Int, params: GPIOParams)(implicit p: Parameters)
  extends GPIO(busWidthBytes, params) with HasTLControlRegMap

case class GPIOLocated(loc: HierarchicalLocation) extends Field[Seq[GPIOAttachParams]](Nil)

case class GPIOAttachParams(
  device: GPIOParams,
  controlWhere: TLBusWrapperLocation = PBUS,
  blockerAddr: Option[BigInt] = None,
  controlXType: ClockCrossingType = NoCrossing,
  intXType: ClockCrossingType = NoCrossing) extends DeviceAttachParams
{
  def attachTo(where: Attachable)(implicit p: Parameters): TLGPIO = where {
    val name = s"gpio_${GPIO.nextId()}"
    val cbus = where.locateTLBusWrapper(controlWhere)
    val gpioClockDomainWrapper = LazyModule(new ClockSinkDomain(take = None))
    val gpio = gpioClockDomainWrapper { LazyModule(new TLGPIO(cbus.beatBytes, device)) }
    gpio.suggestName(name)

    cbus.coupleTo(s"device_named_$name") { bus =>

      val blockerOpt = blockerAddr.map { a =>
        val blocker = LazyModule(new TLClockBlocker(BasicBusBlockerParams(a, cbus.beatBytes, cbus.beatBytes)))
        cbus.coupleTo(s"bus_blocker_for_$name") { blocker.controlNode := TLFragmenter(cbus) := _ }
        blocker
      }

      gpioClockDomainWrapper.clockNode := (controlXType match {
        case _: SynchronousCrossing =>
          cbus.dtsClk.map(_.bind(gpio.device))
          cbus.fixedClockNode
        case _: RationalCrossing =>
          cbus.clockNode
        case _: AsynchronousCrossing =>
          val gpioClockGroup = ClockGroup()
          gpioClockGroup := where.asyncClockGroupsNode
          blockerOpt.map { _.clockNode := gpioClockGroup } .getOrElse { gpioClockGroup }
      })

      (gpio.controlXing(controlXType)
        := TLFragmenter(cbus)
        := blockerOpt.map { _.node := bus } .getOrElse { bus })
    }

    (intXType match {
      case _: SynchronousCrossing => where.ibus.fromSync
      case _: RationalCrossing => where.ibus.fromRational
      case _: AsynchronousCrossing => where.ibus.fromAsync
    }) := gpio.intXing(intXType)

    gpio
  }
}

object GPIO {
  val nextId = { var i = -1; () => { i += 1; i} }

  def makePort(node: BundleBridgeSource[GPIOPortIO], name: String)(implicit p: Parameters): ModuleValue[GPIOPortIO] = {
    val gpioNode = node.makeSink()
    InModuleBody { gpioNode.makeIO()(ValName(name)) }
  }

  def tieoff(g: GPIOPortIO){
    g.pins.foreach { p =>
      p.i.ival := false.B
    }
  }

  def tieoff(f: IOFPortIO) {
    f.iof_0.foreach { iof => iof.default() }
    f.iof_1.foreach { iof => iof.default() }
  }

  def loopback(g: GPIOPortIO)(pinA: Int, pinB: Int) {
    require(g.pins.length > pinA, s"Pin ${pinA} out of range for GPIO port with only ${g.pins.length} pins")
    require(g.pins.length > pinB, s"Pin ${pinB} out of range for GPIO port with only ${g.pins.length} pins")
    g.pins.foreach {p =>
      p.i.ival := Mux(p.o.oe, p.o.oval, p.o.pullup_en) & p.o.ie
    }
    val a = g.pins(pinA)
    val b = g.pins(pinB)
    // This logic is not QUITE right, it doesn't handle all the subtle cases.
    // It is probably simpler to just hook a pad up here and use attach()
    // to model this properly.
    a.i.ival := Mux(b.o.oe, (b.o.oval | b.o.pullup_en), (a.o.pullup_en | (a.o.oe & a.o.oval))) & a.o.ie
    b.i.ival := Mux(a.o.oe, (a.o.oval | b.o.pullup_en), (b.o.pullup_en | (b.o.oe & b.o.oval))) & b.o.ie
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
