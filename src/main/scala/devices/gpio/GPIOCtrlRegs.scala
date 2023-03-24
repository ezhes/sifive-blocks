package sifive.blocks.devices.gpio

object GPIOCtrlRegs {
  val INPUT_EN    = 0x00
  val INPUT_VAL   = 0x04
  val OUTPUT_EN   = 0x08
  val OUTPUT_VAL  = 0x0C
  val MODE        = 0x10
  val PULLUP_EN   = 0x14
  val PULLDOWN_EN = 0x18
  val OUTPUT_XOR  = 0x1C
  val PROG_SLEW   = 0x20
  val DRIVE       = 0x24
  val rise_ie     = 0x28
  val rise_ip     = 0x2C
  val fall_ie     = 0x30
  val fall_ip     = 0x34
  val high_ie     = 0x38
  val high_ip     = 0x3C
  val low_ie      = 0x40
  val low_ip      = 0x44
  val iof_en      = 0x48
  val iof_sel     = 0x4C
  val passthru_high_ie = 0x50
  val passthru_low_ie  = 0x54
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
