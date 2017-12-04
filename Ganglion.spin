{{ General Notes ==========================================
	Instead of Queries running in distinct cogs, see if 
	we can page them into the interpreter or satellite 
	cog (most are short; should be faster than reading
	512 HUB ops in a row…).
}}
{{ Todo ===================================================
	- Consider an extruder command that sets multiple
	  extruders at once; instead of e0 5 step-rate e1 5 step rate,
	  it could be 3 extruders 5 step-rate (3 being a bitmask).

}}
{{ Style conventions ======================================
	Symbols are named [prefixes][variant][noun|verb]
	Symbol prefixes:
	a   - An address pointer.
	c   - A cog variable.
	h   - Hub variable.
	k   - Symbolic constant
	m   - A bit mask.
	p   - A pin number.
	s   - A bit mask for state.
}}
{{ Glossary ===============================================
	Cycle:  A clock cycle, _CLKFREQ occur in 1 sec.
	Slot:   Data structure for motor control queue.
	Step:   Indicates that a motor rotates in its current 
		direction by its current size. Converted to 
		ticks using step-rate.
	Tick:   Time required to execute a stage of the motor
		control pipeline; 500 cycles, 6.25µsec.
}}
CON ' Propeller constants ---------------------------------
  ' 80 Mhz = 16 * 5 Mhz external crystal
  _clkmode = xtal1 + pll16x
  _clkfreq = 80_000_000

CON ' Cog assignments -------------------------------------
	#0
	kControllerCog	' The REPL or Satellite.
	kMotorAxonCog
	kMotorDendriteCog
	kMotorTissueCog
	kSwitchCog
	kPowerCog
	kTemperatureCog
	' No command cog for now to enable PWM heater control.
	'kCommandCog		' Execute local & remote commands.
	kSerialTerminalCog

CON ' Pin assignments --------------------------------------
	#0
	kpActiveSer
	kpActiveSck
	kpActiveRck
	kpActiveClr
	kpLowMotionSer
	kpLowMotionShift
	kpLowMotionClr
	kpHighMotionSer
	#8
	kpHighMotionShift
	kpHighMotionClr
	kpPowerSer
	kpPowerSck
	kpPowerRck
	kpPowerClr
	kpSwitchSer
	kpSwitchCk
	#16
	kpSwitchLoad
	kpReserved0		' Power switch test: Turn LOW to power
	kpReserved1		' Power switch state: high when pressed, low otherwise
	kpMessageClk	' Requires a pull-down resistor.
	kpMessageBaseTx	' Requires a pull-down resistor.
	kpMessageBaseRx	' Requires a pull-down resistor.
	kpActiveSync	' Requires a pull-down resistor.
	kpDisableMotors
	#24
	kpLeftLevelSwitch
	kpRightLevelSwitch
	kpThermistor0
	kpThermistor1
	kpReserved2
	kpCommand		' Sync command execution.
	kpSerialTx
	kpSerialRx

CON ' Motor Assigments ------------------------------------
	#0
	kPlatformMotor
	kHorizontalMotor
	kLeftVerticalMotor
	kRightVerticalMotor

	#0
	kExtruder0Motor
	kExtruder1Motor
	kExtruder2Motor
	kExtruder3Motor
	kExtruder4Motor
	kExtruder5Motor
	kExtruder6Motor
	kExtruder7Motor

	kmHorizontalMotor    = 1 << kHorizontalMotor
	kmLeftVerticalMotor  = 1 << kLeftVerticalMotor 
	kmRightVerticalMotor = 1 << kRightVerticalMotor
	kmVerticalMotors     = kmLeftVerticalMotor | kmRightVerticalMotor 

	kmLevelLeft  = (1 << kpLeftLevelSwitch)
	kmLevelRight = (1 << kpRightLevelSwitch)
	kmLevel      =  kmLevelLeft | kmLevelRight

{{ Controller cog =========================================
	Command (4 bits)
		0: New motor state
		1: Buffer free?
		2: State?
		3: Step rate?
		4: Heat
		5: Reboot
		6: Temperature?
		7: Target temperature?
		8: Delta ticks
		9: Awake
		A: Sleep
		B: Zero rates?	' Deletion candidate
	Address/Motor (4 bits)	
		Note this means we can use the address directly.
	<ack>
	- motor -
		Step rate (16 bits)
		State (4 bits)
		<ack>
	- query -
		<result>
}}
CON ' Interpreter constants -------------------------------
	' Starting a cog takes ~8K cycles, so pad to account
	' for Spin overhead & delay.
	' See <http://bit.ly/1dw0HdT>.
	kSafeCogStartDelay = 1_024 * 16

	kDelimiter = 32			' ASCII space
	kReplInputSize = 24		' Max command length in bytes.

	kmSatellite		 = $03	' Board identification.
	kSatelliteOffset = 2	' Bits to shift off kmSatellite.

CON ' Configuration constants -----------------------------
	kHorizontalSeekSize = kStepperQuarter
	kHorizontalSeekRate = 50

	kVerticalSeekSize = kStepperWhole
	kVerticalSeekRate = 198
	kNumThreads = 10
	kUmPerInch  = 25_400

	' 204.87mm (->210mm), 200 whole steps/revolution, 1/4 step size, steprate
	kMaxHorizontalSeekSteps = 210 * 1_000 / kUmPerInch * {
}		kNumThreads * 200 * 4

	' ~240mm (->250mm), 200 whole steps/revolution, 1/1 step size
	kMaxVerticalSeekSteps  = 275 * 1_000 / kUmPerInch * {
}		kNumThreads * 200 * 1

	kBlockHeightInSteps = 489
	kBlockSpacing = 50
	kLevelStepRate = 500
	kBlockInsertionTicks = kLevelStepRate * (kBlockHeightInSteps + kBlockSpacing)

	kTicksForRemoval = (kBlockHeightInSteps + kBlockSpacing) * kLevelStepRate


CON ' Interpreter commands --------------------------------
	#0
	{! GanglionCommand }
	kCmdError
	kCmdAttachedP
	kCmdBufferFreeP
	kCmdButton
	kCmdCalibrate
	kCmdCenter
	kCmdClockwise
	kCmdCounterClockwise
	kCmdCredits
	kCmdCycle
	kCmdD
	kCmdDP
	kCmdData
	kCmdDirectionP
	kCmdG
	kCmdHandshake
	kCmdHeater
	kCmdHeat
	kCmdHeaterP
	kCmdHeatTest
	kCmdI
	kCmdIP
	kCmdLevel
	kCmdMotor
	kCmdMotorP
	kCmdOff
	kCmdOn
	kCmdP
	kCmdPP
	kCmdPing
	kCmdPressedP
	kCmdReboot
	kCmdRepeat
	kCmdSeek
	kCmdSeekTest
	kCmdSleep
	kCmdStackP
	kCmdStep
	kCmdStepRate
	kCmdStepRateP
	kCmdStepRateStep
	kCmdStepSize
	kCmdStepSizeP
	kCmdStop
	kCmdTemperatureP
	kCmdTemperaturesP
	kCmdTemperatureTargetP
	kCmdText
	kCmdTickP
	kCmdTickTargetP
	kCmdTokenP
	' Only used by host; no direct support.
	kCmdValue
	kCmdValue1
	kCmdValue2
	kCmdValue3
	kCmdValue4
	kCmdVersionP
	kCmdWaitHeated
	kCmdWake
	kCmdWordsP
	' Motor commands
	kCmdPlatform
	kCmdHorizTrack
	kCmdLeftTrack
	kCmdRightTrack
	kCmdExtruder0
	kCmdExtruder1
	kCmdExtruder2
	kCmdExtruder3
	kCmdExtruder4
	kCmdExtruder5
	kCmdExtruder6
	kCmdExtruder7
	'' The following are for optimizing the 
	'' data sent from the host and aren't
	'' meant to be used at the REPL. As such,
	'' they don't appear in the dictionary.
	' step-rate motor commands
	kCmdStepRatePlatform
	kCmdStepRateHorizTrack
	kCmdStepRateLeftTrack
	kCmdStepRateRightTrack
	kCmdStepRateExtruder0
	kCmdStepRateExtruder1
	kCmdStepRateExtruder2
	kCmdStepRateExtruder3
	kCmdStepRateExtruder4
	kCmdStepRateExtruder5
	kCmdStepRateExtruder6
	kCmdStepRateExtruder7
	' motor step-rate-step commands
	kCmdPlatformStepRateStep
	kCmdHorizTrackStepRateStep
	kCmdLeftTrackStepRateStep
	kCmdRightTrackStepRateStep
	kCmdExtruder0StepRateStep
	kCmdExtruder1StepRateStep
	kCmdExtruder2StepRateStep
	kCmdExtruder3StepRateStep
	kCmdExtruder4StepRateStep
	kCmdExtruder5StepRateStep
	kCmdExtruder6StepRateStep
	kCmdExtruder7StepRateStep
	' motor stop commands
	kCmdPlatformStop
	kCmdHorizTrackStop
	kCmdLeftTrackStop
	kCmdRightTrackStop
	kCmdExtruder0Stop
	kCmdExtruder1Stop
	kCmdExtruder2Stop
	kCmdExtruder3Stop
	kCmdExtruder4Stop
	kCmdExtruder5Stop
	kCmdExtruder6Stop
	kCmdExtruder7Stop
	{! end }
	' Constant numbers
	kCmdNumber
	' Single-byte numbers in data made
	#128
	kCmdSmallNumber
	' The REPL-visible commands + 1
	kCmdLength = kCmdStepRatePlatform

	' Input modes.
	#0
	kModeText
	kModeData

	' Edge constants
	#0
	{! GanglionEdge }
	kLeft
	kRight
	kTop
	kBottom
	{! end }

	{! GanglionStep }
	kStepperWhole     = %000
	kStepperHalf      = %001
	kStepperQuarter   = %010
	kStepperEighth    = %011
	kStepperSixteenth = %111
	{! end }

	{ GanglionPowered - in the future T_T
	#0
	kpHeater0
	kpHeater1
	kpReservedFET0
	kpPowerLight
	kpScannerBackLight
	kpScannerMidLight
	kpScannerFrontLight
	kpPrintheadLight
	end }

	{! GanglionPowered }
	#0
	kpReservedFET0
	kpHeater0
	kpHeater1
	kpPowerLight
	kpScannerBackLight
	kpScannerMidLight
	kpScannerFrontLight
	kpPrintheadLight
	{! end }

	kmReservedFET0= 1 << kpReservedFET0
	kmHeater0     = 1 << kpHeater0
	kmHeater1     = 1 << kpHeater1
	kmPowerLight  = 1 << kpPowerLight
	kmScannerBackLight  = 1 << kpScannerBackLight
	kmScannerMidLight   = 1 << kpScannerMidLight
	kmScannerFrontLight = 1 << kpScannerFrontLight
	kmPrintheadLight    = 1 << kpPrintheadLight

	kNumMaxMotors     = 16
	kNumLocalMotors   = 8
	kmLocalMotor      = 7
	kmClockwise       = %1000
	kmCounterClockwise= %0000
	kmStepSize        = %0111

DAT ' Interpreter hub variables ---------------------------
hBoardId	byte 0   ' 0: Base; >0: Satellite.
hReplMode	byte kModeText

hHeater		byte 0
hMotor		byte 0
hmButton	byte 0

hInput		byte 0[kReplInputSize]
hLastToken	byte 0
hThisToken	byte 0
hStack		long 0

' Structure is:
' # heater0Temp heater1Temp bufferLow bufferHigh motorQLow motorQHigh[0-6] isSeeking[7]
hStatusPacket byte "#", 0[7]

' Only local motor states are recorded.
hReplTicks	long 0
hReplState	long 0
hReplRate0	word 0
hReplRate1	word 0
hReplRate2	word 0
hReplRate3	word 0
hReplRate4	word 0
hReplRate5	word 0
hReplRate6	word 0
hReplRate7	word 0

PUB BaseRepl ' Ganglion entry point -----------------------
	EnsureSafety
	'IdentifyBoardType
	InitializeCogs

#ifdef TEST_HEATER 
	hTargetTemp1 := 225

#ifdef TEST_HEATER_EXTRUDING
	CmdWake
	SendStrLn(string("Waiting for target temp..."))
	repeat while hCurrentTemp1 < hTargetTemp1
		SendStr(string("  at "))
		SendDecLn(hCurrentTemp1)
		waitcnt(_CLKFREQ + cnt)

	' Start all motors extruding.
	repeat result from 4 to 7
		hStack := result
		CmdMotor
		hStack := 384
		CmdStepRate
	hStack := POSX
	CmdStep
#endif

	SendStrLn(string("Starting test..."))
	result := 0
	repeat
		SendStr(string("Seconds: "))
		SendDec(result++)
		SendStr(string( " Temp 1: "))
		SendDec(hCurrentTemp1)
		SendStr(string("C. Out = "))
		SendDecLn(hHeaterDuty1)
		waitcnt(_CLKFREQ + cnt)
		next
#endif

	repeat
		repeat
			ReadString(@hInput)
		while hInput == 0
		if hThisToken <> kCmdNumber and hThisToken <> kCmdError
			hLastToken := hThisToken

		hThisToken := ParseInput
		Evaluate(hThisToken)

PRI EnsureSafety ' Turns off heater -----------------------
	outa~
	dira~

	dira[kpPowerSer..kpPowerClr]~~
	outa |= constant(kmPowerClr | kmPowerSer) 
	repeat 8
		outa[kpPowerSck]~~
		outa[kpPowerSck]~
	outa[kpPowerRck]~~
	outa &= constant(!(kmPowerRck | kmPowerSer))

PRI IdentifyBoardType : addr ' Req'd for cog setup --------
	hBoardId := $00
	coginit(kSwitchCog, @InitializeSwitchCog, 0)
	waitcnt(kSafeCogStartDelay	+ cnt)
	cogstop(kSwitchCog)

	if (hButtonState & kmSatellite) == kmSatellite
		' We are a satellite board.
		hBoardId := (hButtonState >> kSatelliteOffset) + 1

		' TODO: Start the satellite code over cog 0.
		'SendStr(string("Satellite: $"))
	else
		' We are a base board.
		waitcnt(kSafeCogStartDelay + cnt)
		' Handshake.
		'SendStr(string("Base: $"))
	'SendHexLn(hBoardId, 2)

PRI InitializeCogs ' Shared startup -----------------------
	' Avoid a propstick/FTDI rebooting bug.
	' See <http://bit.ly/17dEjx7> for more information.
	if ina[kpSerialRx] == 0
		dira[kpSerialTx]~~
		outa[kpSerialTx]~
	else
		coginit(kSerialTerminalCog, {
			} @InitializeSerialTerminalCog, @hSerialRxFront)

	' NOTE:
	' If we were going to have a command cog, we'd want to start
	' it here and have it sleep on a pin.

	' We need to monitor the temperature and FETs even if 
	' the printer is sleeping to update the warning LED.
	longfill(@hTargetTemp0, 0, 2)
	coginit(kPowerCog,       @InitializePowerCog,       0)
	coginit(kTemperatureCog, @InitializeTemperatureCog, 0)

PRI HashInput : hash | index, char
' Hash hInput using djb2 (xor); see <http://bit.ly/1hAWwhF>.
	hash := index := 0

	repeat until (char := byte[@hInput][index++]) == 0
		hash := ((hash << 5) + hash) ^ char

PRI ParseInput : parsedToken | char, index, value, powerOfTen
' Parse hInput into a token, number, or error message.
	index := 0
	value := HashInput

	' Does it match a token?
	repeat until (char := long[@hDictionaryStart][index << 1]) == 0
		if value == char
			' The hash matched; convert to bytes.
			index := index << 3 + 4
			parsedToken := byte[@hDictionaryStart][index]
			if parsedToken == kCmdNumber
				' Constant.
				hStack := byte[@hDictionaryStart][++index]
			return parsedToken
		' Keep looking.
		++index

	' Is it a number?
	value := index := powerOfTen := 0

	' Skip initial minus signs for now.
	if byte[@hInput] == "-"
		++index

	repeat until (char := byte[@hInput][index++]) == 0
		char -= 48
		if (char < 0) or (char > 9)
			' Not a number.
			return kCmdError
		value := value * 10 + char

	' Make negative if needed
	if byte[@hInput] == "-"
		-value

	hStack := value
	parsedToken := kCmdNumber

PRI Evaluate(aToken)
' Our dispatch table.
	case aToken
		'kCmdAttachedP:
		kCmdBufferFreeP: CmdBufferFreeP
		kCmdButton: CmdButton
		kCmdCalibrate: CmdCalibrate
		kCmdCenter: CmdCenter
		kCmdClockwise: CmdClockwise
		kCmdCounterClockwise: CmdCounterClockwise
		kCmdCredits: CmdCredits
		kCmdCycle: CmdCycle
		kCmdD: CmdD
		kCmdDP: CmdDP
		kCmdData: CmdData
		kCmdDirectionP: CmdDirectionP
		kCmdG: CmdG
		'kCmdHandshake:
		kCmdHeat: CmdHeat
		kCmdHeater: CmdHeater
		kCmdHeaterP: CmdHeaterP
		kCmdHeatTest: CmdHeatTest
		kCmdI: CmdI
		kCmdIP: CmdIP
		kCmdLevel: CmdLevel
		kCmdMotor: CmdMotor
		kCmdMotorP: CmdMotorP
		kCmdNumber: return
		kCmdOff: CmdOff
		kCmdOn: CmdOn
		kCmdP: CmdP
		kCmdPP: CmdPP
		kCmdPing: CmdPing
		kCmdPressedP: CmdPressedP
		kCmdReboot: CmdReboot
		kCmdRepeat: CmdRepeat
		kCmdSeek: CmdSeek
		kCmdSeekTest: CmdSeekTest
		kCmdSleep: CmdSleep
		kCmdStackP: CmdStackP
		kCmdStep: CmdStep
		kCmdStepRate: CmdStepRate
		kCmdStepRateP: CmdStepRateP
		kCmdStepRateStep: CmdStepRateStep
		kCmdStepSize: CmdStepSize
		kCmdStepSizeP: CmdStepSizeP
		kCmdStop: CmdStop
		kCmdTemperatureP: CmdTemperatureP
		kCmdTemperaturesP: CmdTemperaturesP
		kCmdTemperatureTargetP: CmdTemperatureTargetP
		kCmdText: CmdText
		kCmdTickP: CmdTickP
		kCmdTickTargetP: CmdTickTargetP
		kCmdTokenP: CmdTokenP
		' Inline value1…4 so that we process reading
		' bytes as quickly as possible. Values are 
		' the most frequent data sent from the host.
		kCmdValue1: 
			hStack := ReadChar
		kCmdValue2:
			hStack := ReadChar
			byte[@hStack][1] := ReadChar
		kCmdValue3:
			hStack := ReadChar
			byte[@hStack][1] := ReadChar
			byte[@hStack][2] := ReadChar
		kCmdValue4:
			hStack := ReadChar
			byte[@hStack][1] := ReadChar
			byte[@hStack][2] := ReadChar
			byte[@hStack][3] := ReadChar
		kCmdVersionP: CmdVersionP
		kCmdWaitHeated: CmdWaitHeated
		kCmdWake: CmdWake
		kCmdWordsP: CmdWordsP
		kCmdPlatform..kCmdExtruder7: CmdSelectMotor(hThisToken)
		' These are all inlined since they're called 
		' with high frequency when printing. These
		' assume no satellite (as does most of the code!).
		kCmdStepRatePlatform..kCmdStepRateExtruder7:
			word[@hReplRate0][hMotor] := hStack
			hMotor := aToken - kCmdStepRatePlatform
		kCmdPlatformStepRateStep..kCmdExtruder7StepRateStep:
			hMotor := aToken - kCmdPlatformStepRateStep
			word[@hReplRate0][hMotor] := hStack
			CmdStep
		kCmdPlatformStop..kCmdExtruder7Stop:
			hMotor := aToken - kCmdPlatformStop
			word[@hReplRate0][hMotor] := 0
		kCmdSmallNumber..255: CmdSmallNumber
		other: CmdError

PRI CmdButton
'' [0, kButtonBotRight] button => /state changed/
'' Sets the current button state.
	hmButton := 1 << hStack

PRI CmdBufferFreeP | free
'' buffer-free? => [0, kNumSlots]
'' How much space do we have in the smallest motor queue? This
'' is used to determine how much data we can send at a time.
	' TODO: Use query & check remotely as well. For now, local.
	free := hQueueFront - hQueueBack
	free := kNumSlots - (free - kNumSlots * (free < 0))
	SendDecResponseLn(free)

PRI CmdCalibrate
'' [ bottom | top ] calibrate => /calibrate to location/
'' Calibrates the platform at the top or bottom of the 
'' vertical space.
	CmdSeek
	CmdCenter
	_WaitForEmptyMotorQueue

	if hThisToken == kCmdCalibrate and hReplMode == kModeText
		SendResponseLn(string("Calibrated."))

PRI CmdCenter | ticks
'' center => /platform centers/
'' Centers the platform carriage.
	hStack := kLeft
	CmdSeek
	ticks := hCurrentTick

	hStack := kRight
	CmdSeek

	_InitHorizTrackSeeking
	CmdCounterClockwise
	hStack := (hCurrentTick - ticks) >> 1 
	CmdStep

	_ZeroMotorRates
	if hThisToken == kCmdCenter and hReplMode == kModeText
		SendResponseLn(string("Centered."))

PRI CmdClockwise 
'' /motor/ clockwise => /motor's direction changed/
'' Makes the current motor's direction clockwise.
	long[@hReplState][hMotor / 8] |= kmClockwise << ((hMotor & kmLocalMotor) << 2)
	' TODO: Send to satellites if needed.

PRI CmdCounterClockwise : offset
'' /motor/ counter-clockwise => /motor's direction changed/
'' Makes the current motor's direction counter clockwise.
	long[@hReplState][hMotor / 8] &= !(kmClockwise << ((hMotor & kmLocalMotor) << 2))
	' TODO: Send to satellites if needed.

PRI CmdCredits
'' credits => /displayed credits/
'' Displays the credits and copyright information.
	SendResponseLn(@hCredits)

PRI CmdCycle: count | minutes
'' /min/ cycle => /Cycles the current heater every min minutes./
'' Cycles the heater to the target temperature and back
	count := 0
	minutes := hStack
	repeat
		SendStr(string("Starting cycle "))
		SendDec(++count)
		SendStrLn(string("; heating."))
		hStack := 230
		CmdHeat
		_Wait(minutes, 0)

		SendStrLn(string("Cooling."))
		hStack := 0
		CmdHeat
		_Wait(minutes, 0)

		SendStr(string("Completed cycle "))
		SendDec(count)
		SendStrLn(string("."))

	' Ensure we turn things off if we broke out.
	hStack := 0
	CmdHeat

PRI CmdD
'' [-16, 16] d => /sets derivative coefficient/
	hD := hStack <# kUpperCoefficientBounds
	hD := hD #> kLowerCoefficientBounds

PRI CmdDP
'' d? => /derivative coefficient/
	_SendCoefficient(hD)

PRI CmdData : idx | timeLeft, timeStart
'' data => /now receives pre-compiled commands/
'' Enters binary data mode.
	hReplMode := kModeData

	repeat while hReplMode == kModeData
		timeLeft  := _CLKFREQ
		repeat while timeLeft > 0
			timeStart := cnt
			hThisToken := UnblockedReadChar

			if hThisToken > 0
				Evaluate(hThisToken)
			else
				' A byte's not ready to read…or we 
				' received a CmdError from the host.
				' Wait for 8 bits (~69.4µsec) to transmit
				waitcnt(constant(kSerialBitTicks * 8) + cnt)
			timeLeft -= cnt - timeStart

		hStatusPacket[2] := hCurrentTemp0.byte
		hStatusPacket[3] := hCurrentTemp1.byte
		hStatusPacket.word[2] := kSerialRxBufferSize - RxCount

		idx := hQueueFront - hQueueBack
		idx := kNumSlots - (idx - kNumSlots * (idx < 0))
		hStatusPacket.word[3] := idx

		hStatusPacket[1] := (hStatusPacket[2] + hStatusPacket[3] {
			} + hStatusPacket[4] + hStatusPacket[5] {
			} + hStatusPacket[6] +hStatusPacket[7]) & $FF

		repeat idx from 0 to 7
			SendChar(hStatusPacket[idx])

PRI CmdDirectionP
'' /motor/ direction? => [clockwise | counter-clockwise]
'' Displays the current motor's direction.
	case (long[@hReplState][hMotor / 8] >> ((hMotor & kmLocalMotor) << 2)) & kmClockwise 
		kmClockwise: SendResponseLn(@hDictClockwise)
		kmCounterClockwise: SendResponseLn(@hDictCounterClockwise)

PRI CmdError
'' error => Unknown token: /information/
'' We couldn't parse the text token; let the user know.
	SendResponse(string("Unknown token: "))
	if hReplMode == kModeData
		SendDec(hThisToken)
		SendStr(string(" Stack: "))
		SendDecLn(hStack)
	else
		SendStr(@hInput)
	SendStr(@hNewline)

PRI CmdG
'' g => /repeats last command/
'' Executes the previous command again.
	hThisToken := hLastToken
	Evaluate(hThisToken)

PRI CmdHeat
'' /temperature/ heat => /sets target temperature/
'' Sets the target temperature for the current heater.
	long[@hTargetTemp0][hHeater] := hStack <# kMaxTemperature

PRI CmdHeater
'' [0, 1] heater => /selects heater/
'' Selects the target heater.
	hHeater := hStack <# 1

PRI CmdHeaterP
'' heater? => /current heater/
'' Displays the currently selected heater.
	SendDecResponseLn(hHeater)

PRI CmdHeatTest : target | count
'' /number/ heat-test => /Cycles the current heater/
'' Cycles the heater to the target temperature and back.
	target := hStack
	count := 0
	repeat
		hStack := target
		SendStr(string("Starting cycle "))
		SendDec(++count)
		SendStrLn(string("."))
		CmdHeat
		CmdWaitHeated
		SendStrLn(string("At temp. Waiting."))
		_Wait(20, 0)

		SendStrLn(string("Cooling."))
		hStack := 0
		CmdHeat
		_Wait(20, 0)

		SendStr(string("Completed cycle "))
		SendDec(count)
		SendStrLn(string("."))
	hStack := 0
	CmdHeat

PRI CmdI
'' [-16, 16] d => /sets derivative coefficient/
	hI := hStack <# kUpperCoefficientBounds
	hI := hI #> kLowerCoefficientBounds

PRI CmdIP
'' d? => /derivative coefficient/
	_SendCoefficient(hI)

PRI CmdLevel : pinStatus | oldDira
'' level => /levels the platform/
'' UNTESTED WITH NEW FIRMWARE
'' Levels the platform. Requires hardware attached to J37 jumper.
	oldDira := dira

	hStack := kTop
	CmdSeek

	CmdSelectMotor(kCmdLeftTrack)
	CmdClockwise
	hReplRate2 := kLevelStepRate
	CmdSelectMotor(kCmdRightTrack)
	CmdClockwise
	hReplRate3 := kLevelStepRate

	hStack := kBlockInsertionTicks 
	CmdStep
	_WaitForEmptyMotorQueue
	SendStrLn(string("Ready for cubes. Press a key when ready."))
	ReadChar

	dira[kpLeftLevelSwitch..kpRightLevelSwitch]~
	pinStatus~~
	hStack := kStepperSixteenth
	CmdSelectMotor(kCmdLeftTrack)
	CmdStepSize
	CmdSelectMotor(kCmdRightTrack)
	CmdStepSize

	' Main calibration loop.
	repeat while pinStatus <> 0
		_LevellingStep(1, 1)
		pinStatus := ina & kmLevel
		if pinStatus == kmLevelLeft
			SendStrLn(string("Right pin triggered."))
			_LevellingStep(-160, -170)
		elseif pinStatus == kmLevelRight
			SendStrLn(string("Left pin triggered."))
			_LevellingStep(-170, -160)

	' Levelling done.
	_LevellingStep(-kTicksForRemoval, -kTicksForRemoval)
	SendStrLn(string("Remove cubes and press a key..."))
	ReadChar

	_LevellingStep(kTicksForRemoval, kTicksForRemoval)
	_ZeroMotorRates
	SendStrLn(string("Levelled."))
	dira := oldDira

PRI CmdMotor
'' [0, kNumMaxMotors] motor => /sets current motor/
	' For now, only accept local motors
	hMotor := hStack <# constant(kNumLocalMotors - 1)

PRI CmdMotorP
'' motor? => /current motor/
'' Displays the current motor number.
	SendDecResponseLn(hMotor)

PRI CmdOff
'' /number/ off => /power bits matching high bits in number cleared/
'' Unsets the requested bits in TOS. Currently, only the first
'' board is supported.
	hPowerIn |= hStack

PRI CmdOn
'' /number/ on => /power bits matching high bits in number set/
'' Sets the requested bits in TOS. Currently, only the first
'' board is supported.
	hPowerIn &= !hStack

PRI CmdP
'' [-16, 16] d => /sets derivative coefficient/
	hP := hStack <# kUpperCoefficientBounds
	hP := hP #> kLowerCoefficientBounds

PRI CmdPP
'' d? => /derivative coefficient/
	_SendCoefficient(hP)

PRI CmdPing
'' ping => pong
'' Sends a 'pong' message back. Used to check if a prop
'' is connected to the serial port.
	SendResponseLn(string("pong"))

PRI CmdPressedP
'' pressed? => [T | F]
'' Displays T if the current button is pressed; F otherwise.
	if _PressedP
		SendResponseLn(@hDictTrue)
	else
		SendResponseLn(@hDictFalse)

PRI CmdReboot
'' reboot => /reboots devices/
'' Reboots the device.
	' TODO: Add multi-board support 
	reboot

PRI CmdRepeat : count
'' [count] repeat => /repeat last command count times/
'' Repeats the most recent command count times once/sec.
	count := hStack
	repeat count
		Evaluate(hLastToken)
		waitcnt(_CLKFREQ + cnt)

PRI CmdSeek
'' [left, right, top, bottom | 0, 3] seek => /moves carriage to edge/
'' Moves the platform until the correct button is hit.
	_WaitForEmptyMotorQueue
	_ZeroMotorRates

	case hStack
		kLeft:
			_InitHorizTrackSeeking
			CmdCounterClockwise
			_SelectButton(kLeftSwitch)
			'SendStrLn(string("Seeking left edge..."))
			_SeekHorizontalButton
		kRight:
			_InitHorizTrackSeeking
			CmdClockwise
			_SelectButton(kRightSwitch)
			'SendStrLn(string("Seeking right edge..."))
			_SeekHorizontalButton
		kTop:
			_InitVertTrackSeeking(kCmdCounterClockwise)
			'SendStrLn(string("Seeking top edge..."))
			repeat kMaxVerticalSeekSteps
				if _SeekVerticalButton(kCmdLeftTrack, kTopLeftSwitch) and _SeekVerticalButton(kCmdRightTrack, kTopRightSwitch)
					quit
				_StepMotorsOnce(kVerticalSeekRate)
			_ZeroMotorRates
		kBottom:
			_InitVertTrackSeeking(kCmdClockwise)
			'SendStrLn(string("Seeking bottom edge..."))
			repeat kMaxVerticalSeekSteps
				if _SeekVerticalButton(kCmdLeftTrack, kBottomLeftSwitch) or _SeekVerticalButton(kCmdRightTrack, kBottomRightSwitch)
					quit
				_StepMotorsOnce(kVerticalSeekRate)
			_ZeroMotorRates

	if hThisToken == kCmdSeek  and hReplMode == kModeText
		SendResponseLn(string("Done."))

PRI CmdSeekTest : count | first, second
'' [left | right | top | bottom] seek-test => /Repeatedly seeks/
'' Seeks horizontally or vertically until a byte is received;
'' halts after completing its current cycle.
	case hStack
		kLeft, kRight:
			first := kLeft
			second := kRight
		kBottom, kTop:
			first := kTop
			second := kBottom
		other:
			CmdError
			return
	count~
	repeat
		hStack := first
		CmdSeek
		hStack := second
		CmdSeek
		SendStr(string("Completed cycle "))
		SendDec(count++)
		SendStr(string(" at tick "))
		SendDec(hCurrentTick)
		SendStrLn(string("."))

PRI CmdSelectMotor(value)
'' Sets the current motor to the value-th motor.
'' Not called directly. No error checks, so care!
	hMotor := (value - kCmdPlatform)

PRI CmdSleep
'' sleep => /Enter powersave mode/
'' Turns off motor cogs.
	' Note that even though we keep the temp
	' and power cogs running, we turn off the
	' FETs here just in case.
	hPowerIn := $FF
	cogstop(kSwitchCog)
	cogstop(kMotorAxonCog)
	cogstop(kMotorDendriteCog)
	cogstop(kMotorTissueCog)

	hButtonState := 0
	hsNextMotion := 0
	hsNextActive := 0
	hCurrentTick := 0
	hQueueFront  := 0
	hQueueBack   := 0
	
	longfill(@hMotionQueue, 0, constant(kNumSlots * kMotionSlotSize))
	longfill(@hRateQueue,   0, constant(kNumSlots * kRateSlotSize))
	longfill(@hReplTicks, 0, 6)
	longfill(@hTargetTemp0, 0, 2)

PRI CmdSmallNumber
'' Places bits 6..0 onto the stack.
	hStack := hThisToken - kCmdSmallNumber

PRI CmdStackP
'' stack? => /value in stack/
'' Displays the stack value in decimal & hexidecimal.
	_SendStrValue(hStack)

PRI CmdStep : offset | t
'' /number/ step => /takes number global steps/
'' Steps TOS steps forward in time; this is used to
'' synchronize state updates with motor movement.
	hReplTicks += hStack
	' TODO: Send to satellites FIRST.

	repeat until (hQueueBack <> ((hQueueFront + 1) & kmNumSlots))
	offset := hQueueFront << 2 	' Convert number to longs.

	longmove(@hMotionQueue + offset * kMotionSlotSize, {
		}@hReplTicks, kMotionSlotSize)	
	longmove(@hRateQueue + offset * kRateSlotSize, {
		}@hReplRate0, kRateSlotSize)

{
	SendStr(string("State: $"))
	SendHexLn(long[@hMotionQueue + offset * kMotionSlotSize][1], 8)
	SendStr(string("Rate0: "))
	SendDecLn(long[@hRateQueue + offset * kRateSlotSize + 0] & $FF_FF)
	SendStr(string("Rate1: "))
	SendDecLn(long[@hRateQueue + offset * kRateSlotSize + 0] >> 16)
	SendStr(string("Rate2: "))
	SendDecLn(long[@hRateQueue + offset * kRateSlotSize + 4] & $FF_FF)
	SendStr(string("Rate3: "))
	SendDecLn(long[@hRateQueue + offset * kRateSlotSize + 4] >> 16)
	SendStr(string("Rate4: "))
	SendDecLn(long[@hRateQueue + offset * kRateSlotSize + 8] & $FF_FF)
	SendStr(string("Rate5: "))
	SendDecLn(long[@hRateQueue + offset * kRateSlotSize + 8] >> 16)
	SendStr(string("Rate6: "))
	SendDecLn(long[@hRateQueue + offset * kRateSlotSize + 12] & $FF_FF)
	SendStr(string("Rate7: "))
	SendDecLn(long[@hRateQueue + offset * kRateSlotSize + 12] >> 16)
}
	hQueueFront := (hQueueFront + 1) & kmNumSlots	

PRI CmdStepRate
'' /number/ step-rate => /step current motor once every number steps/
'' Sets the step rate for the current motor. Disable
'' by using 0 step-rate.
	word[@hReplRate0][hMotor] := hStack

	' TODO: Send to satellites if needed.

PRI CmdStepRateP
'' step-rate? => /step rate of current motor/
'' Sends the current motor's step rate.
	SendDecResponseLn(word[@hReplRate0][hMotor])

PRI CmdStepRateStep
'' step-rate-step => /same as step-rate step/
'' Compresses the 'step-size step' sequence into one word.
	CmdStepRate
	CmdStep

PRI CmdStepSize : offset
'' /[1/1, 1/2, 1/4, 1/8, 1/16] step-size => /assign step size for current motor/
'' Sets the step size for the current motor.
	offset := ((hMotor & kmLocalMotor) << 2)

	long[@hReplState][hMotor >> 3] &= !(kmStepSize << offset)
	long[@hReplState][hMotor >> 3] |= hStack << offset
	' TODO: Send to satellites if needed.

PRI CmdStepSizeP : size
'' step-size? => [1/1, 1/2, 1/4, 1/8, 1/16]
'' Returns the step size of the current motor.
	size := (long[@hReplState][hMotor >> 3] >> {
}		((hMotor & kmLocalMotor) << 2)) & kmStepSize

	case size
		kStepperWhole: SendResponseLn(@hDictWhole)
		kStepperHalf: SendResponseLn(@hDictHalf)
		kStepperQuarter: SendResponseLn(@hDictQuarter)
		kStepperEighth: SendResponseLn(@hDictEighth)
		kStepperSixteenth: SendResponseLn(@hDictSixteenth)
		other:
			SendStr(string("Error: Unknown step size $"))
			SendHexLn(size, 8)

PRI CmdStop
'' stop => /current motor 0 step-rate/
	word[@hReplRate0][hMotor] := 0

PRI CmdTemperatureP
'' temperature? => [0, ...)
'' Queries the current heater block's temperature.
	_SendTemperature(long[@hCurrentTemp0][hHeater])

PRI CmdTemperaturesP
'' temperatures? => /temperatures for each heater/
'' Queries all heater block temperatures.
	SendStr(string(13,10,"Thermistor 0: "))
	_SendTemperature(hCurrentTemp0)
	SendStr(string("Thermistor 1: "))
	_SendTemperature(hCurrentTemp1)

PRI CmdTemperatureTargetP
'' temperature-target? => [0, ...)
'' Queries the current heater block's temperature target.
	SendDecResponseLn(long[@hTargetTemp0][hHeater])

PRI CmdText
'' text => /text mode/
'' Returns to text mode.
	hReplMode := kModeText

PRI CmdTickP
'' tick? => /last taken tick/
'' Returns the latest tick the motor cogs have taken.
	SendDecResponseLn(hCurrentTick)

PRI CmdTickTargetP
'' tick-target? => /the target tick number/
'' Returns the target tick for the motor cogs.
	SendDecResponseLn(hReplTicks)

PRI CmdTokenP
'' token? => /string for token, if any/
'' Converts the value in the top of stack into its printed
'' form, if it exists.
	_SendStrToken(hStack)

PRI CmdVersionP
'' version? => /version/
'' Prints out our version information. For now, this
'' is the build/check-in number.
	SendResponse(@hBuildVersion)

PRI CmdWaitHeated
'' wait-heated => /waits/
'' Waits until the current heater is up to temperature.
'' This is signified by the heater duty being 0.

' Note that we can add a timeout here if we want.
	repeat
		if long[@hHeaterDuty0][hHeater] == 0
			quit
		waitcnt(_CLKFREQ + cnt)

PRI CmdWake
'' wake => /Pr=pares for printing/
'' Enables motor cogs.
	result := cnt
	hcAxonStartCycle	 := constant(kAxonStartCycle     + kSafeCogStartDelay) + result
	hcDendriteStartCycle := constant(kDendriteStartCycle + kSafeCogStartDelay) + result
	hcTissueStartCycle   := constant(kTissueStartCycle   + kSafeCogStartDelay) + result

	coginit(kMotorAxonCog,     @InitializeAxonCog,     0)
	coginit(kMotorDendriteCog, @InitializeDendriteCog, 0)
	coginit(kMotorTissueCog,   @InitializeTissueCog,   0)
	coginit(kSwitchCog,      @InitializeSwitchCog,      0)

PRI CmdWordsP : addr
'' words? => /words/
'' Displays all dictionary words.
	SendStr(@hNewLine)
	
	addr := @hDictionaryStart + 6
	repeat while word[addr] <> 0
		SendStrLn(word[addr])
		addr += 8

PRI _InitHorizTrackSeeking
'' Prepares the horiz track for seeking.
	CmdSelectMotor(kCmdHorizTrack)
	hStack := kHorizontalSeekSize
	CmdStepSize
	hReplRate1 := kHorizontalSeekRate

PRI _InitVertTrackSeeking(dirCommand)
'' Prepares the vert tracks for seeking.
	CmdSelectMotor(kCmdLeftTrack)
	hStack := kVerticalSeekSize
	CmdStepSize
	hReplRate2 := kVerticalSeekRate
	Evaluate(dirCommand)

	CmdSelectMotor(kCmdRightTrack)
	hStack := kVerticalSeekSize
	CmdStepSize
	hReplRate3 := kVerticalSeekRate
	Evaluate(dirCommand)

PRI _LevellingStep(left, right)
'' < 0 for down, >0 for up
	CmdSelectMotor(kCmdLeftTrack)
	hReplRate2 := kLevelStepRate
	if left < 0
		CmdClockwise
	else
		CmdCounterClockwise
	||left

	CmdSelectMotor(kCmdRightTrack)
	hReplRate3 := kLevelStepRate
	if right < 0
		CmdClockwise
	else
		CmdCounterClockwise
	||right

	hStack := (left <# right) * kLevelStepRate
	CmdStep
	if left < right
		hReplRate2 := 0
	else
		hReplRate3 := 0
	hStack := ||(left - right) * kLevelStepRate
	CmdStep
	_WaitForEmptyMotorQueue
		
PRI _PressedP
'' Queries the current button to see if it's pressed or not.
	result := hButtonState & hmButton <> 0

PRI _SeekHorizontalButton
'' Moves until the current horizontal button is pressed,
'' one step at a time.
	repeat kMaxHorizontalSeekSteps
		if _PressedP
			_ZeroMotorRates
			quit
		_StepMotorsOnce(kHorizontalSeekRate)

PRI _SeekVerticalButtons(left, right)
'' Moves until the the left and right vertical buttons 
'' are pressed.
	repeat kMaxVerticalSeekSteps
		if _SeekVerticalButton(kCmdLeftTrack, left) or _SeekVerticalButton(kCmdRightTrack, right)
			quit
		_StepMotorsOnce(kVerticalSeekRate)

PRI _SeekVerticalButton(motor, button)
'' Move the specific vertical motor until the button
'' is hit.
	CmdSelectMotor(motor)
	_SelectButton(button)

	if _PressedP or word[@hReplRate0][hMotor] == 0
		word[@hReplRate0][hMotor] := 0
		return true
	return false

PRI _SelectButton(aButton)
'' Selects the provided button.
	hStack := aButton
	CmdButton

PRI _SendCoefficient(aCoefficient)
'' Sends the provided coefficient.
	if aCoefficient =< kLowerCoefficientBounds or {
}		aCoefficient => kUpperCoefficientBounds
		SendResponseLn(string("disabled"))
		return
	elseif aCoefficient < 0
		SendResponse(string(">> "))
	else
		SendResponse(string("<< "))
	SendDecResponseLn(||aCoefficient)

PRI _SendStrToken(aToken)
'' Prints the string representation of the token, if possible,
'' or the raw value otherwise.
''
'' aToken - The token to decompile and SendStr
	' We have the index; if it's less than kCmdLength, it's valid
	if aToken => kCmdLength
		_SendStrValue(aToken)
	else
		' Ok, it's valid. Just go to the right value.
		SendResponseLn(word[@hDictionaryStart][(aToken << 2) + 3])

PRI _SendStrValue(aValue)
'' SendStrs the provided value in decimal and hexidecimal.
	SendDecResponse(aValue)
	SendStr(string(" (0x"))
	SendHex(aValue, 8)
	SendStrLn(string(")"))

PRI _SendTemperature(aTemperature)
'' Sends a temperature or defective.
	if aTemperature == kTempErrorResult
		SendResponseLn(string("defective?"))
	else
		SendDecResponseLn(aTemperature)

PRI _StepMotorsOnce(atRate)
	_WaitForEmptyMotorQueue
	hStack := atRate
	CmdStep

PRI _Wait(minutes, seconds)
	repeat while minutes > 0 or seconds > 0
		waitcnt(_clkfreq + cnt)
		--seconds
		if (seconds < 1)
			if (minutes-- > 0)
				seconds := 59

PRI _WaitForEmptyMotorQueue
'' Waits until the motor queue is empty
	repeat while (hQueueFront <> hQueueBack)

PRI _ZeroMotorRates
'' Zero local & remote motor rates.
	wordfill(@hReplRate0, 0, kNumLocalMotors)
	' Zero remote motor rates.

{{ Power cog ==============================================
	Controls FET output, using PWM for heater control.

	Note that FET logic is inverted (0 = on, 1 = off).

	Datasheets --------------------------------------------
	Heater: 			N/A
	Shift Registers:	http://bit.ly/oncOl1
}}
CON ' Power constants -------------------------------------
	kmPowerSer    = 1 << kpPowerSer
	kmPowerSck    = 1 << kpPowerSck
	kmPowerRck    = 1 << kpPowerRck
	kmPowerClr    = 1 << kpPowerClr

	kmPowerDira = kmPowerSer | kmPowerSck | kmPowerRck {
}		| kmPowerClr

	kTotalDutyCycles = 250_000

DAT ' Power hub variables ---------------------------------
' Input from user.
hPowerIn	byte $FF ' $FF is OFF.

' Updated by temperature cog.
hPowerOut   byte $FF

DAT ' Power code ------------------------------------------
			org		0
InitializePowerCog
			mov		outa, cmPowerClr
			mov		dira, cmPowerDira

			mov		cNextDutyCycle, cTicksPerDuty
			add		cNextDutyCycle, cnt
PowerSyncLoop ' Syncronize with heater cog.
			' Read the FET bits from the user
			' and the temperature cog.
			rdbyte	cmNextPower, caFetOut
			mov		cDutyLeft, cDutyCycles
			' nop

			rdlong	cHeaterDuty0, caHeaterDuty0
			' nop x2
			rdlong	cHeaterDuty1, caHeaterDuty1
' 40-63 cycles
ReadyDutyLoop ' Turn heaters on or off as needed.
			' FET bits are 0-7.
			cmpsub	cHeaterDuty0, #1					wc
			muxnc	cmNextPower, cmHeater0
			cmpsub	cHeaterDuty1, #1					wc
			muxnc	cmNextPower, cmHeater1

			' MSB-align the bits to shift.
			rol		cmNextPower, #24

			' Ready the loop counter.
			mov		cmPowerBits, #8
' 24 cycles
ShiftPower	' Update the power bits in hub & shift register.
			rol		cmNextPower, #1						wc
			muxc	outa, cmPowerSer
			or		outa, cmPowerSck
			andn	outa, cmPowerSck
			djnz	cmPowerBits, #ShiftPower
' 44 cycles
			' Wait until previous cycle completes…
			waitcnt	cNextDutyCycle, cTicksPerDuty
			' Latch the new cycle.
			or		outa, cmPowerRck
			andn	outa, cmPowerRck

			' Continue the duty cycle.
			djnz	cDutyLeft, #ReadyDutyLoop
' 18+ cycles
			' All done.
			jmp		#PowerSyncLoop

DAT ' Power initialized variables -------------------------
cmPowerDira		long kmPowerDira
cmPowerSer		long kmPowerSer
cmPowerSck		long kmPowerSck
cmPowerRck		long kmPowerRck
cmPowerClr		long kmPowerClr

caFetOut		long @@@hPowerOut

caHeaterDuty0	long @@@hHeaterDuty0
caHeaterDuty1	long @@@hHeaterDuty1
cmHeater0		long kmHeater0
cmHeater1		long kmHeater1

cDutyCycles		long kTotalDutyCycles
cTicksPerDuty	long _CLKFREQ / kTotalDutyCycles

DAT ' Power uninitialized variables -----------------------
cNextDutyCycle		res 1

cHeaterDuty0		res 1
cHeaterDuty1		res 1
cDutyLeft			res 1

cmPowerBits			res 1
cmNextPower			res 1
			fit		$1F0

{{ Temperature cog ========================================
	Controls the heater to maintain the correct temperature.

	Safety measures include a maximum heating time, very
	large temp readings for unplugged thermistors, and
	no permanent blocking.

	Uses a piecewise linear function of five parts, Eq0…Eq4.
	Eq0 is for the highest temperatures (and lowest 
	resistances), and Eq4 the lowest temps (and highest 
	resistances). Each M value should be a negative value 
	with the absolute value listed in the constants---the
	negation will occur later in the calculation.

	Datasheets --------------------------------------------
	Thermistors:		http://bit.ly/YWtwnW
}}
CON ' Temperature constants -------------------------------
	kmThermistor0 = 1 << kpThermistor0
	kmThermistor1 = 1 << kpThermistor1
	
	kmTemperatureDira = kmThermistor0 | kmThermistor1

	' Should be low enough to prevent the PEEK tubes
	' from melting. This is in °C.
	kMaxTemperature = 230
	kTempErrorResult = $7F_FF_FF_FF		' Defective result.
	
	kLowerCoefficientBounds = -16
	kUpperCoefficientBounds = 16

	kWarningLevel = 25					' In °C

	' Piecewise Linear Equation ___________________________
	' Maximum Time Values From Rc Time.
	Keq1mint = 30_300
	Keq2mint = 37_450
	Keq3mint = 90_000
	Keq4mint = 275_000
	Keq5mint = 950_000
	Keq6mint = 3_100_000

	' Slope + Offset Values In 16.16; Fp
	' Converts Ieee-754 Floats To 16.16 Fixed.
	' Note That This Requires Running M4 On This
	' File Before Passing The Output To Bstc.
	' Note M Values *Must* Be Negative And Written
	' *Positive*. Results Will Be Multiplied By -1.

	' 1.31 Format
define(`FP', TRUNC($1 * FLOAT(1 << 31)))dnl
	Keq0m = FP(0.002592)
	kEq0B = 292

	kEq1M = FP(0.001992)
	kEq1B = 275

	kEq2M = FP(0.001179)
	kEq2B = 240

	kEq3M = FP(0.000271)
	kEq3B = 166

	kEq4M = FP(0.000069)
	kEq4B = 113

	kEq5M = FP(0.000016)
	kEq5B = 68

	kEq6M = FP(0.000005)
	kEq6B = 31

DAT ' Temperature hub variables ---------------------------
hCurrentTemp0	long 0
hCurrentTemp1	long 0

hTargetTemp0	long 0
hTargetTemp1	long 0 

' Valid range is [-15, 15]; other values indicate a 0
' coefficient.
hP				long 13 ' 14 oscillates & overheats!
hI				long 7	' 5; 6 is good
hD				long 5 

hHeaterDuty0	long 0
hHeaterDuty1	long 0

DAT ' Temperature code ------------------------------------
			org		0
InitializeTemperatureCog
			mov		outa, #0
			mov		dira, cmTemperatureDira
			' Prepare the pre-shift sync.
			mov		cNextTempSync, cTempSyncTicks
			add		cNextTempSync, cnt
TempSync ' Sync times.
			waitcnt	cNextTempSync, cTempSyncTicks
Charge ' Charge the capacitors. 
			or		outa, cmThermistors
			or		dira, cmThermistors
			mov		cTempSync, cChargeTicks
			add		cTempSync, cnt
			waitcnt	cTempSync, #0
Discharge ' Discharge the capacitors.
			' By default, defective thermistors.
			mov		cTherm0Cnt, #0
			mov		cTherm1Cnt, #0

			' Set to input & ready the early break.
			andn	dira, cmThermistors
			mov		cmActiveThermistors, cmThermistors

			' PowerSync syncs the cog;
			' checkTicks is checked for the loop.
			mov		cTempSync, cDischargeTicks
			mov		cCheckTicks, cDischargeTicks

			' DischargeStart caches the cnt to
			' later calculate the delta.
			mov		cDischargeStart, cnt
			add		cTempSync, cDischargeStart
CheckThermistors ' Check each pin individually.
			test	cmThermistor0, ina				wz
T0	if_z	mov		cTherm0Cnt, cnt
	if_z	andn	cmActiveThermistors, cmThermistor0
	if_z	movs	T0, #cTherm0Cnt

			test	cmThermistor1, ina				wz
T1	if_z	mov		cTherm1Cnt, cnt
	if_z	andn	cmActiveThermistors, cmThermistor1
	if_z	movs	T1, #cTherm1Cnt

			tjz		cmActiveThermistors, #DischargeSync
			cmpsub	cCheckTicks, cCheckLength		wc
	if_c	jmp		#CheckThermistors
DischargeSync ' Power saving at higher temps.
			' Ensure we're deterministic.
			waitcnt	cTempSync, #0

			' Restore the checks.
			movs	T0, #cnt
			movs	T1, #cnt

			' Put the delta in cTherm?Cnt if the
			' cnt is valid.
			test	cmActiveThermistors, cmThermistor0	wc
	if_nc	sub		cTherm0Cnt, cDischargeStart
			test	cmActiveThermistors, cmThermistor1	wc
	if_nc	sub		cTherm1Cnt, cDischargeStart

			' Prepare PID coefficients. Positive values
			' indicate a left shift; negative numbers,
			' a right shift. An |number| > 16 indicates
			' no shift
			rdlong	cPidError, caP
			' Negative numbers will have the high bit set…
			abs		cP, cPidError
			cmp		cP, #16								wc
	if_nc	mov		CalcP, cPZero
	if_nc	jmp		#ReadI
			cmps	cPidError, #0						wc
	if_c	mov		CalcP, cPSar
	if_nc	mov		CalcP, cPShl

ReadI		rdlong	cPidError, caI
			abs		cI, cPidError
			cmp		cI, #16								wc
	if_nc	mov		CalcI, cIZero
	if_nc	jmp		#ReadD
			cmps	cPidError, #0						wc
	if_c	mov		CalcI, cISar
	if_nc	mov		CalcI, cIShl

ReadD		rdlong	cPidError, caD
			abs		cD, cPidError
			cmp		cD, #16								wc
	if_nc	mov		CalcD, cDZero
	if_nc	jmp		#StartConversion
			cmps	cPidError, #0						wc
	if_c	mov		CalcD, cDSar
	if_nc	mov		CalcD, cDShl

StartConversion	' Use the RC values to read the temperatures.
			' Grab the user-set FET bits.
			rdbyte	cmPower, caPowerIn

			' Thermistor & heater 0.
			mov		cFactor0, cTherm0Cnt
			call	#ConvertCyclesToC
			wrlong	cTempResult, caCurrentTemp0

			' Ready the power LED. Ensure we set it
			' to a valid result using muxnc as andn
			' would require us ignoring it in one case.
			cmp		cTempResult, #25					wc
			muxnc	cmPower, cmWarning

			movs	LastI, #cISum0
			movd	StoreI, #cISum0
			rdlong	cPidError, caTargetTemp0			wz

			movs	LastTemp, #cLastTemp0
			movd	StoreLastT, #cLastTemp0
			call	#CalculatePID
			' cPTerm has the duty
			wrlong	cPTerm, caHeaterDuty0Out

			' Thermistor & heater 1
			mov		cFactor0, cTherm1Cnt
			call	#ConvertCyclesToC
			wrlong	cTempResult, caCurrentTemp1

			' Update the power LED
			cmp		 cTempResult, #25					wc
	if_nc	andn	 cmPower, cmWarning
			wrbyte	 cmPower, caPowerOut

			movs	LastI, #cISum1
			movd	StoreI, #cISum1
			rdLong	cPidError, caTargetTemp1			wz

			movs	LastTemp, #cLastTemp1
			movd	StoreLastT, #cLastTemp1
			call	#CalculatePID
			wrlong	cPTerm, caHeaterDuty1Out
			
			' All done; jump back to sync.
			jmp		#TempSync

ConvertCyclesToC   ' cFactor0 := rctime 
			cmp		cFactor0, #0						wz
	if_z	mov		cTempResult, cErrorResult
	if_z	jmp		#ConvertCyclesToC_ret

			' Need M in cFactor1 and B in cEqOffset.
			' Set up the defaults.
			mov		cFactor1, cEq0M
			mov		cEqOffset, cEq0B

			' Check each of the other piecewise options.
			cmp		cEq1MinT, cFactor0					wc
	if_c	mov		cFactor1, cEq1M
	if_c	mov		cEqOffset, cEq1B

			cmp		cEq2MinT, cFactor0					wc
	if_c	mov		cFactor1, cEq2M
	if_c	mov		cEqOffset, cEq2B

			cmp		cEq3MinT, cFactor0					wc
	if_c	mov		cFactor1, cEq3M
	if_c	mov		cEqOffset, cEq3B

			cmp		cEq4MinT, cFactor0					wc
	if_c	mov		cFactor1, cEq4M
	if_c	mov		cEqOffset, cEq4B

			cmp		cEq5MinT, cFactor0					wc
	if_c	mov		cFactor1, cEq5M
	if_c	mov		cEqOffset, cEq5B

			cmp		cEq6MinT, cFactor0					wc
	if_c	mov		cFactor1, cEq6M
	if_c	mov		cEqOffset, cEq6B

			call	#FixedMult
			neg		cTempResult, cTempResult		' Reintroduce -1 from -M.
			adds	cTempResult, cEqOffset			' Add B.
			' Now have the temperature in C.
ConvertCyclesToC_ret ret ' Done.

CalculatePID ' Applies the PID controller to cTempResult
			' If the target temp is 0, we're done.
	if_z	mov		cPTerm, #0
	if_z	jmp		#CalculatePID_ret

			' If there's an error, we're done.
			' NOTE: Might change this for cUpperBounds
			cmp		cTempResult, cMaxTemperature		wc
	if_nc	mov		cPTerm, #0
	if_nc	jmp		#CalculatePID_ret

			' Prepare PID error
			subs	cPidError, cTempResult

			' Prepare P term.
			mov		cPTerm, cPidError
CalcP		shl		cPTerm, cP

			' Prepare I term.
			mov		cITerm, cPidError
CalcI		shl		cITerm, cI
LastI		adds	cITerm, cISum0
			maxs	cITerm, cUpperBounds
			mins	cITerm, #0
StoreI		mov		cISum0, cITerm

			' Prepare the D term.
			mov		cDTerm, cTempResult
LastTemp	subs	cDTerm, cLastTemp0
CalcD		shl		cDTerm, cD
StoreLastT	mov		cLastTemp0, cTempResult

			' Calculate the PID output
			adds	cPTerm, cITerm
			adds	cPTerm, cDTerm
			maxs	cPTerm, cUpperBounds
			mins	cPTerm, #0
			' cPTerm now contains the bounded output.
CalculatePID_ret	ret
' Commands used for self-modifying code
cPZero		xor		cPTerm, cPTerm
cPSar		sar		cPTerm, cP
cPShl		shl		cPTerm, cP

cIZero		xor		cITerm, cITerm
cISar		sar		cITerm, cI
cIShl		shl		cITerm, cI

cDZero		xor		cDTerm, cDTerm
cDSar		sar		cDTerm, cD
cDShl		shl		cDTerm, cD

'' See <http://www.sxlist.com/TECHREF/method/math/muldiv.htm>
'' and <http://www.x86asm.net/articles/fixed-point-arithmetic-and-tricks/>
FixedMult     ' fpResult := fpFactor0 * fpFactor1 in 16.16
			mov       cProdLow, #0           ' Zero all the registers.
			mov       cProdHigh, #0
			mov       cFactor1High, #0

:loop       ' See <http://www.sxlist.com/TECHREF/method/math/muldiv.htm>
			test      cFactor0, #1 						wz
	if_nz 	add       cProdLow, cFactor1 				wc
	if_nz 	addx      cProdHigh, cFactor1High
			shr       cFactor0, #1
			shl       cFactor1, #1 						wc
			rcl       cFactor1High, #1
			tjnz      cFactor0, #:loop

			' Convert 33.31 -> 32.0.
			shr		cProdLow, #31				' Have the LSB.
			shl		cProdHigh, #1				' Have the other 31 MSB.

			mov		cTempResult, cProdLow
			or		cTempResult, cProdHigh
FixedMult_ret ret       ' Done.


DAT ' Temperature initialized variables -------------------
cmTemperatureDira long kmTemperatureDira
cmThermistor0	long kmThermistor0
cmThermistor1	long kmThermistor1
cmThermistors	long kmThermistor0 | kmThermistor1

cChargeTicks	long _CLKFREQ >> 1		' Need >0.47 sec.
' Should take <0.33sec (->0.4sec) to discharge.
cDischargeTicks	long (_CLKFREQ * 4) / 10
cCheckLength	long 13 * 4				' 12 instructions of 4 ticks
cTempSyncTicks	long _CLKFREQ 			' Sync to 1 sec.

cErrorResult	long kTempErrorResult

caCurrentTemp0	long @@@hCurrentTemp0
caCurrentTemp1	long @@@hCurrentTemp1

caTargetTemp0	long @@@hTargetTemp0
caTargetTemp1	long @@@hTargetTemp1

caPowerIn		long @@@hPowerIn
caPowerOut		long @@@hPowerOut
cmWarning		long kmPowerLight

cISum0			long 0
cLastTemp0		long 0
cISum1			long 0
cLastTemp1		long 0

' Linear cycle equations for RC temperature.
cEq1MinT		long kEq1MinT
cEq2MinT		long kEq2MinT
cEq3MinT		long kEq3MinT
cEq4MinT		long kEq4MinT
cEq5MinT		long kEq5MinT
cEq6MinT		long kEq6MinT

cEq0M			long kEq0M
cEq0B			long kEq0B
cEq1M			long kEq1M
cEq1B			long kEq1B
cEq2M			long kEq2M
cEq2B			long kEq2B
cEq3M			long kEq3M
cEq3B			long kEq3B
cEq4M			long kEq4M
cEq4B			long kEq4B
cEq5M			long kEq5M
cEq5B			long kEq5B
cEq6M			long kEq6M
cEq6B			long kEq6B

caP				long @@@hP
caI				long @@@hI
caD				long @@@hD

cMaxTemperature long kMaxTemperature

cUpperBounds	long kTotalDutyCycles
caHeaterDuty0Out long @@@hHeaterDuty0
caHeaterDuty1Out long @@@hHeaterDuty1

DAT ' Temperature uninitialized variables ------------------
cNextTempSync		res 1
cTempSync			res 1
cLoopCnt			res 1
cTherm0Cnt			res 1
cTherm1Cnt			res 1

cmPower				res 1

cDischargeStart		res 1
cCheckTicks			res 1
cmActiveThermistors res 1

cFactor0			res 1	' For fixed point math.
cFactor1			res 1
cEqOffset			res 1
cProdLow			res 1
cProdHigh			res 1
cFactor1High		res 1
cTempResult			res 1

cPidError			res 1
cPidErrorSum		res 1
cPidErrorDelta		res 1

cP					res 1
cI					res 1
cD					res 1

cPTerm				res 1
cITerm				res 1
cDTerm				res 1
			fit		$1F0

{{ Motor axon cog =========================================
	First stage of the motor pipeline. Reads from the motor
	queue when needed; otherwise, determines which
	directions and sizes are required for movement and
	which motors to activate.

	Write to hcAxonStartCycle to set the initial starting
	cycle before starting the cog.

	Datasheets --------------------------------------------
	Motor Driver:		http://bit.ly/18rJ4YN
	Shift Registers:	http://bit.ly/oncOl1

	Motion Slot Layout ------------------------------------
	+- Low Memory-+- High Memory +
	| Target tick | Motion state |
	+--- long ----+---- long ----+

	Motion State Layout
	+--- MSB ----+---------+---------+---------+---+
	| Direction7 | Size7 2 | Size7 1 | Size7 0 | … |
	+---- bit ---+-- bit --+-- bit --+-- bit --+---+

	Rate Slot Layout --------------------------------------
	+- Low memory -+--------------+---+--------------+
	| Motor 0 Rate | Motor 1 Rate | … | Motor 7 Rate |
	+--- short ----+--- short ----+---+--- short ----+

	Next Active Layout ------------------------------------
	+- MSB --+--------+--------+---+-- LSB -+
	| Motor7 | Motor6 | Motor5 | … | Motor0 |
	+- bit --+- bit --+- bit --+---+-- bit -+
}}
CON ' Motor axon constants --------------------------------
' The number of CPU cycles per loop execution.
kCyclesPerTick = 500
kAxonStartCycle = 0

' The maximum number of motion states and step-rates.
kNumSlots		= 256
kmNumSlots		= kNumSlots - 1
kMotionSlotSize = 2  ' Target step, motor direction & size.
kRateSlotSize	= 4  ' 16-bit step rates for each motor.

 ' Ccw/cw location in motion masks.
kmHorizDir	   = 1 << (kHorizontalMotor    * 4 + 3)
kmLeftVertDir  = 1 << (kLeftVerticalMotor  * 4 + 3) 
kmRightVertDir = 1 << (kRightVerticalMotor * 4 + 3) 

' Active motors are zero.
kmNoActiveMotors = $FF

DAT ' Motor axon hub variables ----------------------------
hMotionQueue	long 0[kNumSlots * kMotionSlotSize]
hRateQueue		long 0[kNumSlots * kRateSlotSize]
hQueueFront		long 0
hQueueBack		long 0

' hcAxonStartCycle is in initialized hub variables.

hCurrentTick	long 0 ' The just-completed tick.
hsNextMotion	long 0 ' Dirs + sizes for dendrite.
hsNextActive	byte 0 ' Motors for tissue to step.

DAT ' Motor axon code -------------------------------------
			org		0
InitializeAxonCog
			rdlong  cQueueBack, caQueueBack
			mov		cQueueNextTail, cQueueBack
			mov		outa, #0		' Prevent floating.

			mov		cmActiveOut, #kmNoActiveMotors
			mov		dira, #0		' Don't use pins.
' 40-55 cycles
SyncAxon   ' Sync with dendrite & tissue.
			mov		cNextAxonCycle, hcAxonStartCycle
			waitcnt cNextAxonCycle, #0

			mov		cNextAxonCycle, cAxonCycles
			add		cNextAxonCycle, cnt
SyncAxonLoop ' Everything after this must be deterministic.
			waitcnt cNextAxonCycle, cAxonCycles
'6+ cycles
CheckTicks  ' Only check the queue if current = target ticks.
			cmp		cCurrentTick, cTargetTick		wz
	if_nz   jmp		#ReadState ' current ≠ target; skip the queue.
'8 cycles; 14+ total
CheckQueue ' Are we waiting for anything?
			rdlong  cQueueScratchA, caQueueFront
			' Increment after finishing the last state.
			mov		cQueueBack, cQueueNextTail
			cmp		cQueueScratchA, cQueueBack		wz
			wrlong	cQueueBack, caQueueBack
			' If nothing to do, then zero out active motors.
	if_z	jmp		#WriteActiveMotors
			' We can dispose of the old state.
			' Ensure next tail is valid.
			add		cQueueNextTail, #1
			and		cQueueNextTail, cmNumSlots
'36 cycles; 50+ total
ReadState   ' Dequeue tick & motor state data.
			mov		caStateData, cQueueBack
			' 8 bytes per 2 longs of kMotionSlotSize
			shl		caStateData, #3
			add		caStateData, caMotionQueue
			rdlong  cTargetTick, caStateData
			add		caStateData, #4
			mov		caRateData, cQueueBack
			rdlong  csNextMotionOut, caStateData
'36 cycles; 86+ total
ReadyRate   ' Prepare to read rate data & write the next state.
			' 16 bytes per 4 longs of kRateSlotSize
			shl		caRateData, #4
			add		caRateData, caRateQueue
'8 cycles; 94+ total.
ReadRates   ' Dequeue rates, resetting count to 1 if rate differs.
			' Interleave reading to hit hub sweet spot.
			rdlong	cQueueScratchA, caRateData
			mov		cQueueScratchB, cQueueScratchA
			and		cQueueScratchA, cmFirstRate
			add		caRateData, #4
			shr		cQueueScratchB, #16	

			cmp		cMotorRate0, cQueueScratchA		wz
	if_nz	mov		cMotorCount0, #1
			mov		cMotorRate0, cQueueScratchA

			cmp		cMotorRate1, cQueueScratchB		wz
	if_nz	mov		cMotorCount1, #1
			mov		cMotorRate1, cQueueScratchB

			rdlong	cQueueScratchA, caRateData
			mov		cQueueScratchB, cQueueScratchA
			and		cQueueScratchA, cmFirstRate
			add		caRateData, #4
			shr		cQueueScratchB, #16

			cmp		cMotorRate2, cQueueScratchA		wz
	if_nz	mov		cMotorCount2, #1
			mov		cMotorRate2, cQueueScratchA

			cmp		cMotorRate3, cQueueScratchB		wz
	if_nz	mov		cMotorCount3, #1
			mov		cMotorRate3, cQueueScratchB

			rdlong	cQueueScratchA, caRateData
			mov		cQueueScratchB, cQueueScratchA
			and		cQueueScratchA, cmFirstRate
			add		caRateData, #4
			shr		cQueueScratchB, #16	

			cmp		cMotorRate4, cQueueScratchA		wz
	if_nz	mov		cMotorCount4, #1
			mov		cMotorRate4, cQueueScratchA

			cmp		cMotorRate5, cQueueScratchB		wz
	if_nz	mov		cMotorCount5, #1
			mov		cMotorRate5, cQueueScratchB

			rdlong	cQueueScratchA, caRateData
			mov		cQueueScratchB, cQueueScratchA
			and		cQueueScratchA, cmFirstRate
			add		cCurrentTick, #1					' Hub sync A0
			shr		cQueueScratchB, #16	

			cmp		cMotorRate6, cQueueScratchA		wz
	if_nz	mov		cMotorCount6, #1
			mov		cMotorRate6, cQueueScratchA

			cmp		cMotorRate7, cQueueScratchB		wz
	if_nz	mov		cMotorCount7, #1
			mov		cMotorRate7, cQueueScratchB
' 192 cycles; 286+ total.
SetActiveMotors ' Determine motors to step.
		' Active: Positive step rate & time to step.
		' Set nc for non-zero step rates.
		' Set z when it's time to step.
			wrlong  csNextMotionOut, caNextMotionOut	' Hub sync

			cmp		cMotorRate0,  #1				wc
	  if_nc cmpsub  cMotorCount0, #1				wz
			wrlong  cCurrentTick, caCurrentTick			' Hub sync A1
	  if_nc	muxnz	cmActiveOut,  #$01
if_nc_and_z mov		cMotorCount0, cMotorRate0
		
			cmp		cMotorRate1,  #1				wc
	  if_nc cmpsub  cMotorCount1, #1				wz
	  if_nc muxnz	cmActiveOut,  #$02
if_nc_and_z mov		cMotorCount1, cMotorRate1
		
			cmp		cMotorRate2,  #1				wc
	  if_nc cmpsub  cMotorCount2, #1				wz
	  if_nc muxnz	cmActiveOut,  #$04
if_nc_and_z mov		cMotorCount2, cMotorRate2
		
			cmp		cMotorRate3,  #1	   			wc
	  if_nc cmpsub  cMotorCount3, #1				wz
	  if_nc muxnz	cmActiveOut,  #$08
if_nc_and_z mov		cMotorCount3, cMotorRate3
		
			cmp		cMotorRate4,  #1				wc
	  if_nc cmpsub  cMotorCount4, #1				wz
	  if_nc muxnz	cmActiveOut,  #$10
if_nc_and_z mov		cMotorCount4, cMotorRate4
		
			cmp		cMotorRate5,  #1				wc
	  if_nc cmpsub  cMotorCount5, #1				wz
	  if_nc muxnz	cmActiveOut,  #$20
if_nc_and_z mov		cMotorCount5, cMotorRate5
		
			cmp		cMotorRate6,  #1				wc
	  if_nc cmpsub  cMotorCount6, #1				wz
	  if_nc muxnz	cmActiveOut,  #$40
if_nc_and_z mov		cMotorCount6, cMotorRate6
		
			cmp		cMotorRate7,  #1				wc
	  if_nc cmpsub  cMotorCount7, #1		   		wz
	  if_nc muxnz	cmActiveOut,  #$80
if_nc_and_z mov		cMotorCount7, cMotorRate7
' 152 cycles; 438+ total.
WriteActiveMotors ' Write the next active motors to the hub.
			' Active motors are LOW; initally, all are inactive.
			wrbyte  cmActiveOut, caNextActiveOut
			mov		cmActiveOut, #kmNoActiveMotors
			jmp		#SyncAxonLoop
'Base/Sat/Empty queue and at target
' 20 cycles; 458 total.

DAT ' Motor axon initialized variables --------------------
hcAxonStartCycle long 0   ' Used to sync motor cogs.

cAxonCycles		long kCyclesPerTick

caMotionQueue	long @@@hMotionQueue
caRateQueue		long @@@hRateQueue
caQueueFront	long @@@hQueueFront
caQueueBack		long @@@hQueueBack
cmNumSlots		long kNumSlots - 1

caNextMotionOut	long @@@hsNextMotion
caNextActiveOut	long @@@hsNextActive

caCurrentTick	long @@@hCurrentTick
cCurrentTick	long 0
cTargetTick		long 0

cmFirstRate		long $FF_FF

' Motors start not-stepping.
cMotorRate0	long 0
cMotorRate1	long 0
cMotorRate2	long 0
cMotorRate3	long 0
cMotorRate4	long 0
cMotorRate5	long 0
cMotorRate6	long 0
cMotorRate7	long 0

DAT ' Motor axon uninitialized variables ------------------
cAxonBoardId	res 1

cNextAxonCycle	res 1
cQueueBack		res 1
cQueueNextTail	res 1
caStateData		res 1
caRateData		res 1
cQueueScratchA	res 1
cQueueScratchB	res 1

csNextMotionOut	res 1

cMotorCount0	res 1
cMotorCount1	res 1
cMotorCount2	res 1
cMotorCount3	res 1
cMotorCount4	res 1
cMotorCount5	res 1
cMotorCount6	res 1
cMotorCount7	res 1

cmActiveOut		res 1
			fit		$1F0

{{ Motor dendrite cog =====================================
	Second stage of the motor control pipeline. Shifts out
	direction and step size data for each of the motors.

	Write to hcDendriteStartCycle to set the initial 
	starting cycle before starting the cog.

	Datasheets --------------------------------------------
	Motor Driver:		http://bit.ly/18rJ4YN
	Shift Registers:	http://bit.ly/oncOl1
}}
CON ' Motor dendrite constants ----------------------------
	kDendriteStartCycle = 25

	kmLowMotionSer	  = 1 << kpLowMotionSer 
	kmLowMotionShift  = 1 << kpLowMotionShift 
	kmLowMotionClr	  = 1 << kpLowMotionClr
	kmHighMotionSer   = 1 << kpHighMotionSer
	kmHighMotionShift = 1 << kpHighMotionShift
	kmHighMotionClr   = 1 << kpHighMotionClr

	kmMotionClr   = kmLowMotionClr | kmHighMotionClr
	kmMotionShift = kmLowMotionShift | kmHighMotionShift
	kmMotionDira  = kmLowMotionSer | kmHighMotionSer | {
}			kmMotionClr | kmMotionShift 

DAT ' Motor dendrite hub variables ------------------------

' hcDendriteStartCycle is in initialized variables.

DAT ' Motor dendrite code ---------------------------------
	   		org		0
InitializeDendriteCog
			' Clear existing outputs.
			mov		outa, #0
			mov		dira, cmMotionDira

			' Enable shifting.
			mov		outa, cmEnableMotion

			' Latch the zero output values.
			or		outa, cmMotionShift
			andn	outa, cmMotionShift
' 20 cycles.
SyncDendrite ' Sync with axon & tissue.
			mov		cNextDendriteCycle, hcDendriteStartCycle
			waitcnt cNextDendriteCycle, #0

			mov		cNextDendriteCycle, cDendriteCycles
			add		cNextDendriteCycle, cnt
SyncDendriteLoop ' Deterministic loop.
			waitcnt cNextDendriteCycle, cDendriteCycles
			rdlong  csNextMotionIn, caNextMotionIn

			' Only shift if we need to.
			cmp		csNextMotionIn, csCurrentMotion wz
	if_z	jmp		#SyncDendriteLoop
'22+ cycles.
ReadyMotionShift ' MSB-align the high & low bits.
			mov		csLowMotionBits, csNextMotionIn
			shl		csLowMotionBits, #16
			mov		csHighMotionBits, csNextMotionIn
			mov		csCurrentMotion, csNextMotionIn
'16 cycles; 38+ cycles
ShiftMotionState ' Shift out 32 bits.
			' bits 15 & 31
			shl		csLowMotionBits, #1			wc
			muxc	outa, cmLowMotionSer
			shl		csHighMotionBits, #1		wc
			muxc	outa, cmHighMotionSer
			or		outa, cmMotionShift
			andn	outa, cmMotionShift

			' bits 14 & 30
			shl		csLowMotionBits, #1			wc
			muxc	outa, cmLowMotionSer
			shl		csHighMotionBits, #1		wc
			muxc	outa, cmHighMotionSer
			or		outa, cmMotionShift
			andn	outa, cmMotionShift

			' bits 13 & 29
			shl		csLowMotionBits, #1			wc
			muxc	outa, cmLowMotionSer
			shl		csHighMotionBits, #1		wc
			muxc	outa, cmHighMotionSer
			or		outa, cmMotionShift
			andn	outa, cmMotionShift

			' bits 12 & 28
			shl		csLowMotionBits, #1			wc
			muxc	outa, cmLowMotionSer
			shl		csHighMotionBits, #1		wc
			muxc	outa, cmHighMotionSer
			or		outa, cmMotionShift
			andn	outa, cmMotionShift

			' bits 11 & 27
			shl		csLowMotionBits, #1			wc
			muxc	outa, cmLowMotionSer
			shl		csHighMotionBits, #1		wc
			muxc	outa, cmHighMotionSer
			or		outa, cmMotionShift
			andn	outa, cmMotionShift

			' bits 10 & 26
			shl		csLowMotionBits, #1			wc
			muxc	outa, cmLowMotionSer
			shl		csHighMotionBits, #1		wc
			muxc	outa, cmHighMotionSer
			or		outa, cmMotionShift
			andn	outa, cmMotionShift

			' bits 9 & 25
			shl		csLowMotionBits, #1			wc
			muxc	outa, cmLowMotionSer
			shl		csHighMotionBits, #1		wc
			muxc	outa, cmHighMotionSer
			or		outa, cmMotionShift
			andn	outa, cmMotionShift

			' bits 8 & 24
			shl		csLowMotionBits, #1			wc
			muxc	outa, cmLowMotionSer
			shl		csHighMotionBits, #1		wc
			muxc	outa, cmHighMotionSer
			or		outa, cmMotionShift
			andn	outa, cmMotionShift

			' bits 7 & 23
			shl		csLowMotionBits, #1			wc
			muxc	outa, cmLowMotionSer
			shl		csHighMotionBits, #1		wc
			muxc	outa, cmHighMotionSer
			or		outa, cmMotionShift
			andn	outa, cmMotionShift
	
			' bits 6 & 22
			shl		csLowMotionBits, #1			wc
			muxc	outa, cmLowMotionSer
			shl		csHighMotionBits, #1		wc
			muxc	outa, cmHighMotionSer
			or		outa, cmMotionShift
			andn	outa, cmMotionShift

			' bits 5 & 21
			shl		csLowMotionBits, #1			wc
			muxc	outa, cmLowMotionSer
			shl		csHighMotionBits, #1		wc
			muxc	outa, cmHighMotionSer
			or		outa, cmMotionShift
			andn	outa, cmMotionShift

			' bits 4 & 20
			shl		csLowMotionBits, #1			wc
			muxc	outa, cmLowMotionSer
			shl		csHighMotionBits, #1		wc
			muxc	outa, cmHighMotionSer
			or		outa, cmMotionShift
			andn	outa, cmMotionShift

			' bits 3 & 19
			shl		csLowMotionBits, #1			wc
			muxc	outa, cmLowMotionSer
			shl		csHighMotionBits, #1		wc
			muxc	outa, cmHighMotionSer
			or		outa, cmMotionShift
			andn	outa, cmMotionShift

			' bits 2 & 18
			shl		csLowMotionBits, #1			wc
			muxc	outa, cmLowMotionSer
			shl		csHighMotionBits, #1		wc
			muxc	outa, cmHighMotionSer
			or		outa, cmMotionShift
			andn	outa, cmMotionShift

			' bits 1 & 17
			shl		csLowMotionBits, #1			wc
			muxc	outa, cmLowMotionSer
			shl		csHighMotionBits, #1		wc
			muxc	outa, cmHighMotionSer
			or		outa, cmMotionShift
			andn	outa, cmMotionShift

			' bits 0 & 16
			shl		csLowMotionBits, #1			wc
			muxc	outa, cmLowMotionSer
			shl		csHighMotionBits, #1		wc
			muxc	outa, cmHighMotionSer
			or		outa, cmMotionShift
			andn	outa, cmMotionShift
	
			' Latch the last bit.
			or		outa, cmMotionShift
			andn	outa, cmMotionShift

			' All done.
			jmp		#SyncDendriteLoop
' 396 cycles; 434 total.

DAT ' Motor dendrite initialized variables ----------------
' Start midway through axon.
hcDendriteStartCycle long kCyclesPerTick + kDendriteStartCycle
cDendriteCycles		 long kCyclesPerTick

cmEnableMotion	long kmMotionClr
cmMotionDira	long kmMotionDira
cmMotionShift	long kmMotionShift
cmLowMotionSer	long kmLowMotionSer
cmHighMotionSer long kmHighMotionSer

csCurrentMotion	long 0 
caNextMotionIn	long @@@hsNextMotion

DAT ' Motor dendrite uninitialized variables --------------
cNextDendriteCycle  res 1
csNextMotionIn		res 1
csLowMotionBits		res 1
csHighMotionBits	res 1
			fit		$1F0

{{ Motor Tissue Cog =======================================
	Shifts out the active motor state to cause stepping;
	the final stage in the motor control pipeline.

	Write to hcTissueStartCycle to set the initial starting
	cycle before starting the cog.

	Datasheets --------------------------------------------
	Motor Driver:		http://bit.ly/18rJ4YN
	Shift Registers:	http://bit.ly/oncOl1
}}
CON ' Motor tissue constants ------------------------------
	kTissueStartCycle = 50

	kmActiveSer  = 1 << kpActiveSer
	kmActiveSck  = 1 << kpActiveSck
	kmActiveRck  = 1 << kpActiveRck
	kmActiveClr  = 1 << kpActiveClr
	kmActiveSync = 1 << kpActiveSync

	kmActiveDira = kmActiveSer | kmActiveSck | kmActiveRck | {
}		kmActiveClr

	' The number of cycles base will wait for satellites
	' before setting the 'sync' pin high.
	kTissueSyncWindowCycles = 140

DAT ' Motor tissue hub variables --------------------------

' hcTissueStartCycle is in initialized variables.

DAT ' Motor tissue code -----------------------------------
			org		0
InitializeTissueCog
			' Clear existing outputs, ready high data.
			mov		outa, cmActiveSer
			mov		dira, cmActiveDira

			' Z=0: base; Z≠0: satellite. Can use
			' the flag throughout the cog.
			rdbyte  cTissueBoardId, caTissueBoardId wz
			' The base needs to signal satellites.
	if_z	or		dira, cmActiveSync
	if_z	mov		DetermineTissueSync, cBaseJump
			' Satellite doesn't check buttons.
	if_nz	mov		CheckButtons, cSatJump

			' Enable shifting.
			or		outa, cmEnableActive

			' Set everything high.
			mov		cActiveBitsLeft, #8
:loop		or		outa, cmActiveSck
			andn	outa, cmActiveSck
			djnz	cActiveBitsLeft, #:loop
			or		outa, cmActiveRck
			andn	outa, cmActiveRck
' 128 cycles.
SyncTissue ' Sync with axon & dendrite.
			mov		cNextTissueCycle, hcTissueStartCycle
			waitcnt cNextTissueCycle, #0

			mov		cNextTissueCycle, cTissueCycles
			add		cNextTissueCycle, cnt
SyncTissueLoop
			waitcnt cNextTissueCycle, cTissueCycles
'6+ cycles.
ReadyActiveShift ' MSB-align the bits, check buttons
			rdbyte  cmActiveIn, caNextActiveIn

			' Only step if we have at least one
			' motor active.
			cmp		cmActiveIn, #kmNoActiveMotors	wz
	if_z	jmp		#SyncTissueLoop
' 16 cycles. 22+ total.
CheckButtons ' Stop movement if limit switch is pressed.
			rdbyte  cButtonState, caButtonState			' Hub sync
			' nop
			' nop
			rdlong  csNextDirIn, caNextDirIn

			test	csNextDirIn, cmHorizDir				wc 
	if_c	test	cButtonState, #kmRightSwitch		wz
	if_nc	test	cButtonState, #kmLeftSwitch			wz
	if_nz   or		cmActiveIn, #kmHorizontalMotor

			test	csNextDirIn, cmLeftVertDir			wc 
	if_nc	test	cButtonState, #kmTopLeftSwitch		wz
	if_c	test	cButtonState, #kmBottomSwitches		wz
	if_nz   or		cmActiveIn, #kmLeftVerticalMotor

			test	csNextDirIn, cmRightVertDir			wc 
	if_nc	test	cButtonState, #kmTopRightSwitch		wz
	if_c	test	cButtonState, #kmBottomSwitches		wz
	if_nz   or		cmActiveIn, #kmRightVerticalMotor
' 72 cycles. 94+ total.
ShiftActive	' Shift out the step data.
			' MSB-align. Original bit 31 shifts into C,
			' so this needs to be distinct from the 
			' bit 7 shift. Note we can always shift out
			' bit 7 since it's unaffected by buttons.
			shl		cmActiveIn, #24

			' Bit 7
			shl		cmActiveIn, #1			wc
			muxc	outa, cmActiveSer
			or		outa, cmActiveSck
			andn	outa, cmActiveSck

			' Bit 6
			shl		cmActiveIn, #1			wc
			muxc	outa, cmActiveSer
			or		outa, cmActiveSck
			andn	outa, cmActiveSck

			' Bit 5
			shl		cmActiveIn, #1			wc
			muxc	outa, cmActiveSer
			or		outa, cmActiveSck
			andn	outa, cmActiveSck

			' Bit 4
			shl		cmActiveIn, #1			wc
			muxc	outa, cmActiveSer
			or		outa, cmActiveSck
			andn	outa, cmActiveSck

			' Bit 3
			shl		cmActiveIn, #1			wc
			muxc	outa, cmActiveSer
			or		outa, cmActiveSck
			andn	outa, cmActiveSck

			' Bit 2
			shl		cmActiveIn, #1			wc
			muxc	outa, cmActiveSer
			or		outa, cmActiveSck
			andn	outa, cmActiveSck

			' Bit 1
			shl		cmActiveIn, #1			wc
			muxc	outa, cmActiveSer
			or		outa, cmActiveSck
			andn	outa, cmActiveSck

			' Bit 0
			shl		cmActiveIn, #1			wc
			muxc	outa, cmActiveSer
			or		outa, cmActiveSck
			andn	outa, cmActiveSck

			' Latch the active motors.
			or		outa, cmActiveRck
			andn	outa, cmActiveRck
'140 cycles. 234 total.
ReadyIdleTissue ' Zero, but don't latch, the register.
			or		outa, cmActiveSer	' All high.

			' Bit 7
			or		outa, cmActiveSck
			andn	outa, cmActiveSck

			' Bit 6
			or		outa, cmActiveSck
			andn	outa, cmActiveSck

			' Bit 5
			or		outa, cmActiveSck
			andn	outa, cmActiveSck

			' Bit 4
			or		outa, cmActiveSck
			andn	outa, cmActiveSck

			' Bit 3
			or		outa, cmActiveSck
			andn	outa, cmActiveSck

			' Bit 2
			or		outa, cmActiveSck
			andn	outa, cmActiveSck

			' Bit 1
			or		outa, cmActiveSck
			andn	outa, cmActiveSck

			' Bit 0
			or		outa, cmActiveSck
			andn	outa, cmActiveSck

'68 cycles. 302 total. => 72 cycles. 306 total.
DetermineTissueSync ' Satellite waits for base's pin. Note Base jumps.
			waitpeq cmActiveSync, cmActiveSync
			jmp		#LatchIdleState
'Base/satellite
'4/10+ cycles. 306/312+ total
BaseTissueSync ' Wait, then signal
			mov		cTissueSyncWindow, cTissueSyncCycles
			add		cTissueSyncWindow, cnt
			waitcnt cTissueSyncWindow, #0
			or		outa, cmActiveSync
			nop		' To match the satellite's jmp.
'22+/0 cycles. 328/312+ total
LatchIdleState ' Shift out all high bits.
			or		outa, cmActiveRck
			andn	outa, cmActiveRck

			' Base needs to disable sync pin.
	if_z	andn	outa, cmActiveSync
			jmp		#SyncTissueLoop
'16 cycles. 344/328+ total. 
' 156 cycles free, so our sync window needs to be 
' less than 150 to sync.

DAT ' Motor tissue initialized variables ------------------
caTissueBoardId	long @@@hBoardId

' Start after axon is completed and midway through dendrite.
hcTissueStartCycle long kCyclesPerTick + kTissueStartCycle
cTissueCycles	   long kCyclesPerTick
cTissueSyncCycles  long kTissueSyncWindowCycles

cmActiveDira	long kmActiveDira
cmActiveSer		long kmActiveSer
cmActiveSck		long kmActiveSck
cmActiveRck		long kmActiveRck
cmEnableActive	long kmActiveClr
cmActiveSync	long kmActiveSync

caNextActiveIn	long @@@hsNextActive
caButtonState	long @@@hButtonState
caNextDirIn		long @@@hsNextMotion

cmHorizDir		long kmHorizDir
cmLeftVertDir	long kmLeftVertDir
cmRightVertDir	long kmRightVertDir

cBaseJump		jmp #BaseTissueSync
cSatJump		jmp	#ShiftActive

DAT ' Motor tissue uninitialized variables ----------------
cTissueBoardId		res 1

cActiveBitsLeft		res 1
cmActiveIn			res 1
cButtonState		res 1

cNextTissueCycle	res 1
cTissueSyncWindow   res 1
csNextDirIn			res 1
	   		fit		$1F0

{{ Serial Terminal Cog ====================================
	Updates the RX and TX buffers for the serial port.
	This code is heavily based on Parallax Serial Terminal
	(by Jeff Martin, Andy Lindsay, and Chip Gracey), which
	is based on FullDuplexSerialPlus (by Andy Lindsay),
	which in turn is heavily based on FullDuplexSerial (by
	Chip Gracey).

	Possible improvements involve splitting this into two
	cogs, one for RX and the other for TX; doing so should
	double our possible rx/tx rates.

	PAR - Points to hSerialRxFront in Hub memory.
}}
CON ' Serial terminal  constants --------------------------
' These should be powers of two.
kSerialRxBufferSize = 8192 
kSerialTxBufferSize = 256
kmSerialRxBuffer = kSerialRxBufferSize - 1
kmSerialTxBuffer = kSerialTxBufferSize - 1

kSerialBaudRate = 115_200
kSerialBitTicks = _CLKFREQ / kSerialBaudRate

DAT ' Serial terminal hub variables -----------------------
hSerialRxFront	word 0
hSerialRxBack	word 0
hSerialTxFront	word 0
hSerialTxBack	word 0
hSerialRxBuffer	byte 0[kSerialRxBufferSize]
hSerialTxBuffer byte 0[kSerialTxBufferSize]

hLastRxCount	byte 0
hNewLine		byte 13, 10, 0

CON ' Serial terminal spin code ---------------------------
PRI ReadChar : readByte | count
' Blocks until returning an 8-bit character.
' Based on Radiant Serial Terminal.spin.
	repeat
		readByte~~
		if hSerialRxBack <> hSerialRxFront
			count := RxCount
			if count < hLastRxCount
				SendStr(string("Error: Rx overflow of "))
				SendDec(count)
				SendStrLn(string(" bytes."))
			hLastRxCount := count - 1
		
			readByte := hSerialRxBuffer[hSerialRxBack]
			hSerialRxBack := (hSerialRxBack + 1) & kmSerialRxBuffer
		else
			' A byte's not ready to read…
			' Wait for 8 bits (~69.4µsec) to transmit
			waitcnt(constant(kSerialBitTicks * 8) + cnt)	
	while readByte < 0

PRI UnblockedReadChar : readByte | count
' Reads a byte and returns immediately. Note that since
' hThisToken is a byte, we can't return negative numbers.
' As such, return 0 (kCmdError) when we don't have
' anything to read...
	readByte := 0
	if hSerialRxBack <> hSerialRxFront
		count := RxCount
		if count < hLastRxCount
			SendStr(string("Error: Rx overflow of "))
			SendDec(count)
			SendStrLn(string(" bytes."))
		hLastRxCount := count - 1

		readByte := hSerialRxBuffer[hSerialRxBack]
		hSerialRxBack := (hSerialRxBack + 1) & kmSerialRxBuffer

PRI RxCount : count
' Returns the number of bytes in the Rx buffer.
	count := hSerialRxFront - hSerialRxBack
	count -= kSerialRxBufferSize * (count < 0)

PRI ReadString(haString) : readByte | aBase
' Fills haString with up to kReplInputSize bytes; stops
' and adds a 0 upon encountering unprintable characters.
' Based on Radiant Serial Terminal.spin
	aBase := haString

	repeat constant(kReplInputSize - 1)	' Reserve space for 0
		readByte := ReadChar

		if readByte == 8 and haString > aBase
			byte[--haString] := 0
		' Only continue reading if it's a printable character.
		elseif (byte[haString++] := readByte) =< kDelimiter
			quit
	' Zero-terminate the string.
	byte[haString + (byte[haString - 1] =< kDelimiter)]~

PRI SendChar(byteChar)
' Writes byteChar to TX buffer; blocks until space
' is available.
' Based on Radiant Serial Terminal.spin.
	repeat until (hSerialTxBack <> ((hSerialTxFront + 1) & kmSerialTxBuffer))
	hSerialTxBuffer[hSerialTxFront] := byteChar
	hSerialTxFront := (hSerialTxFront + 1) & kmSerialTxBuffer

PRI SendDec(value) | i, x
' Sends value as an ASCII-string decimal.
' Based on Radiant Serial Terminal.spin.
	x := value == NEGX		' Check for max negative
	if value < 0
		' If negative, make positive; adjust for max negative
		value := ||(value + x)
		SendChar("-")	   ' Output sign.

	i := 1_000_000_000	  ' Initialize divisor

	repeat 10			   ' Loop for 10 digits
		if value => i
			' If non-zero digit, output digit; adjust for max negative
			SendChar(value / i + "0" + x * (i == 1))
			value //= i	 ' and digit from value.
			result~~		' flag non-zero found
		elseif result or i == 1
			SendChar("0")   ' If zero digit (or only digit) output it
		i /= 10		 		' Update divisor.

PRI SendDecLn(value)
' Sends value as an ASCII-string decimal and newline.
	SendDec(value)
	SendStr(@hNewLine)

PRI SendDecResponse(value)
' Sends the token and delimiter in data mode;
' just the value otherwise.
	SendDec(value)

PRI SendDecResponseLn(value)
' Sends the token and delimiter in data mode;
' just the value otherwise.
	SendDecResponse(value)
	SendStr(@hNewLine)

PRI SendHex(value, digits)
' Send value as a hexidecimal string up to digits in length.
' Based on Radiant Serial Terminal.spin.
	value <<= (8 - digits) << 2
	repeat digits
		SendChar(lookupz((value <-= 4) & $F : "0".."9", "A".."F"))

PRI SendHexLn(value, digits)
' Sends value as a hexidecimal string up to digits in
' length followed by a newline.
	SendHex(value, digits)
	SendStr(@hNewLine)

PRI SendResponse(haString)
' Sends the token and delimiter in data mode; just the
' string otherwise.
	SendStr(haString)

PRI SendResponseLn(haString)
' Sends the response and a newline.
	SendResponse(haString)
	SendStr(@hNewLine)

PRI SendStr(haString)
' Sends a zero-terminated string.
' Based on Radiant Serial Terminal.spin.
	repeat strsize(haString)
		SendChar(byte[haString++])

PRI SendStrLn(haString)
' Sends a zero-terminated string and newline.
	SendStr(haString)
	SendStr(@hNewLine)

PRI SendTemperature(value)
' Sends the temperature or a warning value if
' detected.
	if value => kMaxTemperature
		SendStrLn(string("defective?"))
	else
		SendDecLn(value)

DAT ' Serial terminal code --------------------------------
			org   0
InitializeSerialTerminalCog ' Ready pins for rx & tx.
			mov   outa, cmSerialTx		' Init tx pin for no conversions.
			mov   dira, cmSerialTx
			mov   txcode, #SerialTransmit ' Init ping-pong multitasking.

SerialReceive ' Doesn't support rx inversions.
			jmpret  rxcode, txcode		' Run chunk of tx code, then return.

			' Wait for start bit on rx pin.
			test	cmSerialRx, ina wc
	if_c	jmp	 #SerialReceive

			' Ready to receive a byte.
			mov	 rxbits, #9
			mov	 rxcnt, cBitTicks
			shr	 rxcnt, #1
			add	 rxcnt, cnt

:bit		add	 rxcnt, cBitTicks		' Ready next bit period.

:wait		jmpret  rxcode, txcode		' Run chunk of tx code, then return.

			' Check if bit receive period done.
			mov	 serTemp1, rxcnt
			sub	 serTemp1, cnt
			cmps serTemp1, #0 wc
	if_nc	jmp	 #:wait

			' Receive bit on rx pin
			test cmSerialRx, ina wc
			rcr	 rxdata, #1
			djnz rxbits, #:bit

			shr	 rxdata, #32-9		' Justify and trim received byte.

			' Save the received byte and increment head.
			rdword  serTemp2, par
			and	 rxdata, #$FF
			add	 serTemp2, caRxBuffer
			wrbyte  rxdata, serTemp2
			sub	 serTemp2, caRxBuffer
			add	 serTemp2, #1
			and	 serTemp2, cRxBufferMask
			wrword  serTemp2, par

			jmp	 #SerialReceive		' Byte done, receive next byte.

SerialTransmit ' Doesn't support inversions, drain/source.
			jmpret  txcode, rxcode		' Run chunk of rx code, then return
			' Check for head <> tail
			rdword  serTemp2, caTxFront
			rdword  serTemp3, caTxBack
			cmp	 serTemp2, serTemp3 wz
	if_z	jmp	 #SerialTransmit

			' Get a byte and increment tail.
			add	 serTemp3, caTxBuffer
			rdbyte  txdata,serTemp3
			sub	 serTemp3, caTxBuffer
			add	 serTemp3, #1
			and	 serTemp3, cTxBufferMask
			wrword  serTemp3, caTxBack

			' Ready byte to transmit.
			or	  txdata, #$100
			shl	 txdata, #2
			or	  txdata, #1
			mov	 txbits, #11
			mov	 txcnt, cnt

:bit		shr	 txdata, #1 wc
			muxc	outa, cmSerialTx
			add	 txcnt, cBitTicks	' Ready next cnt.

:wait		jmpret  txcode,rxcode	 ' Run chunk of rx code, then return.

			' Check if bit transmit period done.
			mov	 serTemp1, txcnt
			sub	 serTemp1, cnt
			cmps	serTemp1, #0 wc
	if_nc	jmp	 #:wait

			djnz	txbits, #:bit	 ' Another bit to transmit?

			jmp	 #SerialTransmit   ' Byte done, transmit next byte.

DAT ' Serial terminal initialized variables ---------------
cRxBufferMask	long kmSerialRxBuffer
cTxBufferMask	long kmSerialTxBuffer

cBitTicks   long kSerialBitTicks
cmSerialRx  long 1 << kpSerialRx
cmSerialTx  long 1 << kpSerialTx

caRxBuffer  long @@@hSerialRxBuffer
caTxFront   long @@@hSerialTxFront
caTxBack	long @@@hSerialTxBack
caTxBuffer  long @@@hSerialTxBuffer

DAT ' Serial terminal uninitialized variables -------------
serTemp1	res 1 ' Working variable.
serTemp2	res 1 ' Working variable.
serTemp3	res 1 ' Working variable.

rxdata		res 1 ' The byte to receive.
rxbits		res 1 ' Bits to receive index.
rxcnt		res 1 ' Target wait count.
rxcode		res 1 ' Ping-pong multitasking address.

txdata		res 1 ' The byte to transmit.
txbits		res 1 ' Bits to transmit index.
txcnt		res 1 ' Target wait count.
txcode		res 1 ' Ping-pong multitasking address.
			fit		$1F0

{{ Switch Cog =============================================
	Shifts 8 bits in from the button/address shift register.

	If both the left & right switches are triggered, the
	remaining bits identify the board number. 

	Data Sheets -------------------------------------------
		Register: <http://bit.ly/1gFaTP3>
				  <http://bit.ly/18iJWi3>
}}
CON ' Switch constants ------------------------------------
	{! GanglionSwitch }
	#0
	kLeftSwitch
	kRightSwitch
	kTopLeftSwitch
	kBottomLeftSwitch
	kTopRightSwitch
	kBottomRightSwitch
	{! end }

	kmLeftSwitch		= 1 << kLeftSwitch
	kmRightSwitch		= 1 << kRightSwitch
	kmTopLeftSwitch		= 1 << kTopLeftSwitch
	kmBottomLeftSwitch	= 1 << kBottomLeftSwitch
	kmTopRightSwitch	= 1 << kTopRightSwitch
	kmBottomRightSwitch	= 1 << kBottomRightSwitch

	kmTopSwitches	= kmTopLeftSwitch | kmTopRightSwitch
	kmBottomSwitches= kmBottomLeftSwitch | kmBottomRightSwitch

	kmSwitchSer		= 1 << kpSwitchSer
	kmSwitchCk		= 1 << kpSwitchCk
	kmSwitchLoad	= 1 << kpSwitchLoad

	kmSwitchDira	= kmSwitchCk | kmSwitchLoad

	kInputBits = 8

DAT ' Switch hub variables --------------------------------
hButtonState	long 0

DAT ' Switch code -----------------------------------------
			org		0
InitializeSwitchCog	' Ready pins.
			mov		outa, cmSwitchDira
			mov		dira, cmSwitchDira

			mov		cSwitchSync, cSwitchDelay
			add		cSwitchSync, cnt
SyncSwitches ' Only need to run once per tick.
			waitcnt	cSwitchSync, cSwitchDelay
			wrbyte	cSwitchInput, caSwitchState
			andn	outa, cmSwitchLoad	' Prepare for reset.
			or		outa, cmSwitchLoad	' Enable reading.

			mov		cSwitchBitsLeft, #kInputBits
' 14+ cycles
ReadSwitches ' Main loop for reading bits.
			test	cmSwitchSer, ina			wc
			rcl		cSwitchInput, #1
			andn	outa, cmSwitchCk
			or		outa, cmSwitchCk
			djnz	cSwitchBitsLeft, #ReadSwitches
' 44 cycles
WriteInput ' Record the input.
			jmp		#SyncSwitches
' 12+ cycles.

DAT ' Switch initialized variables ------------------------
cmSwitchDira	long kmSwitchDira
cmSwitchSer		long kmSwitchSer
cmSwitchCk		long kmSwitchCk
cmSwitchLoad	long kmSwitchLoad

caSwitchState	long @@@hButtonState

' Only need to run once per tick.
cSwitchDelay	long kCyclesPerTick

DAT ' Switch uninitialized variables ----------------------
cSwitchSync		res 1
cSwitchBitsLeft	res 1
cSwitchInput	res 1
			fit		$1F0

DAT ' Dictionary ------------------------------------------
hDictionaryStart
	' error - The only command that should be out of place
	long      $07615598
	byte      kCmdError
	byte      0
	word      @@@hDictError

	' attached?
	long      $e21aabd5
	byte      kCmdAttachedP
	byte      0
	word      @@@hDictAttachedP

	' buffer-free?
	long      $65ba0326               ' Hash value
	byte      kCmdBufferFreeP         ' Command token
	byte      0                       ' Arg for constants
	word      @@@hDictBufferFree       ' Text for decompilation

	' button
	long	$e8601636
	byte	kCmdButton
	byte	0
	word	@@@hDictButton

	' calibrate
	long      $017109a7
	byte      kCmdCalibrate
	byte      0
	word      @@@hDictCalibrate

	' clockwise
	long      $130a56c0
	byte      kCmdClockwise
	byte      0
	word      @@@hDictClockwise

	' center
	long      $e4b044cb
	byte      kCmdCenter
	byte      0
	word      @@@hDictCenter

	' counter-clockwise
	long      $baa941f9
	byte      kCmdCounterClockwise
	byte      0
	word      @@@hDictCounterClockwise

	' credits
	long      $98cacfbe
	byte      kCmdCredits
	byte      0
	word      @@@hDictCredits

	' cycle
	long      $06f8f110
	byte      kCmdCycle
	byte      0
	word      @@@hDictCycle

	' d
	long	$00000064
	byte	kCmdD
	byte	0
	word	@@@hDictD

	' d?
	long	$00000cdb
	byte	kCmdDP
	byte	0
	word	@@@hDictDP

	' data
	long      $00354710
	byte      kCmdData
	byte      0
	word      @@@hDictData

	' direction?
	long      $24ce911a
	byte      kCmdDirectionP
	byte      0
	word      @@@hDictDirectionP

	' extruder0
	long      $94c4f6ad
	byte      kCmdExtruder0
	byte      0
	word      @@@hDictExtruder0

	' extruder1
	long      $94c4f6ac
	byte      kCmdExtruder1
	byte      0
	word      @@@hDictExtruder1

	' extruder2
	long      $94c4f6af
	byte      kCmdExtruder2
	byte      0
	word      @@@hDictExtruder2

	' extruder3
	long      $94c4f6ae
	byte      kCmdExtruder3
	byte      0
	word      @@@hDictExtruder3

	' extruder4
	long      $94c4f6a9
	byte      kCmdExtruder4
	byte      0
	word      @@@hDictExtruder4

	' extruder5
	long      $94c4f6a8
	byte      kCmdExtruder5
	byte      0
	word      @@@hDictExtruder5

	' extruder6
	long      $94c4f6ab
	byte      kCmdExtruder6
	byte      0
	word      @@@hDictExtruder6

	' extruder7
	long      $94c4f6aa
	byte      kCmdExtruder7
	byte      0
	word      @@@hDictExtruder7

	' g
	long      $00000067
	byte      kCmdG
	byte      0
	word      @@@hDictG

	' handshake
	long      $efe51db7
	byte      kCmdHandshake
	byte      0
	word      @@@hDictHandshake

	' heat
	long      $00378838
	byte      kCmdHeat
	byte      0
	word      @@@hDictHeat

	' heater
	long	$ec3a7a8f
	byte	kCmdHeater
	byte	0
	word	@@@hDictHeater

	' heater?
	long	$7389cc50
	byte	kCmdHeaterP
	byte	0
	word	@@@hDictHeaterP

	' heat-test
	long      $78592923
	byte      kCmdHeatTest
	byte      0
	word      @@@hDictHeatTest

	' horiz-track
	long      $dbc2dea4
	byte      kCmdHorizTrack
	byte      0
	word      @@@hDictHorizTrack

	' i
	long	$00000069
	byte	kCmdI
	byte	0
	word	@@@hDictI

	' i?
	long	$00000db6
	byte	kCmdIP
	byte	0
	word	@@@hDictIP

	' left-track
	long      $9af669d9
	byte      kCmdLeftTrack
	byte      0
	word      @@@hDictLeftTrack

	' level
	long      $076cf676
	byte      kCmdLevel
	byte      0
	word      @@@hDictLevel

	' motor
	long      $07e3f56b
	byte      kCmdMotor
	byte      0
	word      @@@hDictMotor

	' motor?
	long      $0462a2f4
	byte      kCmdMotorP
	byte      0
	word      @@@hDictMotorP

	' off
	long      $0001d32f
	byte      kCmdOff
	byte      0
	word      @@@hDictOff

	' on
	long      $00000e21
	byte      kCmdOn
	byte      0
	word      @@@hDictOn

	' p
	long	$00000070
	byte	kCmdP
	byte	0
	word	@@@hDictP

	' p?
	long	$00000e4f
	byte	kCmdPP
	byte	0
	word	@@@hDictPP

	' ping
	long      $003bfc50
	byte      kCmdPing
	byte      0
	word      @@@hDictPing

	' platform
	long      $028c0bff
	byte      kCmdPlatform
	byte      0
	word      @@@hDictPlatform

	' pressed?
	long      $11c2b6d9
	byte      kCmdPressedP
	byte      0
	word      @@@hDictPressedP

	' reboot
	long      $0c988da1
	byte      kCmdReboot
	byte      0
	word      @@@hDictReboot

	' repeat
	long      $0c92e7b7
	byte      kCmdRepeat
	byte      0
	word      @@@hDictRepeat

	' right-track
	long      $2f3cf542
	byte      kCmdRightTrack
	byte      0
	word      @@@hDictRightTrack

	' seek
	long      $003e8718
	byte      kCmdSeek
	byte      0
	word      @@@hDictSeek

	' seek-test
	long      $f9568443
	byte      kCmdSeekTest
	byte      0
	word      @@@hDictSeekTest

	' sleep
	long	$0817924f
	byte	kCmdSleep
	byte	0
	word	@@@hDictSleep

	' stack?
	long      $09599af1
	byte      kCmdStackP
	byte      0
	word      @@@hDictStackP

	' step
	long      $003e6052
	byte      kCmdStep
	byte      0
	word      @@@hDictStep

	' step-rate
	long      $88dc017d
	byte      kCmdStepRate
	byte      0
	word      @@@hDictStepRate

	' step-rate?
	long	$a45c3122
	byte	kCmdStepRateP
	byte	0
	word	@@@hDictStepRateP

	' step-rate-step
	long	$ec7c9562
	byte	kCmdStepRateStep
	byte	0
	word	@@@hDictStepRateStep

	' step-size
	long      $88d83cda
	byte      kCmdStepSize
	byte      0
	word      @@@hDictStepSize

	' step-size?
	long      $a3dfd825
	byte      kCmdStepSizeP
	byte      0
	word      @@@hDictStepSizeP

	' stop
	long      $003e6098
	byte      kCmdStop
	byte      0
	word      @@@hDictStop

	' temperature?
	long      $db8403f3
	byte      kCmdTemperatureP
	byte      0
	word      @@@hDictTemperatureP

	' temperatures?
	long      $07adbd80
	byte      kCmdTemperaturesP
	byte      0
	word      @@@hDictTemperaturesP

	' temperature-target?
	long      $618110ef
	byte      kCmdTemperatureTargetP
	byte      0
	word      @@@hDictTemperatureTargetP
	
	' ticks?
	long      $0883aa19
	byte      kCmdTickP
	byte      0
	word      @@@hDictTicksP

	' tick-target?
	long      $fedab6d6
	byte      kCmdTickTargetP
	byte      0
	word      @@@hDictTickTargetP
	' text
	long      $003df99d
	byte      kCmdText
	byte      0
	word      @@@hDictText

	' token?
	long      $0812f264
	byte      kCmdTokenP
	byte      0
	word      @@@hDictTokenP

	' value1
	long      $15a9fd7a
	byte      kCmdValue1
	byte      0
	word      @@@hDictValue1

	' value2
	long      $15a9fd79
	byte      kCmdValue2
	byte      0
	word      @@@hDictValue2

	' value3
	long      $15a9fd78
	byte      kCmdValue3
	byte      0
	word      @@@hDictValue3

	' value4
	long      $15a9fd7f
	byte      kCmdValue4
	byte      0
	word      @@@hDictValue4

	' version?
	long      $b9b87ce5
	byte      kCmdVersionP
	byte      0
	word      @@@hDictVersionP

	' wait-heated
	long      $a92a1fdf
	byte      kCmdWaitHeated
	byte      0
	word      @@@hDictWaitHeated

	' wake
	long	$0040a958
	byte	kCmdWake
	byte	0
	word	@@@hDictWake

	' words
	long      $13707942
	byte      kCmdWordsP
	byte      0
	word      @@@hDictWordsP

	' Step sizes may need to become commands
	' similar to the named motors.
	' 1/1
	long      $0000d60f
	byte      kCmdNumber
	byte      kStepperWhole
	word      @@@hDictWhole

	' 1/2
	long      $0000d60c
	byte      kCmdNumber
	byte      kStepperHalf
	word      @@@hDictHalf

	' 1/4
	long      $0000d60a
	byte      kCmdNumber
	byte      kStepperQuarter
	word      @@@hDictQuarter

	' 1/8
	long      $0000d606
	byte      kCmdNumber
	byte      kStepperEighth
	word      @@@hDictEighth

	' 1/16
	long      $001b97d9
	byte      kCmdNumber
	byte      kStepperSixteenth
	word      @@@hDictSixteenth

	' left
	long      $003998db
	byte      kCmdNumber
	byte      kLeft
	word      @@@hDictLeft

	' right
	long      $0825ec40
	byte      kCmdNumber
	byte      kRight
	word      @@@hDictRight

	' top
	long      $0001e18b
	byte      kCmdNumber
	byte      kTop
	word      @@@hDictTop

	' bottom
	long      $e775e2af
	byte      kCmdNumber
	byte      kBottom
	word      @@@hDictBottom

	' power-light
	long	$98654b2c
	byte	kCmdNumber
	byte	kmPowerLight
	word	@@@hDictPowerLight

	' scanner-back-light
	long	$486fe5f3
	byte	kCmdNumber
	byte	kmScannerBackLight
	word	@@@hDictScannerBackLight

	' scanner-mid-light
	long	$263f3178
	byte	kCmdNumber
	byte	kmScannerMidLight
	word	@@@hDictScannerMidLight

	' scanner-front-light
	long	$d243c599
	byte	kCmdNumber
	byte	kmScannerFrontLight
	word	@@@hDictScannerFrontLight

	' printhead-light
	long	$67e6a20a
	byte	kCmdNumber
	byte	kmPrintheadLight
	word	@@@hDictPrintheadLight

	' End of dictionary
	long      0[2]

	' REPL commands for decompiling.
	hDictError		byte "error", 0
	hDictAttachedP	byte "attached?", 0
	'' backlight => /bit pattern for backlight/
	hDictBottom		byte "bottom", 0
	hDictBufferFree	byte "buffer-free?", 0
	hDictButton		byte "button", 0
	hDictCalibrate	byte "calibrate", 0
	hDictCenter		byte "center", 0
	hDictClockwise	byte "clockwise", 0
	hDictCounterClockwise byte "counter-clockwise", 0
	hDictCredits	byte "credits", 0
        hDictCycle      byte "cycle", 0
	hDictD			byte "d", 0
	hDictDP			byte "d?", 0
	hDictData		byte "data", 0
	hDictDirectionP	byte "direction?", 0
	'' extruder0 => /selects the first extruder motor/
	hDictExtruder0	byte "extruder0", 0
	'' extruder1 => /selects the second extruder motor/
	hDictExtruder1	byte "extruder1", 0
	'' extruder2 => /selects the third extruder motor/
	hDictExtruder2	byte "extruder2", 0
	'' extruder3 => /selects the fourth extruder motor/
	hDictExtruder3	byte "extruder3", 0
	'' extruder4 => /selects the fifth extruder motor/
	hDictExtruder4	byte "extruder4", 0
	'' extruder5 => /selects the sixth extruder motor/
	hDictExtruder5	byte "extruder5", 0
	'' extruder6 => /selects the seventh extruder motor/
	hDictExtruder6	byte "extruder6", 0
	'' extruder7 => /selects the eighth extruder motor/
	hDictExtruder7	byte "extruder7", 0
	hDictG			byte "g", 0
	hDictTicksP		byte "ticks?", 0
	hDictTickTargetP	byte "tick-target?", 0
	hDictHandshake	byte "handshake", 0
	hDictHeat		byte "heat", 0
	hDictHeater		byte "heater", 0
	hDictHeaterP	byte "heater?", 0
	hDictHeatTest	byte "heat-test", 0
	'' horiz-track => /selects the horizontal track motor/
	hDictHorizTrack	byte "horiz-track", 0
	hDictI			byte "i", 0
	hDictIP			byte "i?", 0
	hDictLeft		byte "left", 0
	'' left-track => /selects the left-vertical track motor/
	hDictLeftTrack	byte "left-track", 0
	hDictLevel		byte "level", 0
	hDictMotor		byte "motor", 0
	hDictMotorP		byte "motor?", 0
	hDictOff		byte "off", 0
	hDictOn			byte "on", 0
	hDictP			byte "p", 0
	hDictPP			byte "p?", 0
	hDictPing		byte "ping", 0
	'' platform => /selects the platform motor/
	hDictPlatform	byte "platform", 0
	'' power-light => /mask for FETs on stack/
	hDictPowerLight byte "power-light", 0
	hDictPressedP	byte "pressed?", 0
	'' printhead-light => /mask for FETs on stack/
	hDictPrintheadLight byte "printhead-light", 0
	hDictReboot		byte "reboot", 0
	hDictRepeat		byte "repeat", 0
	hDictRight		byte "right", 0
	'' right-track => /selects the right-vertical track motor/
	hDictRightTrack	byte "right-track", 0
	'' scanner-back-light => /mask for FET on stack/
	hDictScannerBackLight  byte "scanner-back-light", 0
	'' scanner-mid-light => /mask for FET on stack/
	hDictScannerMidLight   byte "scanner-mid-light", 0
	'' scanner-front-light => /mask for FET on stack/
	hDictScannerFrontLight byte "scanner-front-light", 0
	hDictSeek		byte "seek", 0
	hDictSeekTest	byte "seek-test", 0
	hDictSleep		byte "sleep", 0
	hDictStackP		byte "stack?", 0
	hDictStep		byte "step", 0
	hDictSteps		byte "step(s)", 0
	hDictStepRate	byte "step-rate", 0
	hDictStepRateP	byte "step-rate?", 0
        hDictStepRateStep       byte  "step-rate-step", 0
	hDictStepSize	byte "step-size", 0
	hDictStepSizeP	byte "step-size?", 0
        hDictStop               byte "stop", 0
	hDictTemperatureP	byte  "temperature?", 0
	hDictTemperaturesP	byte  "temps?", 0
	hDictTemperatureTargetP byte "temperature-target?", 0
	hDictText		byte "text", 0
	hDictTokenP		byte "token?", 0
	hDictTop		byte "top", 0
	hDictValue1		byte "value1", 0
	hDictValue2		byte "value2", 0
	hDictValue3		byte "value3", 0
	hDictValue4		byte "value4", 0
	hDictVersionP	byte "version?", 0
	hDictWaitHeated	byte "wait-heated", 0
	hDictWake		byte "wake", 0
	hDictWordsP		byte "words?", 0
	'' 1/1 => /whole step pattern/
	hDictWhole		byte "1/1", 0
	'' 1/2 => /half step pattern/
	hDictHalf		byte "1/2", 0
	'' 1/4 => /quarter step pattern/
	hDictQuarter	byte "1/4", 0
	'' 1/8 => /eighth step pattern/
	hDictEighth		byte "1/8", 0
	'' 1/16 => /sixteenth step pattern/
	hDictSixteenth	byte "1/16", 0

	' Used for queries.
	hDictTrue		byte "T", 0
	hDictFalse		byte "F", 0

	hCredits
	byte      13, 10, "Programming: Kevin Harris", 13, 10
	byte      "Testing: Nathan Patterson, Nathan Schumacher", 13, 10
	byte      "Special Thanks: Daniel Harris", 13, 10
	byte      "Some portions based on Parallax Serial Terminal", 13, 10
	byte      "by Jeff Martin, Andy Lindsay, and Chip Gracey.", 13, 10, 13, 10
	byte      "Ganglion (c) 2013 Radiant Fabrication. All Rights Reserved.", 0

{ Definitions for non-REPL commands for the host logging/etc.
  These should be ignored by Ganglion.

	byte	kCmdStepRatePlatform

	word	@@@hDictStepRatePlatform
	hDictStepRatePlatform	byte "step-rate-platform"
	byte	kCmdStepRateHorizTrack

	word	@@@hDictStepRateHorizTrack
	hDictStepRateHorizTrack	byte "step-rate-horiz-track"
	byte	kCmdStepRateLeftTrack

	word	@@@hDictStepRateLeftTrack
	hDictStepRateLeftTrack	byte "step-rate-left-track"
	byte	kCmdStepRateRightTrack

	word	@@@hDictStepRateRightTrack
	hDictStepRateRightTrack	byte "step-rate-right-track"
	byte	kCmdStepRateExtruder0

	word	@@@hDictStepRateExtruder0
	hDictStepRateExtruder0	byte "step-rate-extruder0"
	byte	kCmdStepRateExtruder1

	word	@@@hDictStepRateExtruder1
	hDictStepRateExtruder1	byte "step-rate-extruder1"
	byte	kCmdStepRateExtruder2

	word	@@@hDictStepRateExtruder2
	hDictStepRateExtruder2	byte "step-rate-extruder2"
	byte	kCmdStepRateExtruder3

	word	@@@hDictStepRateExtruder3
	hDictStepRateExtruder3	byte "step-rate-extruder3"
	byte	kCmdStepRateExtruder4

	word	@@@hDictStepRateExtruder4
	hDictStepRateExtruder4	byte "step-rate-extruder4"
	byte	kCmdStepRateExtruder5

	word	@@@hDictStepRateExtruder5
	hDictStepRateExtruder5	byte "step-rate-extruder5"
	byte	kCmdStepRateExtruder6

	word	@@@hDictStepRateExtruder6
	hDictStepRateExtruder6	byte "step-rate-extruder6"
	byte	kCmdStepRateExtruder7

	word	@@@hDictStepRateExtruder7
	hDictStepRateExtruder7	byte "step-rate-extruder7"
	byte	kCmdPlatformStepRateStep

	word	@@@hDictPlatformStepRateStep
	hDictPlatformStepRateStep	byte "platform-step-rate-step"
	byte	kCmdHorizTrackStepRateStep

	word	@@@hDictHorizTrackStepRateStep
	hDictHorizTrackStepRateStep	byte "horiz-track-step-rate-step"
	byte	kCmdLeftTrackStepRateStep

	word	@@@hDictLeftTrackStepRateStep
	hDictLeftTrackStepRateStep	byte "left-track-step-rate-step"
	byte	kCmdRightTrackStepRateStep

	word	@@@hDictRightTrackStepRateStep
	hDictRightTrackStepRateStep	byte "right-track-step-rate-step"
	byte	kCmdExtruder0StepRateStep

	word	@@@hDictExtruder0StepRateStep
	hDictExtruder0StepRateStep	byte "extruder-0-step-rate-step"
	byte	kCmdExtruder1StepRateStep

	word	@@@hDictExtruder1StepRateStep
	hDictExtruder1StepRateStep	byte "extruder-1-step-rate-step"
	byte	kCmdExtruder2StepRateStep

	word	@@@hDictExtruder2StepRateStep
	hDictExtruder2StepRateStep	byte "extruder-2-step-rate-step"
	byte	kCmdExtruder3StepRateStep

	word	@@@hDictExtruder3StepRateStep
	hDictExtruder3StepRateStep	byte "extruder-3-step-rate-step"
	byte	kCmdExtruder4StepRateStep

	word	@@@hDictExtruder4StepRateStep
	hDictExtruder4StepRateStep	byte "extruder-4-step-rate-step"
	byte	kCmdExtruder5StepRateStep

	word	@@@hDictExtruder5StepRateStep
	hDictExtruder5StepRateStep	byte "extruder-5-step-rate-step"
	byte	kCmdExtruder6StepRateStep

	word	@@@hDictExtruder6StepRateStep
	hDictExtruder6StepRateStep	byte "extruder-6-step-rate-step"
	byte	kCmdExtruder7StepRateStep

	word	@@@hDictExtruder7StepRateStep
	hDictExtruder7StepRateStep	byte "extruder-7-step-rate-step"
	byte	kCmdPlatformStop

	word	@@@hDictPlatformStop
	hDictPlatformStop	byte "platform-stop"
	byte	kCmdHorizTrackStop

	word	@@@hDictHorizTrackStop
	hDictHorizTrackStop	byte "horiz-track-stop"
	byte	kCmdLeftTrackStop

	word	@@@hDictLeftTrackStop
	hDictLeftTrackStop	byte "left-track-stop"
	byte	kCmdRightTrackStop

	word	@@@hDictRightTrackStop
	hDictRightTrackStop	byte "right-track-stop"
	byte	kCmdExtruder0Stop

	word	@@@hDictExtruder0Stop
	hDictExtruder0Stop	byte "extruder0-stop"
	byte	kCmdExtruder1Stop

	word	@@@hDictExtruder1Stop
	hDictExtruder1Stop	byte "extruder1-stop"
	byte	kCmdExtruder2Stop

	word	@@@hDictExtruder2Stop
	hDictExtruder2Stop	byte "extruder2-stop"
	byte	kCmdExtruder3Stop

	word	@@@hDictExtruder3Stop
	hDictExtruder3Stop	byte "extruder3-stop"
	byte	kCmdExtruder4Stop

	word	@@@hDictExtruder4Stop
	hDictExtruder4Stop	byte "extruder4-stop"
	byte	kCmdExtruder5Stop

	word	@@@hDictExtruder5Stop
	hDictExtruder5Stop	byte "extruder5-stop"
	byte	kCmdExtruder6Stop

	word	@@@hDictExtruder6Stop
	hDictExtruder6Stop	byte "extruder6-stop"
	byte	kCmdExtruder7Stop

	word	@@@hDictExtruder7Stop
	hDictExtruder7Stop	byte "extruder7-stop"
}

DAT '' Build/commit number information.
hBuildVersion	file "version.txt"
				byte 0       ' Null terminate the build number.
